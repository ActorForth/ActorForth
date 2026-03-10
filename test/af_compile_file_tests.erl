-module(af_compile_file_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    af_type:reset().

compile_file_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile .a4 file to module", fun() ->
            File = "test/test_lib.a4",
            {ok, test_lib} = af_compile_file:compile(File),
            ?assertEqual([{'Int', 10}], test_lib:double([{'Int', 5}])),
            ?assertEqual([{'Int', 49}], test_lib:square([{'Int', 7}])),
            ?assertEqual([{'Int', 6}], test_lib:inc([{'Int', 5}]))
        end} end,

        fun(_) -> {"compile .a4 file with custom module name", fun() ->
            File = "test/test_lib.a4",
            {ok, af_test_math} = af_compile_file:compile(File, af_test_math),
            ?assertEqual([{'Int', 10}], af_test_math:double([{'Int', 5}])),
            ?assertEqual([{'Int', 49}], af_test_math:square([{'Int', 7}]))
        end} end,

        fun(_) -> {"compile .a4 file to directory", fun() ->
            File = "test/test_lib.a4",
            TmpDir = "/tmp/af_compile_test_" ++ integer_to_list(erlang:unique_integer([positive])),
            {ok, test_lib} = af_compile_file:compile_to_dir(File, TmpDir),
            BeamFile = filename:join(TmpDir, "test_lib.beam"),
            ?assert(filelib:is_regular(BeamFile)),
            %% Clean up
            file:delete(BeamFile),
            file:del_dir(TmpDir)
        end} end,

        fun(_) -> {"compile nonexistent file returns error", fun() ->
            {error, {read_error, _, enoent}} = af_compile_file:compile("nonexistent.a4")
        end} end,

        fun(_) -> {"compile_to_dir nonexistent file returns error", fun() ->
            Result = af_compile_file:compile_to_dir("nonexistent.a4", "/tmp/af_test"),
            ?assertMatch({error, {read_error, _, enoent}}, Result)
        end} end,

        fun(_) -> {"compile file with no word definitions returns error", fun() ->
            %% Create a temp .a4 file with no word definitions (just pushes atoms)
            TmpFile = "/tmp/af_no_words_test.a4",
            ok = file:write_file(TmpFile, "hello world"),
            Result = af_compile_file:compile(TmpFile),
            ?assertMatch({error, {no_compilable_words, _}}, Result),
            file:delete(TmpFile)
        end} end,

        fun(_) -> {"compile_to_dir file with no word definitions returns error", fun() ->
            TmpFile = "/tmp/af_no_words_test2.a4",
            ok = file:write_file(TmpFile, "hello world"),
            Result = af_compile_file:compile_to_dir(TmpFile, "/tmp/af_test_out"),
            ?assertMatch({error, {no_compilable_words, _}}, Result),
            file:delete(TmpFile)
        end} end
    ]}.

%%% === Self-Hosted Compilation Tests ===

compile_selfhosted_basic_test() ->
    %% compile_selfhosted/1 uses lib_math.a4 which has square, double, cube
    {ok, Mod} = af_compile_file:compile_selfhosted("samples/lib_math.a4"),
    ?assertEqual([{'Int', 25}], Mod:square([{'Int', 5}])),
    ?assertEqual([{'Int', 14}], Mod:double([{'Int', 7}])),
    ?assertEqual([{'Int', 27}], Mod:cube([{'Int', 3}])).

compile_selfhosted_with_name_test() ->
    {ok, Mod} = af_compile_file:compile_selfhosted("samples/lib_math.a4", "math_sh_test"),
    ?assert(is_atom(Mod)),
    %% Module name should be af_r2_ prefixed
    ?assertEqual(af_r2_math_sh_test, Mod).

compile_to_dir_selfhosted_test() ->
    OutDir = "/tmp/af_test_beam_" ++ integer_to_list(erlang:unique_integer([positive])),
    {ok, Mod} = af_compile_file:compile_to_dir_selfhosted("samples/lib_math.a4", OutDir),
    ?assert(is_atom(Mod)),
    %% Verify beam file was created
    BeamFile = filename:join(OutDir, atom_to_list(Mod) ++ ".beam"),
    ?assert(filelib:is_file(BeamFile)),
    %% Clean up
    file:delete(BeamFile),
    file:del_dir(OutDir).

compile_selfhosted_nonexistent_test() ->
    Result = af_compile_file:compile_selfhosted("nonexistent_file.a4"),
    ?assertMatch({error, _}, Result).

compile_to_dir_selfhosted_nonexistent_test() ->
    Result = af_compile_file:compile_to_dir_selfhosted("nonexistent_file.a4", "/tmp/af_test"),
    ?assertMatch({error, _}, Result).

compile_selfhosted_no_words_test() ->
    %% A file with only comments and bare expressions, no word definitions
    TmpFile = "/tmp/af_sh_no_words_test.a4",
    ok = file:write_file(TmpFile, <<"# just a comment\n1 2 3">>),
    Result = af_compile_file:compile_selfhosted(TmpFile),
    ?assertMatch({error, _}, Result),
    file:delete(TmpFile).

compile_to_dir_selfhosted_no_words_test() ->
    TmpFile = "/tmp/af_sh_no_words_test2.a4",
    ok = file:write_file(TmpFile, <<"# no words here">>),
    Result = af_compile_file:compile_to_dir_selfhosted(TmpFile, "/tmp/af_test_out"),
    ?assertMatch({error, _}, Result),
    file:delete(TmpFile).

%%% === Self-Hosted with Pattern Matching (retranslate_bodies) ===

compile_selfhosted_pattern_matching_test() ->
    %% Compile fib.a4 which uses sub-clauses (pattern matching)
    {ok, Mod} = af_compile_file:compile_selfhosted("samples/fib.a4"),
    ?assertEqual([{'Int', 0}], Mod:fib([{'Int', 0}])),
    ?assertEqual([{'Int', 1}], Mod:fib([{'Int', 1}])),
    ?assertEqual([{'Int', 8}], Mod:fib([{'Int', 6}])),
    ?assertEqual([{'Int', 55}], Mod:fib([{'Int', 10}])).

compile_selfhosted_multi_word_calls_test() ->
    %% Test inter-word calls: retranslate_bodies converts apply_impl -> call
    %% Create a temp file where one word calls another
    TmpFile = "/tmp/af_sh_interword_test.a4",
    ok = file:write_file(TmpFile, <<": helper Int -> Int ; dup + .\n: main Int -> Int ; helper 1 + .">>),
    {ok, Mod} = af_compile_file:compile_selfhosted(TmpFile),
    %% main(5) should call helper(5) = 10, then 10 + 1 = 11
    ?assertEqual([{'Int', 11}], Mod:main([{'Int', 5}])),
    file:delete(TmpFile).

compile_to_dir_selfhosted_retranslate_test() ->
    %% Verify retranslate_bodies works in compile_to_dir_selfhosted path
    TmpFile = "/tmp/af_sh_retranslate_test.a4",
    ok = file:write_file(TmpFile, <<": sq Int -> Int ; dup * .\n: sq2 Int -> Int ; sq sq .">>),
    OutDir = "/tmp/af_sh_retranslate_out_" ++ integer_to_list(erlang:unique_integer([positive])),
    {ok, Mod} = af_compile_file:compile_to_dir_selfhosted(TmpFile, OutDir),
    %% Load the beam from directory for testing
    BeamFile = filename:join(OutDir, atom_to_list(Mod) ++ ".beam"),
    ?assert(filelib:is_file(BeamFile)),
    %% Clean up
    file:delete(BeamFile),
    file:del_dir(OutDir),
    file:delete(TmpFile).

%%% === Self-Hosted Product Type Compilation ===

compile_selfhosted_product_type_test() ->
    TmpFile = "/tmp/af_sh_product_test.a4",
    ok = file:write_file(TmpFile, <<"type Point\n  x Int\n  y Int\n.\n: mk-point Int Int -> Point ; point .">>),
    {ok, Mod} = af_compile_file:compile_selfhosted(TmpFile),
    ?assert(is_atom(Mod)),
    file:delete(TmpFile).

%%% === Coverage gap tests ===

%% Test compile_to_dir_selfhosted with sub-clauses (pattern matching).
%% Exercises retranslate_body's {lit, {'List', Clauses}} branch (lines 115-117).
compile_to_dir_selfhosted_subclauses_test() ->
    TmpFile = "/tmp/af_sh_subclauses_test.a4",
    ok = file:write_file(TmpFile,
        <<"# factorial with pattern matching\n"
          ": fact Int -> Int ;\n"
          "    : 0 -> 1 ;\n"
          "    : Int -> Int ;\n"
          "        dup 1 - fact * .\n">>),
    OutDir = "/tmp/af_sh_subclauses_out_" ++ integer_to_list(erlang:unique_integer([positive])),
    {ok, Mod} = af_compile_file:compile_to_dir_selfhosted(TmpFile, OutDir),
    ?assert(is_atom(Mod)),
    BeamFile = filename:join(OutDir, atom_to_list(Mod) ++ ".beam"),
    ?assert(filelib:is_file(BeamFile)),
    file:delete(BeamFile),
    file:del_dir(OutDir),
    file:delete(TmpFile).

%% Test retranslate_body's false branch for apply_impl (line 112).
%% Uses a word that references an external (non-local) name via apply_impl.
%% The r0 compiler emits {apply_impl, "unknown-op"} for unrecognized tokens;
%% retranslate_bodies should leave it as apply_impl (not convert to call).
compile_to_dir_selfhosted_external_apply_impl_test() ->
    TmpFile = "/tmp/af_sh_ext_apply_test.a4",
    ok = file:write_file(TmpFile,
        <<"# inter-word call + external op reference\n"
          ": helper Int -> Int ; dup + .\n"
          ": foo Int -> Int ; helper external-thing .\n">>),
    OutDir = "/tmp/af_sh_ext_apply_out_" ++ integer_to_list(erlang:unique_integer([positive])),
    {ok, Mod} = af_compile_file:compile_to_dir_selfhosted(TmpFile, OutDir),
    ?assert(is_atom(Mod)),
    BeamFile = filename:join(OutDir, atom_to_list(Mod) ++ ".beam"),
    ?assert(filelib:is_file(BeamFile)),
    file:delete(BeamFile),
    file:del_dir(OutDir),
    file:delete(TmpFile).

%% Test ensure_types() when type registry is not initialized.
%% Clears the registry before calling compile/1.
compile_with_no_registry_test() ->
    %% Erase the registry to trigger ensure_types initialization path
    catch ets:delete(af_type_registry),
    {ok, lib_math} = af_compile_file:compile("samples/lib_math.a4"),
    ?assertEqual([{'Int', 25}], lib_math:square([{'Int', 5}])),
    ok.

%% Test compile_to_dir_selfhosted read error path (line 100).
compile_to_dir_selfhosted_read_error_test() ->
    Result = af_compile_file:compile_to_dir_selfhosted("nonexistent_file.a4", "/tmp/af_test"),
    ?assertMatch({error, {read_error, _, enoent}}, Result).
