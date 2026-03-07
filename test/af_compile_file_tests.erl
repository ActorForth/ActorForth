-module(af_compile_file_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    af_type:reset(),
    af_repl:init_types().

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
