-module(af_beam_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_string:init(),
    af_type_compiler:init(),
    af_type_beam:init().

%% --- BeamModule creation ---

beam_module_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"beam-module creates BeamModule from Atom", fun() ->
            C1 = eval("my_mod beam-module", af_interpreter:new_continuation()),
            [{'BeamModule', ModVal}] = C1#continuation.data_stack,
            ?assertEqual(my_mod, maps:get(name, ModVal)),
            ?assertEqual([], maps:get(functions, ModVal)),
            ?assertEqual([], maps:get(exports, ModVal))
        end} end,

        fun(_) -> {"beam-fun creates BeamFunction", fun() ->
            C1 = eval("my_mod beam-module double 1 int beam-fun", af_interpreter:new_continuation()),
            [{'BeamFunction', FunVal}] = C1#continuation.data_stack,
            ?assertEqual(double, maps:get(name, FunVal)),
            ?assertEqual(1, maps:get(arity, FunVal))
        end} end,

        fun(_) -> {"beam-arg adds argument reference", fun() ->
            C1 = eval("my_mod beam-module f 1 int beam-fun 1 int beam-arg", af_interpreter:new_continuation()),
            [{'BeamFunction', FunVal}] = C1#continuation.data_stack,
            Exprs = maps:get(body_exprs, FunVal),
            ?assertEqual(1, length(Exprs)),
            ?assertMatch({var, 1, 'Arg1'}, hd(Exprs))
        end} end,

        fun(_) -> {"beam-int adds integer literal", fun() ->
            C1 = eval("my_mod beam-module f 1 int beam-fun 42 int beam-int", af_interpreter:new_continuation()),
            [{'BeamFunction', FunVal}] = C1#continuation.data_stack,
            Exprs = maps:get(body_exprs, FunVal),
            ?assertMatch([{integer, 1, 42}], Exprs)
        end} end
    ]}.

%% --- Compile and call ---

beam_compile_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile identity function", fun() ->
            C1 = eval("af_beam_test_id beam-module identity 1 int beam-fun 1 int beam-arg beam-return beam-compile", af_interpreter:new_continuation()),
            [{'Atom', "af_beam_test_id"}] = C1#continuation.data_stack,
            ?assertEqual(hello, af_beam_test_id:identity(hello)),
            ?assertEqual(42, af_beam_test_id:identity(42))
        end} end,

        fun(_) -> {"compile double function using beam-op", fun() ->
            C1 = eval("af_beam_test_dbl beam-module double 1 int beam-fun 1 int beam-arg 1 int beam-arg + 2 int beam-op beam-return beam-compile", af_interpreter:new_continuation()),
            [{'Atom', "af_beam_test_dbl"}] = C1#continuation.data_stack,
            ?assertEqual(10, af_beam_test_dbl:double(5)),
            ?assertEqual(0, af_beam_test_dbl:double(0))
        end} end,

        fun(_) -> {"compile function with remote call", fun() ->
            C1 = eval("af_beam_test_len beam-module len 1 int beam-fun 1 int beam-arg erlang length 1 int beam-call beam-return beam-compile", af_interpreter:new_continuation()),
            [{'Atom', "af_beam_test_len"}] = C1#continuation.data_stack,
            ?assertEqual(3, af_beam_test_len:len([a, b, c])),
            ?assertEqual(0, af_beam_test_len:len([]))
        end} end,

        fun(_) -> {"compile module with multiple functions", fun() ->
            C1 = eval("af_beam_test_multi beam-module", af_interpreter:new_continuation()),
            C2 = eval("inc 1 int beam-fun 1 int beam-arg 1 int beam-int + 2 int beam-op beam-return", C1),
            C3 = eval("dec 1 int beam-fun 1 int beam-arg 1 int beam-int - 2 int beam-op beam-return", C2),
            C4 = eval("beam-compile", C3),
            [{'Atom', "af_beam_test_multi"}] = C4#continuation.data_stack,
            ?assertEqual(6, af_beam_test_multi:inc(5)),
            ?assertEqual(4, af_beam_test_multi:dec(5))
        end} end,

        fun(_) -> {"compile atom-returning function", fun() ->
            C1 = eval("af_beam_test_atom beam-module greet 0 int beam-fun hello beam-atom beam-return beam-compile", af_interpreter:new_continuation()),
            [{'Atom', "af_beam_test_atom"}] = C1#continuation.data_stack,
            ?assertEqual(hello, af_beam_test_atom:greet())
        end} end,

        fun(_) -> {"compile add function (2 args)", fun() ->
            C1 = eval("af_beam_test_add beam-module add 2 int beam-fun 1 int beam-arg 2 int beam-arg + 2 int beam-op beam-return beam-compile", af_interpreter:new_continuation()),
            [{'Atom', "af_beam_test_add"}] = C1#continuation.data_stack,
            ?assertEqual(15, af_beam_test_add:add(7, 8)),
            ?assertEqual(0, af_beam_test_add:add(5, -5))
        end} end
    ]}.

%% --- compile-to-beam with transparent wrapper ---

compile_to_beam_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"define word then compile equivalent to BEAM", fun() ->
            C1 = eval(": square Int -> Int ; dup * .", af_interpreter:new_continuation()),
            C2 = eval("7 int square", C1),
            [{'Int', 49}] = C2#continuation.data_stack,
            C3 = eval("af_beam_test_sq beam-module square 1 int beam-fun 1 int beam-arg 1 int beam-arg * 2 int beam-op beam-return beam-compile", C1),
            [{'Atom', "af_beam_test_sq"}] = C3#continuation.data_stack,
            ?assertEqual(49, af_beam_test_sq:square(7)),
            ?assertEqual(100, af_beam_test_sq:square(10))
        end} end,

        fun(_) -> {"compile-to-beam compiles and replaces with native wrapper", fun() ->
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            %% Verify interpreted version works
            C2 = eval("5 int double", C1),
            [{'Int', 10}] = C2#continuation.data_stack,
            %% Compile to native BEAM
            C3 = eval("double af_ctb_wrap compile-to-beam", C1),
            [{'Atom', "af_ctb_wrap"}] = C3#continuation.data_stack,
            %% The word should still work through ActorForth — now via native wrapper
            C4 = eval("7 int double", C3),
            [{'Int', 14}, {'Atom', "af_ctb_wrap"}] = C4#continuation.data_stack,
            %% Native function also callable directly
            ?assertEqual(10, af_ctb_wrap:double(5))
        end} end,

        fun(_) -> {"compile-to-beam with arithmetic word", fun() ->
            C1 = eval(": inc Int -> Int ; 1 + .", af_interpreter:new_continuation()),
            C2 = eval("inc af_ctb_inc compile-to-beam", C1),
            [{'Atom', "af_ctb_inc"}] = C2#continuation.data_stack,
            ?assertEqual(6, af_ctb_inc:inc(5)),
            %% Use through ActorForth
            C3 = eval("10 int inc", C2),
            [{'Int', 11}, {'Atom', "af_ctb_inc"}] = C3#continuation.data_stack
        end} end
    ]}.

%% --- compile-all ---

compile_all_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile-all compiles all user words into one module", fun() ->
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval(": square Int -> Int ; dup * .", C1),
            C3 = eval("af_ctb_all compile-all", C2),
            [{'Atom', "af_ctb_all"}] = C3#continuation.data_stack,
            %% Both functions exist in the module
            ?assertEqual(10, af_ctb_all:double(5)),
            ?assertEqual(49, af_ctb_all:square(7)),
            %% Both work through ActorForth via wrappers
            C4 = eval("3 int double", C3),
            [{'Int', 6}, {'Atom', "af_ctb_all"}] = C4#continuation.data_stack,
            C5 = eval("4 int square", C3),
            [{'Int', 16}, {'Atom', "af_ctb_all"}] = C5#continuation.data_stack
        end} end
    ]}.

%% --- save-module ---

save_module_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"save-module writes .beam file to disk", fun() ->
            C1 = eval(": triple Int -> Int ; dup dup + + .", af_interpreter:new_continuation()),
            C2 = eval("triple af_save_test compile-to-beam", C1),
            %% Save to a temp directory
            TmpDir = "/tmp/af_beam_test_" ++ integer_to_list(erlang:unique_integer([positive])),
            %% Stack: [Atom("af_save_test")]
            %% save-module expects [String(dir), Atom(mod)]
            C3 = eval("\"" ++ TmpDir ++ "\" save-module", C2),
            [] = C3#continuation.data_stack,
            %% Verify file exists
            BeamFile = filename:join(TmpDir, "af_save_test.beam"),
            ?assert(filelib:is_regular(BeamFile)),
            %% Verify it's a valid beam file
            {ok, Binary} = file:read_file(BeamFile),
            ?assert(byte_size(Binary) > 0),
            %% Cleanup
            file:delete(BeamFile),
            file:del_dir(TmpDir)
        end} end
    ]}.

%% --- build-escript ---

build_escript_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"build-escript creates executable file", fun() ->
            %% Compile a word with args (so name doesn't auto-execute)
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval("double af_escript_test compile-to-beam", C1),
            [{'Atom', "af_escript_test"}] = C2#continuation.data_stack,
            TmpFile = "/tmp/af_escript_test_" ++ integer_to_list(erlang:unique_integer([positive])),
            %% Build escript via exported Erlang API (entry=double, but main wraps it)
            ok = af_type_beam:build_escript(af_escript_test, double, TmpFile),
            %% Verify file exists and is executable
            ?assert(filelib:is_regular(TmpFile)),
            {ok, #file_info{mode = Mode}} = file:read_file_info(TmpFile),
            ?assert((Mode band 8#111) > 0),
            file:delete(TmpFile)
        end} end,

        fun(_) -> {"build-escript runs and exits cleanly", fun() ->
            %% Compile a zero-arg word directly via Erlang API
            WordDef = {"answer", [], ['Int'], [#operation{name = "42"}]},
            {ok, af_escript_run} = af_word_compiler:compile_words_to_module(af_escript_run, [WordDef]),
            ?assertEqual(42, af_escript_run:answer()),
            TmpFile = "/tmp/af_escript_run_" ++ integer_to_list(erlang:unique_integer([positive])),
            ok = af_type_beam:build_escript(af_escript_run, answer, TmpFile),
            %% Run it — main calls answer() then halt(0)
            Result = os:cmd(TmpFile ++ " 2>&1; echo $?"),
            Lines = string:split(string:trim(Result), "\n", all),
            LastLine = lists:last(Lines),
            ?assertEqual("0", LastLine),
            file:delete(TmpFile)
        end} end,

        fun(_) -> {"build-escript word works through ActorForth", fun() ->
            %% Test the ActorForth word with a function that takes args
            C1 = eval(": triple Int -> Int ; dup dup + + .", af_interpreter:new_continuation()),
            C2 = eval("triple af_esc_word compile-to-beam", C1),
            [{'Atom', "af_esc_word"}] = C2#continuation.data_stack,
            TmpFile = "/tmp/af_esc_word_" ++ integer_to_list(erlang:unique_integer([positive])),
            %% Use the ActorForth build-escript word
            C3 = eval("triple \"" ++ TmpFile ++ "\" build-escript", C2),
            [] = C3#continuation.data_stack,
            ?assert(filelib:is_regular(TmpFile)),
            file:delete(TmpFile)
        end} end
    ]}.

%% --- build-release ---

build_release_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"build-release creates OTP app structure", fun() ->
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            C2 = eval("double af_rel_test compile-to-beam", C1),
            TmpDir = "/tmp/af_rel_test_" ++ integer_to_list(erlang:unique_integer([positive])),
            C3 = eval("af_rel_test \"1.0.0\" \"" ++ TmpDir ++ "\" build-release", C2),
            [{'Atom', "af_rel_test"}] = C3#continuation.data_stack,
            %% Verify structure
            EbinDir = filename:join([TmpDir, "af_rel_test-1.0.0", "ebin"]),
            AppFile = filename:join(EbinDir, "af_rel_test.app"),
            BeamFile = filename:join(EbinDir, "af_rel_test.beam"),
            ?assert(filelib:is_regular(AppFile)),
            ?assert(filelib:is_regular(BeamFile)),
            %% Verify .app contents
            {ok, [AppSpec]} = file:consult(AppFile),
            {application, af_rel_test, Props} = AppSpec,
            ?assertEqual("1.0.0", proplists:get_value(vsn, Props)),
            ?assertEqual([af_rel_test], proplists:get_value(modules, Props)),
            %% Cleanup
            file:delete(BeamFile),
            file:delete(AppFile),
            file:del_dir(EbinDir),
            file:del_dir(filename:join(TmpDir, "af_rel_test-1.0.0")),
            file:del_dir(TmpDir)
        end} end
    ]}.
