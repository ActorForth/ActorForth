-module(af_beam_tests).

-include_lib("eunit/include/eunit.hrl").
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
            %% Build: identity(Arg1) -> Arg1
            C1 = eval("af_beam_test_id beam-module identity 1 int beam-fun 1 int beam-arg beam-return beam-compile", af_interpreter:new_continuation()),
            [{'Atom', "af_beam_test_id"}] = C1#continuation.data_stack,
            %% Call the compiled function
            ?assertEqual(hello, af_beam_test_id:identity(hello)),
            ?assertEqual(42, af_beam_test_id:identity(42))
        end} end,

        fun(_) -> {"compile double function using beam-op", fun() ->
            %% Build: double(Arg1) -> Arg1 + Arg1
            C1 = eval("af_beam_test_dbl beam-module double 1 int beam-fun 1 int beam-arg 1 int beam-arg + 2 int beam-op beam-return beam-compile", af_interpreter:new_continuation()),
            [{'Atom', "af_beam_test_dbl"}] = C1#continuation.data_stack,
            ?assertEqual(10, af_beam_test_dbl:double(5)),
            ?assertEqual(0, af_beam_test_dbl:double(0))
        end} end,

        fun(_) -> {"compile function with remote call", fun() ->
            %% Build: len(Arg1) -> erlang:length(Arg1)
            C1 = eval("af_beam_test_len beam-module len 1 int beam-fun 1 int beam-arg erlang length 1 int beam-call beam-return beam-compile", af_interpreter:new_continuation()),
            [{'Atom', "af_beam_test_len"}] = C1#continuation.data_stack,
            ?assertEqual(3, af_beam_test_len:len([a, b, c])),
            ?assertEqual(0, af_beam_test_len:len([]))
        end} end,

        fun(_) -> {"compile module with multiple functions", fun() ->
            C1 = eval("af_beam_test_multi beam-module", af_interpreter:new_continuation()),
            %% Function 1: inc(X) -> X + 1
            C2 = eval("inc 1 int beam-fun 1 int beam-arg 1 int beam-int + 2 int beam-op beam-return", C1),
            %% Function 2: dec(X) -> X - 1
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

%% --- ActorForth word definition compiled to BEAM ---

af_word_to_beam_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"define ActorForth word then compile equivalent to BEAM", fun() ->
            %% Define square in ActorForth
            C1 = eval(": square Int -> Int ; dup * .", af_interpreter:new_continuation()),
            C2 = eval("7 int square", C1),
            [{'Int', 49}] = C2#continuation.data_stack,
            %% Now build the same function as BEAM
            C3 = eval("af_beam_test_sq beam-module square 1 int beam-fun 1 int beam-arg 1 int beam-arg * 2 int beam-op beam-return beam-compile", C1),
            [{'Atom', "af_beam_test_sq"}] = C3#continuation.data_stack,
            %% Both should produce the same result
            ?assertEqual(49, af_beam_test_sq:square(7)),
            ?assertEqual(100, af_beam_test_sq:square(10))
        end} end,

        fun(_) -> {"compile-to-beam compiles defined word to native BEAM", fun() ->
            %% Define double in ActorForth
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            [] = C1#continuation.data_stack,
            %% Compile it to BEAM using compile-to-beam
            C2 = eval("double af_beam_test_ctb compile-to-beam", C1),
            [{'Atom', "af_beam_test_ctb"}] = C2#continuation.data_stack,
            %% Call the native BEAM function
            ?assertEqual(10, af_beam_test_ctb:double(5)),
            ?assertEqual(0, af_beam_test_ctb:double(0)),
            ?assertEqual(-6, af_beam_test_ctb:double(-3))
        end} end,

        fun(_) -> {"compile-to-beam with arithmetic word", fun() ->
            %% Define inc: adds 1
            C1 = eval(": inc Int -> Int ; 1 + .", af_interpreter:new_continuation()),
            C2 = eval("inc af_beam_test_ctb2 compile-to-beam", C1),
            [{'Atom', "af_beam_test_ctb2"}] = C2#continuation.data_stack,
            ?assertEqual(6, af_beam_test_ctb2:inc(5)),
            ?assertEqual(0, af_beam_test_ctb2:inc(-1))
        end} end
    ]}.
