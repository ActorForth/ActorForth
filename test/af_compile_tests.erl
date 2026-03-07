-module(af_compile_tests).

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
    af_type_compiler:init().

%% --- compile_word: closure-based compilation ---

compile_word_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile_word creates operation with compiled source", fun() ->
            %% Define a word via the interpreter to get body ops
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            ?assertEqual([], C1#continuation.data_stack),
            %% Find the defined word's ops
            {ok, #operation{impl = _Impl, name = "double"} = Op} =
                af_type:find_op_by_name("double", 'Int'),
            %% Compile it
            Compiled = af_compile:compile_word("double", Op#operation.sig_in, Op#operation.sig_out, [Op]),
            ?assertEqual("double", Compiled#operation.name),
            ?assertEqual(compiled, Compiled#operation.source)
        end} end,

        fun(_) -> {"compiled word produces correct results", fun() ->
            %% Define double, then compile it manually
            C1 = eval(": double Int -> Int ; dup + .", af_interpreter:new_continuation()),
            %% Use the interpreted version to verify
            C2 = eval("5 int double", C1),
            [{'Int', 10}] = C2#continuation.data_stack,
            %% Now test the compiled dup and + directly
            DupOp = #operation{name = "dup", impl = fun(Cont) ->
                [H | _] = Cont#continuation.data_stack,
                Cont#continuation{data_stack = [H | Cont#continuation.data_stack]}
            end},
            PlusOp = #operation{name = "+", impl = fun(Cont) ->
                [{'Int', A}, {'Int', B} | R] = Cont#continuation.data_stack,
                Cont#continuation{data_stack = [{'Int', B + A} | R]}
            end},
            Compiled = af_compile:compile_word("double", ['Int'], ['Int'], [DupOp, PlusOp]),
            Cont = #continuation{data_stack = [{'Int', 7}]},
            Result = (Compiled#operation.impl)(Cont),
            ?assertEqual([{'Int', 14}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"compiled word optimizes known primitives", fun() ->
            %% dup, drop, swap, +, -, * are optimized
            DupOp = #operation{name = "dup", impl = fun(_) -> error(should_not_call) end},
            Compiled = af_compile:compile_word("test", [], [], [DupOp]),
            Cont = #continuation{data_stack = [{'Int', 5}]},
            Result = (Compiled#operation.impl)(Cont),
            ?assertEqual([{'Int', 5}, {'Int', 5}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"compiled arithmetic chain", fun() ->
            %% Compile: dup + (should give 10 from 5)
            Ops = [
                #operation{name = "dup", impl = fun(_) -> error(unused) end},
                #operation{name = "+", impl = fun(_) -> error(unused) end}
            ],
            Compiled = af_compile:compile_word("double", ['Int'], ['Int'], Ops),
            Cont = #continuation{data_stack = [{'Int', 5}]},
            Result = (Compiled#operation.impl)(Cont),
            ?assertEqual([{'Int', 10}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"compiled swap and subtract", fun() ->
            Ops = [
                #operation{name = "swap", impl = fun(_) -> error(unused) end},
                #operation{name = "-", impl = fun(_) -> error(unused) end}
            ],
            Compiled = af_compile:compile_word("rev-sub", ['Int', 'Int'], ['Int'], Ops),
            Cont = #continuation{data_stack = [{'Int', 3}, {'Int', 10}]},
            Result = (Compiled#operation.impl)(Cont),
            %% swap gives [10, 3], then - gives 3 - 10 = -7
            ?assertEqual([{'Int', -7}], Result#continuation.data_stack)
        end} end
    ]}.

%% --- compile_module: BEAM module generation ---

compile_module_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile_module generates loadable BEAM module", fun() ->
            %% Define a simple word body
            DupOp = #operation{name = "dup", impl = fun(Cont) ->
                [H | _] = Cont#continuation.data_stack,
                Cont#continuation{data_stack = [H | Cont#continuation.data_stack]}
            end},
            PlusOp = #operation{name = "+", impl = fun(Cont) ->
                [{'Int', A}, {'Int', B} | R] = Cont#continuation.data_stack,
                Cont#continuation{data_stack = [{'Int', B + A} | R]}
            end},
            WordDefs = [{"double", ['Int'], ['Int'], [DupOp, PlusOp]}],
            {ok, af_test_mod1} = af_compile:compile_module(af_test_mod1, WordDefs),
            %% The compiled module should be callable
            Result = af_test_mod1:double([{'Int', 5}]),
            ?assertEqual([{'Int', 10}], Result)
        end} end,

        fun(_) -> {"compile_module with multiple words", fun() ->
            DupOp = #operation{name = "dup", impl = fun(_) -> error(unused) end},
            PlusOp = #operation{name = "+", impl = fun(_) -> error(unused) end},
            MulOp = #operation{name = "*", impl = fun(_) -> error(unused) end},
            WordDefs = [
                {"double", ['Int'], ['Int'], [DupOp, PlusOp]},
                {"square", ['Int'], ['Int'], [DupOp, MulOp]}
            ],
            {ok, af_test_mod2} = af_compile:compile_module(af_test_mod2, WordDefs),
            ?assertEqual([{'Int', 10}], af_test_mod2:double([{'Int', 5}])),
            ?assertEqual([{'Int', 25}], af_test_mod2:square([{'Int', 5}]))
        end} end,

        fun(_) -> {"apply_impl dispatches through type system", fun() ->
            %% apply_impl is used by generated BEAM code
            Stack = [{'Int', 3}, {'Int', 7}],
            Result = af_compile:apply_impl("+", Stack),
            ?assertEqual([{'Int', 10}], Result)
        end} end,

        fun(_) -> {"apply_impl pushes Atom for unknown word", fun() ->
            Stack = [{'Int', 5}],
            Result = af_compile:apply_impl("unknown_word", Stack),
            ?assertEqual([{'Atom', "unknown_word"}, {'Int', 5}], Result)
        end} end
    ]}.
