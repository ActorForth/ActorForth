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

%% --- Milestone 8: Multi-clause compilation and compile word ---

multi_clause_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"multi-clause factorial compiles to native BEAM", fun() ->
            C1 = eval(": factorial 0 Int -> Int ; drop 1 .",
                       af_interpreter:new_continuation()),
            C2 = eval(": factorial Int -> Int ; dup 1 - factorial * .", C1),
            %% Verify interpreted version works first
            C3 = eval("5 int factorial", C2),
            [{'Int', 120}] = C3#continuation.data_stack,
            %% Now compile to native
            C4 = eval("\"factorial\" compile", C2),
            ?assertEqual([], C4#continuation.data_stack),
            %% Run the native version
            C5 = eval("10 int factorial", C4),
            [{'Int', 3628800}] = C5#continuation.data_stack
        end} end,
        fun(_) -> {"compile word replaces interpreted with native", fun() ->
            C1 = eval(": double Int -> Int ; dup + .",
                       af_interpreter:new_continuation()),
            C2 = eval("\"double\" compile", C1),
            C3 = eval("21 int double", C2),
            [{'Int', 42}] = C3#continuation.data_stack,
            %% Verify it's actually native
            {ok, Op} = af_type:find_op_by_name("double", 'Int'),
            ?assertMatch({native, _}, Op#operation.source)
        end} end,
        fun(_) -> {"compile with pattern matching: identity base case", fun() ->
            C1 = eval(": myid 0 Int -> Int ; drop 0 .",
                       af_interpreter:new_continuation()),
            C2 = eval(": myid Int -> Int ; dup + .", C1),
            %% Compile
            C3 = eval("\"myid\" compile", C2),
            %% Base case: 0 returns 0
            C4 = eval("0 int myid", C3),
            [{'Int', 0}] = C4#continuation.data_stack,
            %% General case: 5 returns 10 (dup +)
            C5 = eval("5 int myid", C3),
            [{'Int', 10}] = C5#continuation.data_stack
        end} end,
        fun(_) -> {"compile error for nonexistent word", fun() ->
            ?assertError(_,
                eval("\"nonexistent\" compile", af_interpreter:new_continuation()))
        end} end,
        fun(_) -> {"multi-clause grouping preserves clause order", fun() ->
            %% Value-constrained clause first (more specific), general clause second
            WordDefs = [
                {"fib", [{'Int', 0}], ['Int'], [#operation{name = "drop"}, #operation{name = "0"}]},
                {"fib", [{'Int', 1}], ['Int'], [#operation{name = "drop"}, #operation{name = "1"}]},
                {"fib", ['Int'], ['Int'], [#operation{name = "dup"}, #operation{name = "1"},
                    #operation{name = "-"}, #operation{name = "fib"},
                    #operation{name = "swap"}, #operation{name = "2"},
                    #operation{name = "-"}, #operation{name = "fib"},
                    #operation{name = "+"}]}
            ],
            Groups = af_word_compiler:group_by_name(WordDefs),
            [{_, Defs}] = Groups,
            ?assertEqual(3, length(Defs))
        end} end
    ]}.

%% --- Additional coverage tests ---

additional_coverage_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile_module single-op body (no chain)", fun() ->
            %% Single-op body exercises the base case of build_body_chain (line 102-103)
            DupOp = #operation{name = "dup", impl = fun(Cont) ->
                [H | _] = Cont#continuation.data_stack,
                Cont#continuation{data_stack = [H | Cont#continuation.data_stack]}
            end},
            WordDefs = [{"mydup", ['Int'], ['Int', 'Int'], [DupOp]}],
            {ok, af_test_single_op} = af_compile:compile_module(af_test_single_op, WordDefs),
            ?assertEqual([{'Int', 5}, {'Int', 5}], af_test_single_op:mydup([{'Int', 5}]))
        end} end,

        fun(_) -> {"compile_module three-op body chain", fun() ->
            %% Three ops exercises the block/match path multiple times (lines 104-108)
            Ops = [
                #operation{name = "dup", impl = fun(Cont) ->
                    [H | _] = Cont#continuation.data_stack,
                    Cont#continuation{data_stack = [H | Cont#continuation.data_stack]}
                end},
                #operation{name = "+", impl = fun(Cont) ->
                    [{'Int', A}, {'Int', B} | R] = Cont#continuation.data_stack,
                    Cont#continuation{data_stack = [{'Int', B + A} | R]}
                end},
                #operation{name = "dup", impl = fun(Cont) ->
                    [H | _] = Cont#continuation.data_stack,
                    Cont#continuation{data_stack = [H | Cont#continuation.data_stack]}
                end}
            ],
            WordDefs = [{"dbl_dup", ['Int'], ['Int', 'Int'], Ops}],
            {ok, af_test_3op} = af_compile:compile_module(af_test_3op, WordDefs),
            ?assertEqual([{'Int', 10}, {'Int', 10}], af_test_3op:dbl_dup([{'Int', 5}]))
        end} end,

        fun(_) -> {"compile_module empty body (identity)", fun() ->
            %% Empty body: build_body_chain returns CurrentVar directly (line 96-97)
            WordDefs = [{"noop", ['Int'], ['Int'], []}],
            {ok, af_test_empty_body} = af_compile:compile_module(af_test_empty_body, WordDefs),
            ?assertEqual([{'Int', 42}], af_test_empty_body:noop([{'Int', 42}]))
        end} end,

        fun(_) -> {"apply_impl with dup operation", fun() ->
            %% Tests apply_impl with stack operations
            Stack = [{'Int', 5}],
            Result = af_compile:apply_impl("dup", Stack),
            ?assertEqual([{'Int', 5}, {'Int', 5}], Result)
        end} end,

        fun(_) -> {"apply_impl with swap operation", fun() ->
            Stack = [{'Int', 3}, {'Int', 7}],
            Result = af_compile:apply_impl("swap", Stack),
            ?assertEqual([{'Int', 7}, {'Int', 3}], Result)
        end} end,

        fun(_) -> {"apply_impl with drop operation", fun() ->
            Stack = [{'Int', 3}, {'Int', 7}],
            Result = af_compile:apply_impl("drop", Stack),
            ?assertEqual([{'Int', 7}], Result)
        end} end,

        fun(_) -> {"apply_impl with subtract operation", fun() ->
            Stack = [{'Int', 3}, {'Int', 10}],
            Result = af_compile:apply_impl("-", Stack),
            ?assertEqual([{'Int', 7}], Result)
        end} end,

        fun(_) -> {"apply_impl with multiply operation", fun() ->
            Stack = [{'Int', 3}, {'Int', 7}],
            Result = af_compile:apply_impl("*", Stack),
            ?assertEqual([{'Int', 21}], Result)
        end} end,

        fun(_) -> {"compile_word with rot optimization", fun() ->
            RotOp = #operation{name = "rot", impl = fun(_) -> error(unused) end},
            Compiled = af_compile:compile_word("myrot", ['Int', 'Int', 'Int'], ['Int', 'Int', 'Int'], [RotOp]),
            Cont = #continuation{data_stack = [{'Int', 1}, {'Int', 2}, {'Int', 3}]},
            Result = (Compiled#operation.impl)(Cont),
            ?assertEqual([{'Int', 3}, {'Int', 1}, {'Int', 2}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"compile_word with over optimization", fun() ->
            OverOp = #operation{name = "over", impl = fun(_) -> error(unused) end},
            Compiled = af_compile:compile_word("myover", ['Int', 'Int'], ['Int', 'Int', 'Int'], [OverOp]),
            Cont = #continuation{data_stack = [{'Int', 1}, {'Int', 2}]},
            Result = (Compiled#operation.impl)(Cont),
            ?assertEqual([{'Int', 2}, {'Int', 1}, {'Int', 2}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"compile_word with drop optimization", fun() ->
            DropOp = #operation{name = "drop", impl = fun(_) -> error(unused) end},
            Compiled = af_compile:compile_word("mydrop", ['Int'], [], [DropOp]),
            Cont = #continuation{data_stack = [{'Int', 5}, {'Int', 10}]},
            Result = (Compiled#operation.impl)(Cont),
            ?assertEqual([{'Int', 10}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"compile_word with general case (non-optimized op)", fun() ->
            %% Use an op name that doesn't match any optimized case
            CustomImpl = fun(Cont) ->
                [{'Int', V} | R] = Cont#continuation.data_stack,
                Cont#continuation{data_stack = [{'Int', V * 10} | R]}
            end,
            CustomOp = #operation{name = "times10", impl = CustomImpl},
            Compiled = af_compile:compile_word("t10", ['Int'], ['Int'], [CustomOp]),
            Cont = #continuation{data_stack = [{'Int', 3}]},
            Result = (Compiled#operation.impl)(Cont),
            ?assertEqual([{'Int', 30}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"compile_word with multiply optimization", fun() ->
            MulOp = #operation{name = "*", impl = fun(_) -> error(unused) end},
            Compiled = af_compile:compile_word("sq", ['Int'], ['Int'],
                [#operation{name = "dup", impl = fun(_) -> error(unused) end}, MulOp]),
            Cont = #continuation{data_stack = [{'Int', 4}]},
            Result = (Compiled#operation.impl)(Cont),
            ?assertEqual([{'Int', 16}], Result#continuation.data_stack)
        end} end,

        fun(_) -> {"compile_word with subtract optimization", fun() ->
            SubOp = #operation{name = "-", impl = fun(_) -> error(unused) end},
            Compiled = af_compile:compile_word("dec", ['Int'], ['Int'],
                [#operation{name = "dup", impl = fun(_) -> error(unused) end}, SubOp]),
            Cont = #continuation{data_stack = [{'Int', 5}]},
            Result = (Compiled#operation.impl)(Cont),
            %% dup gives [5, 5], then - gives 5 - 5 = 0
            ?assertEqual([{'Int', 0}], Result#continuation.data_stack)
        end} end
    ]}.

%% --- auto-compile tests ---

auto_compile_setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_string:init().

auto_compile_test_() ->
    {foreach, fun auto_compile_setup/0, fun(_) ->
        persistent_term:erase(af_auto_compile)
    end, [
        fun(_) -> {"auto-compile off by default", fun() ->
            C1 = eval(": triple Int -> Int ; 3 * .", af_interpreter:new_continuation()),
            {ok, Op} = af_type:find_op_by_name("triple", 'Int'),
            ?assertMatch({compiled, _}, Op#operation.source)
        end} end,
        fun(_) -> {"auto-compile on compiles word to native on definition", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("True bool auto-compile", C0),
            C2 = eval(": triple Int -> Int ; 3 * .", C1),
            {ok, Op} = af_type:find_op_by_name("triple", 'Int'),
            ?assertMatch({native, _}, Op#operation.source),
            %% Verify it actually works
            C3 = eval("7 int triple", C2),
            [{'Int', 21}] = C3#continuation.data_stack
        end} end,
        fun(_) -> {"auto-compile toggle off stops auto-compiling", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("True bool auto-compile", C0),
            C2 = eval(": dbl Int -> Int ; 2 * .", C1),
            {ok, Op1} = af_type:find_op_by_name("dbl", 'Int'),
            ?assertMatch({native, _}, Op1#operation.source),
            %% Turn off
            C3 = eval("False bool auto-compile", C2),
            C4 = eval(": trpl Int -> Int ; 3 * .", C3),
            {ok, Op2} = af_type:find_op_by_name("trpl", 'Int'),
            ?assertMatch({compiled, _}, Op2#operation.source)
        end} end,
        fun(_) -> {"auto-compile works with multi-clause words", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("True bool auto-compile", C0),
            C2 = eval(": myabs 0 Int -> Int ; .", C1),
            C3 = eval(": myabs Int -> Int ; dup 0 < dup 0 swap - swap drop .", C2),
            %% Multi-clause should be auto-compiled
            {ok, Op} = af_type:find_op_by_name("myabs", 'Int'),
            ?assertMatch({native, _}, Op#operation.source)
        end} end
    ]}.

%% Expose group_by_name for testing
group_by_name(WordDefs) ->
    af_word_compiler:group_by_name(WordDefs).
