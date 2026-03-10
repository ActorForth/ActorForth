-module(af_type_int_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

arithmetic_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"int constructor from Atom", fun() ->
            C = eval("42 int", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 42}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"int passthrough when already Int", fun() ->
            C = eval("42 int int", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 42}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"addition", fun() ->
            C = eval("3 5 +", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 8}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"subtraction", fun() ->
            C = eval("10 3 -", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 7}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"multiplication", fun() ->
            C = eval("4 5 *", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 20}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"division", fun() ->
            C = eval("20 4 /", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"division by zero raises error", fun() ->
            ?assertError(_, eval("10 0 /", af_interpreter:new_continuation()))
        end} end,

        fun(_) -> {"literal int pushed directly", fun() ->
            C = eval("123", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 123}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"negative literal", fun() ->
            C = eval("-7", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', -7}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"int constructor from numeric string atom (op_int path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "99"}]},
            Tokens = af_parser:parse("int", "test"),
            C = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Int', 99}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"modulo operation", fun() ->
            C = eval("10 3 mod", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 1}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"modulo by zero raises error", fun() ->
            ?assertError(_, eval("10 0 mod", af_interpreter:new_continuation()))
        end} end,

        fun(_) -> {"abs operation", fun() ->
            C = eval("-5 abs", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"abs of positive", fun() ->
            C = eval("5 abs", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"max operation", fun() ->
            C = eval("3 7 max", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 7}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"max with equal values", fun() ->
            C = eval("5 5 max", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"min operation", fun() ->
            C = eval("3 7 min", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 3}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"min with equal values", fun() ->
            C = eval("5 5 min", af_interpreter:new_continuation()),
            ?assertEqual([{'Int', 5}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"int passthrough on already-Int value directly", fun() ->
            %% Build stack with Int already on it, call int
            Cont = #continuation{data_stack = [{'Int', 42}]},
            Tokens = af_parser:parse("int", "test"),
            C = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Int', 42}], C#continuation.data_stack)
        end} end
    ]}.
