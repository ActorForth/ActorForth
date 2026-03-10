-module(af_type_bool_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

bool_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"bool constructor from true Atom", fun() ->
            C = eval("true bool", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"bool constructor from True Atom", fun() ->
            C = eval("True bool", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"bool constructor from false Atom", fun() ->
            C = eval("false bool", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"bool constructor from False Atom", fun() ->
            C = eval("False bool", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"bool passthrough when already Bool", fun() ->
            C = eval("true bool bool", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"not operator", fun() ->
            C = eval("true not", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"equality comparison", fun() ->
            C = eval("3 3 ==", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"inequality comparison", fun() ->
            C = eval("3 4 !=", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"less than", fun() ->
            C = eval("3 5 <", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"greater than", fun() ->
            C = eval("5 3 >", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"less than or equal", fun() ->
            C = eval("3 3 <=", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"greater than or equal", fun() ->
            C = eval("5 3 >=", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"literal true parsed directly", fun() ->
            C = eval("true", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"literal false parsed directly", fun() ->
            C = eval("false", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"literal True parsed directly", fun() ->
            C = eval("True", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"literal False parsed directly", fun() ->
            C = eval("False", af_interpreter:new_continuation()),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"bool constructor from Atom 'true' (op_bool path)", fun() ->
            %% Literal handler auto-converts "true" to Bool, so op_bool never fires
            %% via normal eval. Test by manually placing Atom on stack.
            Cont = #continuation{data_stack = [{'Atom', "true"}]},
            Tokens = af_parser:parse("bool", "test"),
            C = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"bool constructor from Atom 'True' (op_bool path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "True"}]},
            Tokens = af_parser:parse("bool", "test"),
            C = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Bool', true}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"bool constructor from Atom 'false' (op_bool path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "false"}]},
            Tokens = af_parser:parse("bool", "test"),
            C = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end,

        fun(_) -> {"bool constructor from Atom 'False' (op_bool path)", fun() ->
            Cont = #continuation{data_stack = [{'Atom', "False"}]},
            Tokens = af_parser:parse("bool", "test"),
            C = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Bool', false}], C#continuation.data_stack)
        end} end
    ]}.
