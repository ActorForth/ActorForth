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
            %% Push a non-numeric atom so literal handler doesn't fire,
            %% then manually test the int constructor
            %% Use "int" on a string that IS numeric but pushed as Atom via quoting
            %% Actually, numeric tokens get converted by literal handler.
            %% op_int is only called when: Atom on TOS + "int" word invoked.
            %% Numeric tokens auto-convert, so we need a non-auto-convertible path.
            %% Let's push a number as a string that "int" can convert.
            %% Actually "42" auto-converts. The int constructor (op_int) is for
            %% cases like: already Atom "42" on stack -> int -> Int 42.
            %% But literal handler catches "42" first. So op_int only fires
            %% if someone pushes an Atom string and then says "int".
            %% Force it by building the stack manually.
            Cont = #continuation{data_stack = [{'Atom', "99"}]},
            Tokens = af_parser:parse("int", "test"),
            C = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Int', 99}], C#continuation.data_stack)
        end} end
    ]}.
