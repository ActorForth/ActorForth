-module(af_literal_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_list:init(),
    af_type_actor:init().

%% --- Int literal handler ---

int_literal_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"bare number becomes Int", fun() ->
            C = eval("42", af_interpreter:new_continuation()),
            [{'Int', 42}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"bare number in arithmetic", fun() ->
            C = eval("40 2 +", af_interpreter:new_continuation()),
            [{'Int', 42}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"explicit int constructor still works", fun() ->
            C = eval("42 int", af_interpreter:new_continuation()),
            [{'Int', 42}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"int pass-through on already-Int value", fun() ->
            C = eval("42 int int", af_interpreter:new_continuation()),
            [{'Int', 42}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"negative number auto-converted by literal handler", fun() ->
            C = eval("-5", af_interpreter:new_continuation()),
            [{'Int', -5}] = C#continuation.data_stack
        end} end
    ]}.

%% --- Bool literal handler ---

bool_literal_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"True becomes Bool", fun() ->
            C = eval("True", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"False becomes Bool", fun() ->
            C = eval("False", af_interpreter:new_continuation()),
            [{'Bool', false}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"lowercase true becomes Bool", fun() ->
            C = eval("true", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"explicit bool constructor still works", fun() ->
            C = eval("True bool", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"bool pass-through on already-Bool value", fun() ->
            C = eval("True bool bool", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"non-bool word stays Atom", fun() ->
            C = eval("junk", af_interpreter:new_continuation()),
            [{'Atom', "junk"}] = C#continuation.data_stack
        end} end
    ]}.

%% --- Mixed literal + arithmetic ---

mixed_literal_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"literals in complex expression", fun() ->
            C = eval("10 20 + 2 *", af_interpreter:new_continuation()),
            [{'Int', 60}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"literals with comparison", fun() ->
            C = eval("5 3 >", af_interpreter:new_continuation()),
            [{'Bool', true}] = C#continuation.data_stack
        end} end,
        fun(_) -> {"literals inside word body", fun() ->
            C1 = eval(": double Int -> Int ; dup +.", af_interpreter:new_continuation()),
            C2 = eval("21 double", C1),
            [{'Int', 42}] = C2#continuation.data_stack
        end} end
    ]}.

%% --- Debug on/off ---

debug_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"debug on sets flag", fun() ->
            C = eval("debug on", af_interpreter:new_continuation()),
            ?assert(C#continuation.debug),
            [] = C#continuation.data_stack
        end} end,
        fun(_) -> {"debug off clears flag", fun() ->
            C1 = eval("debug on", af_interpreter:new_continuation()),
            C2 = eval("debug off", C1),
            ?assertNot(C2#continuation.debug),
            [] = C2#continuation.data_stack
        end} end
    ]}.
