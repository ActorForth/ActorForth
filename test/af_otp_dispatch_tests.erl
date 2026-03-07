-module(af_otp_dispatch_tests).

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
    af_type_string:init(),
    af_type_list:init(),
    af_type_float:init(),
    af_type_tuple:init(),
    af_type_ffi:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_beam:init(),
    af_type_otp:init().

%% Test the empty result stack case (line 24)
empty_result_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"call_word with word that drops everything returns empty + original state", fun() ->
            %% Define a word that drops the state item from the stack
            C1 = eval("type Dropper value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": vanish Dropper -> ; drop .", C1),
            State = {'Dropper', #{value => {'Int', 0}}},
            {ReturnValues, ReturnState} = af_otp_dispatch:call_word("vanish", [], State),
            ?assertEqual([], ReturnValues),
            ?assertEqual(State, ReturnState)
        end} end
    ]}.

%% Test ensure_types when registry is undefined (line 42)
ensure_types_test_() ->
    [
        {"ensure_types initializes when registry undefined", fun() ->
            %% Destroy the registry to trigger the undefined branch
            catch ets:delete(af_type_registry),
            %% call_word should trigger ensure_types which inits the registry
            %% We just need any valid call - use a simple word
            %% After ensure_types, Int type should be available
            State = {'Int', 5},
            {_Ret, _NewState} = af_otp_dispatch:call_word("dup", [], State),
            %% If we got here without error, ensure_types worked
            ?assert(true)
        end}
    ].
