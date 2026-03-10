-module(af_otp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset().

%% --- gen-server-module ---

gen_server_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile gen_server from product type", fun() ->
            %% Define a Counter product type with words
            C1 = eval("type Counter value Int .", af_interpreter:new_continuation()),
            C2 = eval(": increment Counter -> Counter ; value 1 + value! .", C1),
            C3 = eval(": count Counter -> Counter Int ; value .", C2),
            %% Compile to gen_server module
            C4 = eval("af_gs_counter Counter gen-server-module", C3),
            [{'Atom', "af_gs_counter"}] = C4#continuation.data_stack,
            %% Start with a tagged product type instance as state
            InitState = {'Counter', #{value => {'Int', 0}}},
            {ok, Pid} = gen_server:start_link(af_gs_counter, InitState, []),
            %% Cast: increment (async)
            ok = gen_server:cast(Pid, increment),
            ok = gen_server:cast(Pid, increment),
            ok = gen_server:cast(Pid, increment),
            timer:sleep(50),
            %% Call: count (sync)
            {ok, Count} = gen_server:call(Pid, count),
            ?assertEqual(3, Count),
            gen_server:stop(Pid)
        end} end,

        fun(_) -> {"gen_server handles unknown calls gracefully", fun() ->
            C1 = eval("type Simple value Int .", af_interpreter:new_continuation()),
            C2 = eval(": get Simple -> Simple Int ; value .", C1),
            C3 = eval("af_gs_simple Simple gen-server-module", C2),
            [{'Atom', "af_gs_simple"}] = C3#continuation.data_stack,
            InitState = {'Simple', #{value => {'Int', 42}}},
            {ok, Pid} = gen_server:start_link(af_gs_simple, InitState, []),
            ?assertEqual({error, unknown_call}, gen_server:call(Pid, nonexistent)),
            gen_server:stop(Pid)
        end} end
    ]}.

%% --- af_otp_dispatch ---

dispatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"dispatch call_word returns values", fun() ->
            C1 = eval("type Box value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": get-value Box -> Box Int ; value .", C1),
            State = {'Box', #{value => {'Int', 99}}},
            {ReturnValues, _NewState} = af_otp_dispatch:call_word("get-value", [], State),
            ?assertEqual([99], ReturnValues)
        end} end,
        fun(_) -> {"dispatch cast_word modifies state", fun() ->
            C1 = eval("type Box value Int .", af_interpreter:new_continuation()),
            _C2 = eval(": set-42 Box -> Box ; 42 value! .", C1),
            State = {'Box', #{value => {'Int', 0}}},
            {'Box', NewFields} = af_otp_dispatch:cast_word("set-42", [], State),
            ?assertEqual({'Int', 42}, maps:get(value, NewFields))
        end} end
    ]}.
