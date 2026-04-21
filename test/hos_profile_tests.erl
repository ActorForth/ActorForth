-module(hos_profile_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

%% eprof profile of the HOS nowait bench to identify hot spots.
%% Enable with: rebar3 eunit --module=hos_profile_tests

profile_test_() ->
    {timeout, 120, fun() ->
        af_repl:init_types(),
        af_type_compiler:clear_pending_checks(),
        af_hos_check:clear_registry(),

        {ok, Content} = file:read_file("samples/hos/elevator/bench/elevator_nowait.a4"),
        Tokens = af_parser:parse(binary_to_list(Content), "elevator_nowait.a4"),
        Cont = af_interpreter:new_continuation(),
        _Cont2 = af_interpreter:interpret_tokens(Tokens, Cont),
        af_type_compiler:finalize_pending_checks(),

        C1 = af_interpreter:new_continuation(),
        T1 = af_parser:parse("\"BuildingSystem\" find-system", "bench"),
        C2 = af_interpreter:interpret_tokens(T1, C1),
        [Node | _] = C2#continuation.data_stack,

        Pid = af_hos_runtime:spawn_system(Node),
        timer:sleep(20),

        eprof:start(),
        eprof:start_profiling([Pid]),
        N = 100,
        Events = [call_button_pressed, up_arrival, door_opened, doors_closed,
                  up_destination, up_arrival, door_opened, doors_closed,
                  up_arrival, door_opened, doors_closed, up_destination,
                  up_arrival, door_opened, doors_closed, call_button_pressed],
        lists:foreach(fun(_) ->
            lists:foreach(fun(E) -> af_hos_runtime:send_event(Pid, E, []) end, Events)
        end, lists:seq(1, N)),
        timer:sleep(300),
        eprof:stop_profiling(),
        af_hos_runtime:stop_system(Pid),
        eprof:log("/tmp/eprof.out"),
        eprof:analyze(total, [{sort, time}]),
        eprof:stop()
    end}.
