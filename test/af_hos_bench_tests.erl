-module(af_hos_bench_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

%% Benchmark harness for the HOS elevator demo.
%%
%% Runs the timer-free variant (`elevator_nowait.a4`) and measures
%% end-to-end message-passing throughput for N floor-button-press
%% cycles. Also provides a smoke test that drives the original
%% timer-bearing `elevator.a4` through the same harness at a much
%% lower N, to prove the harness machinery works against the normal
%% spec too.
%%
%% Produces a human-readable events/sec number on io:format so the
%% run_bench.sh orchestrator can scrape it.
%%
%% Expected events per floor-button-press cycle, observed by Car:
%%   assign, closed, arrived, settled, opened, depart, closed = 7
%%
%% One full cycle also causes the following system-global events:
%%   BuildingSystem:  floor-button-press
%%   Dispatcher:      request
%%   Car:             7 (as above)
%%   Door:            close, open, opened, close, closed = 5
%%   Motor:           move-to, arrived = 2
%% Total across the tree: 1 + 1 + 7 + 5 + 2 = 16 events per cycle.
%%
%% For the reported throughput number we use the 16-events-per-cycle
%% total; this is the number other languages' benches will also count
%% (one request drives the whole state machine through all subsystems).

-define(CYCLE_EVENTS, 16).
-define(CAR_EVENTS_PER_CYCLE, 7).

setup() ->
    af_repl:init_types(),
    af_type_compiler:clear_pending_checks(),
    af_hos_check:clear_registry().

load_spec(Path) ->
    {ok, Content} = file:read_file(Path),
    Tokens = af_parser:parse(binary_to_list(Content), Path),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()).

get_log(Pid) ->
    Ref = make_ref(),
    Pid ! {get_log, self(), Ref},
    receive
        {reply, Ref, Log} -> Log
    after 5000 ->
        {error, timeout}
    end.

introspect_child_pid(ParentPid, ChildName) ->
    Ref = make_ref(),
    ParentPid ! {introspect_child, ChildName, self(), Ref},
    receive
        {reply, Ref, Pid} -> Pid
    after 5000 ->
        erlang:error(introspect_timeout)
    end.

%% Wait until Car has accumulated `Target` additional log entries, or
%% until the deadline passes. Returns the final log length so the
%% caller can decide whether progress stalled.
wait_for_car_growth(CarPid, Before, Target, DeadlineMs) ->
    Now = erlang:monotonic_time(millisecond),
    wait_for_car_growth(CarPid, Before, Target, Now + DeadlineMs, Now).

wait_for_car_growth(CarPid, Before, Target, Deadline, _Last) ->
    Current = length(get_log(CarPid)),
    case Current - Before >= Target of
        true -> {ok, Current};
        false ->
            Now = erlang:monotonic_time(millisecond),
            case Now >= Deadline of
                true -> {stalled, Current};
                false ->
                    timer:sleep(1),
                    wait_for_car_growth(
                        CarPid, Before, Target, Deadline, Now)
            end
    end.

%% Core timing loop. Sends N floor-button-press events, synchronises
%% on Car's log growth between events so one cycle completes before
%% the next starts. Returns {Cycles, ElapsedMs}.
run_cycles(RootPid, CarPid, N) ->
    BeforeStart = length(get_log(CarPid)),
    Start = erlang:monotonic_time(millisecond),
    Cycles = run_cycle_loop(RootPid, CarPid, 0, N, BeforeStart),
    End = erlang:monotonic_time(millisecond),
    {Cycles, End - Start}.

run_cycle_loop(_RootPid, _CarPid, Done, N, _Before) when Done >= N ->
    Done;
run_cycle_loop(RootPid, CarPid, Done, N, Before) ->
    af_hos_runtime:send_event(RootPid, 'floor-button-press', []),
    case wait_for_car_growth(
            CarPid, Before, ?CAR_EVENTS_PER_CYCLE, 2000) of
        {ok, NewLen} ->
            run_cycle_loop(RootPid, CarPid, Done + 1, N, NewLen);
        {stalled, NewLen} ->
            io:format(user,
                "~n[bench] cycle ~p stalled at log-len=~p (needed +~p)~n",
                [Done + 1, NewLen - Before, ?CAR_EVENTS_PER_CYCLE]),
            Done
    end.

report_bench(Label, Cycles, ElapsedMs, EventsPerCycle) ->
    Seconds = ElapsedMs / 1000.0,
    Events = Cycles * EventsPerCycle,
    EventsPerSec = case Seconds > 0.0 of
                       true  -> Events / Seconds;
                       false -> 0.0
                   end,
    io:format(user,
        "~n=== ~s ===~n"
        "cycles=~p  events=~p  wall=~.3fs  events/sec=~.2f~n"
        "[BENCH] ~s events/sec=~.2f cycles=~p wall=~.3fs~n",
        [Label, Cycles, Events, Seconds, EventsPerSec,
         Label, EventsPerSec, Cycles, Seconds]),
    EventsPerSec.


%% -------------------------------------------------------------------
%% Nowait bench: the real measurement, high N.
%% -------------------------------------------------------------------

nowait_bench_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     [{"elevator_nowait.a4 throughput (N=1000)",
       {timeout, 300, fun() ->
        load_spec("samples/hos/elevator/bench/elevator_nowait.a4"),
        Root = af_hos_check:lookup_system("BuildingSystem"),
        RootPid = af_hos_runtime:spawn_system(Root),
        try
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            CarPid  = introspect_child_pid(DispPid, "Car"),
            %% Warm-up: 10 cycles to let the scheduler settle.
            {_WarmCycles, _WarmMs} = run_cycles(RootPid, CarPid, 10),
            %% Timed run.
            N = 1000,
            {Cycles, ElapsedMs} = run_cycles(RootPid, CarPid, N),
            Rate = report_bench("Erlang HOS (nowait)",
                                Cycles, ElapsedMs, ?CYCLE_EVENTS),
            ?assertEqual(N, Cycles),
            ?assert(Rate >= 100.0)
        after
            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end
      end}}]}.


%% -------------------------------------------------------------------
%% Timered bench: prove the harness also works against the original
%% spec. Low N because a full cycle is ~1.2 seconds of wall time.
%% -------------------------------------------------------------------

timered_smoke_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     [{"elevator.a4 throughput smoke (N=10)",
       {timeout, 120, fun() ->
        load_spec("samples/hos/elevator/elevator.a4"),
        Root = af_hos_check:lookup_system("BuildingSystem"),
        RootPid = af_hos_runtime:spawn_system(Root),
        try
            DispPid = introspect_child_pid(RootPid, "Dispatcher"),
            CarPid  = introspect_child_pid(DispPid, "Car"),
            N = 10,
            {Cycles, ElapsedMs} = run_cycles_slow(RootPid, CarPid, N),
            Rate = report_bench("Erlang HOS (timered)",
                                Cycles, ElapsedMs, ?CYCLE_EVENTS),
            ?assertEqual(N, Cycles),
            ?assert(Rate > 0.0)
        after
            af_hos_runtime:stop_system(RootPid),
            timer:sleep(50)
        end
      end}}]}.

%% Slow variant for the timered spec: deadline per cycle is 5s to
%% comfortably accommodate the ~1.2s real-time cycle.
run_cycles_slow(RootPid, CarPid, N) ->
    BeforeStart = length(get_log(CarPid)),
    Start = erlang:monotonic_time(millisecond),
    Cycles = run_cycle_loop_slow(RootPid, CarPid, 0, N, BeforeStart),
    End = erlang:monotonic_time(millisecond),
    {Cycles, End - Start}.

run_cycle_loop_slow(_RootPid, _CarPid, Done, N, _Before) when Done >= N ->
    Done;
run_cycle_loop_slow(RootPid, CarPid, Done, N, Before) ->
    af_hos_runtime:send_event(RootPid, 'floor-button-press', []),
    case wait_for_car_growth(
            CarPid, Before, ?CAR_EVENTS_PER_CYCLE, 5000) of
        {ok, NewLen} ->
            run_cycle_loop_slow(RootPid, CarPid, Done + 1, N, NewLen);
        {stalled, NewLen} ->
            io:format(user,
                "~n[bench] slow cycle ~p stalled at log-len=~p~n",
                [Done + 1, NewLen - Before]),
            Done
    end.
