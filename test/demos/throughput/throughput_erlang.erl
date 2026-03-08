%%% throughput_erlang.erl — Actor message dispatch throughput benchmark
%%%
%%% Erlang equivalent of throughput_demo.a4. A gen_server Counter actor
%%% receives N bump casts, then a sync get_count call verifies all arrived.
%%% No sleep delays — the sync call guarantees FIFO ordering.

-module(throughput_erlang).
-behaviour(gen_server).

-export([start_test/0, bench/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(counter, {count = 0 :: integer()}).

%% gen_server callbacks
init(Initial) ->
    {ok, #counter{count = Initial}}.

handle_cast(bump, #counter{count = N} = S) ->
    {noreply, S#counter{count = N + 1}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Unknown, State) ->
    {noreply, State}.

handle_call(get_count, _From, #counter{count = N} = S) ->
    {reply, N, S};
handle_call(_Unknown, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Send N bump casts to Pid
blast(0, _Pid) -> ok;
blast(N, Pid) ->
    gen_server:cast(Pid, bump),
    blast(N - 1, Pid).

run_once() ->
    run_once(100000).

run_once(N) ->
    {ok, Pid} = gen_server:start_link(?MODULE, 0, []),
    blast(N, Pid),
    N = gen_server:call(Pid, get_count),
    gen_server:cast(Pid, stop),
    ok.

start_test() ->
    run_once(),
    io:format("Erlang throughput test passed (100000 messages).~n").

bench(Iters) when Iters > 0 ->
    bench(Iters, 100000).

bench(Iters, N) when Iters > 0 ->
    %% Warm-up
    run_once(N),
    Times = [begin
        {T, _} = timer:tc(fun() -> run_once(N) end),
        T
    end || _ <- lists:seq(1, Iters)],
    Min = lists:min(Times),
    Max = lists:max(Times),
    Avg = lists:sum(Times) div Iters,
    io:format("BENCH_RESULT: min=~b avg=~b max=~b iters=~b msgs=~b~n",
              [Min, Avg, Max, Iters, N]).
