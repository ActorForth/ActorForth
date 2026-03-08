%%% debounce_erlang_equivalent.erl
%%%
%%% Erlang equivalent of debounce_demo.a4 — for side-by-side comparison.
%%%
%%% This module implements the same feature set as the ActorForth demo:
%%%   - Product types (Entry, Stats, Batch, Tally/Logger, Store)
%%%   - EventStore actor (accumulates event counts)
%%%   - Logger actor (receives typed batch summaries, counts them)
%%%   - Tests verifying construction, field access, and actor protocols
%%%
%%% WHAT THIS FILE DEMONSTRATES
%%% ============================
%%% This is the minimal Erlang code needed for feature parity with
%%% debounce_demo.a4. Both files do the same thing. Compare:
%%%
%%%   debounce_demo.a4:               ~80 lines of definitions + tests
%%%   debounce_erlang_equivalent.erl: ~280 lines for the same functionality
%%%
%%% The difference comes from:
%%%   1. gen_server boilerplate (init, handle_call, handle_cast, terminate,
%%%      code_change) — required per actor, even if most clauses do nothing.
%%%   2. Record definitions in a separate .hrl file (inlined here for
%%%      comparison, but normally split across files).
%%%   3. Explicit start/stop lifecycle management.
%%%   4. No type safety at message boundaries — you can send any term to
%%%      any process. A typo in a message tag is a silent no-op (handled
%%%      by the catch-all clause), not a compile-time error.
%%%   5. Nested record updates require intermediate variables for each
%%%      level of nesting.
%%%
%%% NOTE ON THE PRODUCTION CODE
%%% ============================
%%% The actual BigVault debounce_actor.erl (255 lines) handles 8 event
%%% types with nested record updates, timer management, and statistics
%%% accumulation. It also depends on:
%%%   - debounce_actor.hrl (25 lines of record definitions)
%%%   - vault_config.hrl (10 lines)
%%%   - storage_monitor.hrl (46 lines of related records)
%%%   - A separate dynamic supervisor module
%%%   - A separate logging gen_server module
%%% Total: ~350 lines across 5 files for the debounce subsystem alone.

-module(debounce_erlang_equivalent).

-behaviour(gen_server).

%% API
-export([start_test/0, bench/1]).

%% gen_server callbacks (required by behaviour — cannot be omitted)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%% ============================================================
%%% Record definitions — equivalent to ActorForth product types
%%% ============================================================
%%% In ActorForth these are 5 one-line type declarations.
%%% In Erlang, records provide named fields but no type enforcement
%%% at message boundaries.

-record(entry, {
    path     :: binary(),
    hash     :: binary(),
    size     :: integer(),
    kind     :: binary()
}).

-record(stats, {
    size_diff  :: integer(),
    count_diff :: integer()
}).

-record(batch, {
    added      :: integer(),
    deleted    :: integer(),
    size_delta :: integer(),
    count_delta :: integer()
}).

%% Logger state
-record(tally, {
    batches = 0 :: integer()
}).

%% EventStore state
-record(store, {
    events = 0 :: integer()
}).

%%% ============================================================
%%% Actor type tag — needed to multiplex two actor types in one
%%% module. ActorForth doesn't need this because each product
%%% type is a separate actor type automatically.
%%% ============================================================

-record(actor_state, {
    type    :: store | tally,
    state   :: #store{} | #tally{}
}).


%%% ============================================================
%%% gen_server boilerplate — required per actor
%%% ============================================================
%%% ActorForth equivalent: "0 store server" (3 words)

init({store, Initial}) ->
    {ok, #actor_state{type = store, state = #store{events = Initial}}};
init({tally, Initial}) ->
    {ok, #actor_state{type = tally, state = #tally{batches = Initial}}}.

%% --- EventStore operations ---

%% record: add N to event count
handle_cast({record, N}, #actor_state{type = store, state = S} = AS) ->
    {noreply, AS#actor_state{state = S#store{events = S#store.events + N}}};

%% log_batch: receive a batch, increment batch count
handle_cast({log_batch, #batch{}}, #actor_state{type = tally, state = T} = AS) ->
    {noreply, AS#actor_state{state = T#tally{batches = T#tally.batches + 1}}};

%% stop
handle_cast(stop, State) ->
    {stop, normal, State};

%% Catch-all — silently drops unknown messages.
%% This is the clause that makes Erlang actors fragile:
%% a typo like {reocrd, 5} instead of {record, 5} is silently ignored.
%% In ActorForth, the typed actor vocab rejects it before sending.
handle_cast(_Unknown, State) ->
    {noreply, State}.

%% total: get EventStore event count
handle_call(total, _From, #actor_state{type = store, state = S} = AS) ->
    {reply, S#store.events, AS};

%% logged: get Logger batch count
handle_call(logged, _From, #actor_state{type = tally, state = T} = AS) ->
    {reply, T#tally.batches, AS};

%% Catch-all — silently returns error for unknown calls.
handle_call(_Unknown, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% Unused but required by gen_server behaviour — cannot be omitted
%% without a compiler warning.
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% ============================================================
%%% Test harness
%%% ============================================================
%%% Equivalent to the assertion section of debounce_demo.a4.
%%% Run with: debounce_erlang_equivalent:start_test().

start_test() ->
    test_product_types(),
    test_actors(),
    io:format("All Erlang equivalent tests passed.~n").

%% Benchmark: run tests N times, report min/avg/max in microseconds.
%% Usage: erl -eval "debounce_erlang_equivalent:bench(100), halt()."
bench(N) when N > 0 ->
    %% Warm-up run
    run_once(),
    Times = [begin
        {T, _} = timer:tc(fun run_once/0),
        T
    end || _ <- lists:seq(1, N)],
    Min = lists:min(Times),
    Max = lists:max(Times),
    Avg = lists:sum(Times) div N,
    io:format("BENCH_RESULT: min=~bus avg=~bus max=~bus (~b iterations)~n",
              [Min, Avg, Max, N]).

run_once() ->
    test_product_types(),
    test_actors().

test_product_types() ->
    %% --- Batch construction and field access ---
    %% ActorForth: 5 3 200 2 batch
    %%             added 5 assert-eq
    B = #batch{added = 5, deleted = 3, size_delta = 200, count_delta = 2},
    5 = B#batch.added,
    3 = B#batch.deleted,
    200 = B#batch.size_delta,
    2 = B#batch.count_delta,

    %% --- Stats ---
    %% ActorForth: 1024 1 stats
    S = #stats{size_diff = 1024, count_diff = 1},
    1024 = S#stats.size_diff,
    1 = S#stats.count_diff,

    %% --- Entry ---
    %% ActorForth: "/docs/report.pdf" "abc123" 4096 "application/pdf" entry
    E = #entry{path = <<"/docs/report.pdf">>, hash = <<"abc123">>,
               size = 4096, kind = <<"application/pdf">>},
    <<"/docs/report.pdf">> = E#entry.path,
    <<"abc123">> = E#entry.hash,
    4096 = E#entry.size,
    <<"application/pdf">> = E#entry.kind,

    ok.

test_actors() ->
    %% Start actors — ActorForth: "0 store server" / "0 tally server"
    {ok, StorePid} = gen_server:start_link(?MODULE, {store, 0}, []),
    {ok, TallyPid} = gen_server:start_link(?MODULE, {tally, 0}, []),

    %% --- Logger actor tests ---
    %% ActorForth: << 5 3 200 2 batch log_batch >>
    Batch1 = #batch{added = 5, deleted = 3, size_delta = 200, count_delta = 2},
    gen_server:cast(TallyPid, {log_batch, Batch1}),

    %% ActorForth: << 10 1 500 9 batch log_batch >>
    Batch2 = #batch{added = 10, deleted = 1, size_delta = 500, count_delta = 9},
    gen_server:cast(TallyPid, {log_batch, Batch2}),

    %% ActorForth: << logged >> 2 assert-eq
    %% Need a small delay because casts are async — ActorForth's
    %% server protocol handles this transparently via call semantics.
    timer:sleep(10),
    2 = gen_server:call(TallyPid, logged),

    %% --- EventStore actor tests ---
    %% ActorForth: << 5 record >>
    gen_server:cast(StorePid, {record, 5}),
    %% ActorForth: << 3 record >>
    gen_server:cast(StorePid, {record, 3}),

    %% ActorForth: << total >> 8 assert-eq
    timer:sleep(10),
    8 = gen_server:call(StorePid, total),

    %% Cleanup — ActorForth: << stop >> drop
    gen_server:cast(StorePid, stop),
    gen_server:cast(TallyPid, stop),

    ok.
