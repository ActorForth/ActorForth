#!/usr/bin/env escript

%% bench_ops_erlang.escript — Comprehensive operations benchmark (native Erlang)
%%
%% Performs the same computational work as the A4 benchmark using native Erlang,
%% for cross-language performance comparison.
%%
%% Reports microseconds per operation for direct cross-language comparison.
%%
%% Usage:
%%   escript bench_ops_erlang.escript

-define(N, 100000).

main(_) ->
    io:format("~n=== Erlang Native Operations Benchmark ===~n~n"),

    NIters = 5,

    io:format("--- Native Erlang (~p iterations, ~p timed runs, median) ---~n", [?N, NIters]),

    TArith = bench("  Arithmetic chain", fun() -> run_arith(?N) end, NIters),
    TList  = bench("  List ops",         fun() -> run_list_ops(?N) end, NIters),
    TStr   = bench("  String ops",       fun() -> run_string_ops(?N) end, NIters),
    TMap   = bench("  Map ops",          fun() -> run_map_ops(?N) end, NIters),
    TProd  = bench("  Product type ops", fun() -> run_product_ops(?N) end, NIters),
    TComp  = bench("  Compiled word chain", fun() -> run_compiled_words(?N) end, NIters),

    Total = TArith + TList + TStr + TMap + TProd + TComp,
    PerOp = Total / ?N,
    io:format("~n  TOTAL: ~8.1f ms  (~6.3f us/op)~n~n", [Total / 1000.0, PerOp]),

    %% Machine-readable summary for cross-language comparison
    io:format("BENCH_DATA: arith=~.3f list=~.3f string=~.3f map=~.3f product=~.3f wordchain=~.3f~n",
        [TArith/?N, TList/?N, TStr/?N, TMap/?N, TProd/?N, TComp/?N]).


%% --- Arithmetic operations ---

square(X) -> X * X.
cube(X) -> X * X * X.
arith_chain(X) -> abs(square(X) + cube(X)).

%% --- List operations ---

list_ops() ->
    L = [1, 2, 3, 4, 5],
    L1 = lists:reverse(L),
    L2 = L1 ++ [10, 20],
    length(L2).

%% --- String operations ---

str_work(S) ->
    Trimmed = string:trim(binary_to_list(S)),
    Upper = string:uppercase(Trimmed),
    unicode:characters_to_binary(Upper).

string_ops() ->
    str_work(<<"  hello world  ">>).

%% --- Map operations ---

map_ops() ->
    M0 = #{},
    M1 = maps:put(<<"a">>, 1, M0),
    M2 = maps:put(<<"b">>, 2, M1),
    M3 = maps:put(<<"c">>, 3, M2),
    length(maps:keys(M3)).

%% --- Product type (Vec3) operations ---

-record(vec3, {x, y, z}).

vec_mag2(#vec3{x = X, y = Y, z = Z}) ->
    square(X) + square(Y) + square(Z).

%% --- Composed word chain ---

compose_test(X) -> abs(max(square(X), cube(X))).


%% --- Benchmark runners ---

run_arith(N) ->
    lists:foreach(fun(I) -> arith_chain(I) end, lists:seq(1, N)).

run_list_ops(N) ->
    lists:foreach(fun(_) -> list_ops() end, lists:seq(1, N)).

run_string_ops(N) ->
    lists:foreach(fun(_) -> string_ops() end, lists:seq(1, N)).

run_map_ops(N) ->
    lists:foreach(fun(_) -> map_ops() end, lists:seq(1, N)).

run_product_ops(N) ->
    lists:foreach(fun(I) -> vec_mag2(#vec3{x=I, y=I+1, z=I+2}) end, lists:seq(1, N)).

run_compiled_words(N) ->
    lists:foreach(fun(I) -> compose_test(I) end, lists:seq(1, N)).


%% --- Benchmark harness ---

bench(Label, Fun, NIters) ->
    %% Warmup
    Fun(),
    %% Timed runs, take median
    Times = [begin
        {T, _} = timer:tc(Fun),
        T
    end || _ <- lists:seq(1, NIters)],
    Sorted = lists:sort(Times),
    Median = lists:nth((NIters div 2) + 1, Sorted),
    PerOp = Median / ?N,
    io:format("~s: ~8.1f ms  (~6.3f us/op)~n", [Label, Median / 1000.0, PerOp]),
    Median.
