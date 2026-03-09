#!/usr/bin/env escript
%%! -pa _build/default/lib/actorforth/ebin

%% Comprehensive benchmark: exercises all major A4 operations
%% comparing interpreted vs auto-compiled (native BEAM) execution.

-include_lib("actorforth/include/token.hrl").
-include_lib("actorforth/include/continuation.hrl").

main(_) ->
    init_types(),

    io:format("~n=== ActorForth Comprehensive Operations Benchmark ===~n~n"),

    %% Define test words (interpreted)
    C0 = af_interpreter:new_continuation(),
    C1 = define_words(C0),

    %% Run interpreted benchmarks
    io:format("--- Interpreted (closure-based) ---~n"),
    IntArith = bench("  Arithmetic chain (1000x)", fun() -> run_arith(C1, 1000) end),
    IntList  = bench("  List ops (1000x)",         fun() -> run_list_ops(C1, 1000) end),
    IntStr   = bench("  String ops (1000x)",       fun() -> run_string_ops(C1, 1000) end),
    IntMap   = bench("  Map ops (1000x)",          fun() -> run_map_ops(C1, 1000) end),
    IntProd  = bench("  Product type ops (1000x)", fun() -> run_product_ops(C1, 1000) end),
    IntComp  = bench("  Compiled word chain (1000x)", fun() -> run_compiled_words(C1, 1000) end),

    %% Now compile all words to native BEAM
    C2 = compile_words(C1),

    io:format("~n--- Native BEAM (compiled) ---~n"),
    NatArith = bench("  Arithmetic chain (1000x)", fun() -> run_arith(C2, 1000) end),
    NatList  = bench("  List ops (1000x)",         fun() -> run_list_ops(C2, 1000) end),
    NatStr   = bench("  String ops (1000x)",       fun() -> run_string_ops(C2, 1000) end),
    NatMap   = bench("  Map ops (1000x)",          fun() -> run_map_ops(C2, 1000) end),
    NatProd  = bench("  Product type ops (1000x)", fun() -> run_product_ops(C2, 1000) end),
    NatComp  = bench("  Compiled word chain (1000x)", fun() -> run_compiled_words(C2, 1000) end),

    io:format("~n--- Speedup (native / interpreted) ---~n"),
    report_speedup("  Arithmetic", IntArith, NatArith),
    report_speedup("  List ops",   IntList,  NatList),
    report_speedup("  String ops", IntStr,   NatStr),
    report_speedup("  Map ops",    IntMap,   NatMap),
    report_speedup("  Product",    IntProd,  NatProd),
    report_speedup("  Word chain", IntComp,  NatComp),

    IntTotal = IntArith + IntList + IntStr + IntMap + IntProd + IntComp,
    NatTotal = NatArith + NatList + NatStr + NatMap + NatProd + NatComp,
    report_speedup("  TOTAL",      IntTotal, NatTotal),
    io:format("~n").

init_types() ->
    af_type:init(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_compiler:init(),
    af_type_product:init(),
    af_type_string:init(),
    af_type_map:init(),
    af_type_list:init(),
    af_type_float:init(),
    af_type_tuple:init().

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "bench"),
    af_interpreter:interpret_tokens(Tokens, Cont).

define_words(C0) ->
    %% Arithmetic words
    C1 = eval(": square Int -> Int ; dup * .", C0),
    C2 = eval(": cube Int -> Int ; dup dup * * .", C1),
    C3 = eval(": arith-chain Int -> Int ; dup square swap cube + abs .", C2),

    %% List words
    C4 = eval(": list-sum List -> Int ; 0 + reduce .", C3),
    C5 = eval(": double Int -> Int ; 2 * .", C4),
    C6 = eval(": list-work List -> Int ; double map 0 + reduce .", C5),

    %% String words
    C7 = eval(": str-work String -> String ; trim to-upper .", C6),

    %% Product type words
    C8 = eval("type Vec3 x Int y Int z Int .", C7),
    C9 = eval(": vec-mag2 Vec3 -> Vec3 Int ; x square swap y square rot + swap z square rot + .", C8),

    %% Composed words
    C10 = eval(": compose-test Int -> Int ; dup square swap cube max abs .", C9),
    C10.

compile_words(C) ->
    Words = ["square", "cube", "arith-chain", "double", "compose-test", "str-work", "vec-mag2"],
    lists:foldl(fun(W, Acc) ->
        eval("\"" ++ W ++ "\" compile", Acc)
    end, C, Words).

bench(Label, Fun) ->
    %% Warmup
    Fun(),
    %% Timed run (5 iterations, take median)
    Times = [begin
        {T, _} = timer:tc(Fun),
        T
    end || _ <- lists:seq(1, 5)],
    Sorted = lists:sort(Times),
    Median = lists:nth(3, Sorted),
    io:format("~s: ~.1f ms~n", [Label, Median / 1000.0]),
    Median.

report_speedup(Label, IntTime, NatTime) ->
    Speedup = IntTime / max(NatTime, 1),
    io:format("~s: ~.1fx faster~n", [Label, Speedup]).

run_arith(C, N) ->
    lists:foreach(fun(I) ->
        eval(integer_to_list(I) ++ " arith-chain drop", C)
    end, lists:seq(1, N)).

run_list_ops(C, N) ->
    %% Build a list, apply ops
    lists:foreach(fun(_) ->
        eval("nil 1 cons 2 cons 3 cons 4 cons 5 cons reverse nil 10 cons 20 cons append length drop", C)
    end, lists:seq(1, N)).

run_string_ops(C, N) ->
    lists:foreach(fun(_) ->
        eval("\"  hello world  \" str-work \"  foo bar  \" str-work concat \"baz\" concat drop", C)
    end, lists:seq(1, N)).

run_map_ops(C, N) ->
    lists:foreach(fun(_) ->
        eval("map-new 1 \"a\" map-put 2 \"b\" map-put 3 \"c\" map-put map-keys length drop", C)
    end, lists:seq(1, N)).

run_product_ops(C, N) ->
    lists:foreach(fun(I) ->
        eval(integer_to_list(I) ++ " " ++ integer_to_list(I+1) ++ " " ++ integer_to_list(I+2) ++ " vec3 vec-mag2 drop drop", C)
    end, lists:seq(1, N)).

run_compiled_words(C, N) ->
    lists:foreach(fun(I) ->
        eval(integer_to_list(I) ++ " compose-test drop", C)
    end, lists:seq(1, N)).
