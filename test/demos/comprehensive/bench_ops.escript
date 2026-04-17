#!/usr/bin/env escript
%%! -pa _build/default/lib/actorforth/ebin

%% Comprehensive benchmark: exercises all major A4 operations
%% comparing interpreted vs native-compiled (BEAM) execution.
%%
%% Key difference from naive benchmarks: tokens are parsed ONCE and
%% re-interpreted N times, just like a real A4 program would execute.
%% For parameterized tests (arithmetic), values are pre-pushed onto the
%% stack before interpreting the pre-parsed token list.
%%
%% Reports microseconds per operation for direct cross-language comparison.

-include_lib("actorforth/include/token.hrl").
-include_lib("actorforth/include/continuation.hrl").

-define(N, 10000).

main(_) ->
    init_types(),

    io:format("~n=== ActorForth Comprehensive Operations Benchmark ===~n"),
    io:format("    (tokens parsed once, interpreted N times — real A4 execution)~n~n"),

    %% Define test words (interpreted)
    C0 = af_interpreter:new_continuation(),
    C1 = define_words(C0),

    %% Pre-parse all benchmark token lists (done ONCE, reused N times)
    ArithTokens   = af_parser:parse("arith-chain drop", "bench"),
    ListTokens    = af_parser:parse("nil 1 cons 2 cons 3 cons 4 cons 5 cons reverse nil 10 cons 20 cons append length drop", "bench"),
    StringTokens  = af_parser:parse("str-work drop", "bench"),
    MapTokens     = af_parser:parse("map-new 1 \"a\" map-put 2 \"b\" map-put 3 \"c\" map-put map-keys length drop", "bench"),
    ProdTokens    = af_parser:parse("vec3 vec-mag2 drop drop", "bench"),
    CompTokens    = af_parser:parse("compose-test drop", "bench"),

    %% Run interpreted benchmarks
    io:format("--- Interpreted (closure-based, ~p iterations) ---~n", [?N]),
    IntArith = bench("  Arithmetic chain", fun() -> run_arith(ArithTokens, C1, ?N) end),
    IntList  = bench("  List ops",         fun() -> run_list(ListTokens, C1, ?N) end),
    IntStr   = bench("  String ops",       fun() -> run_string(StringTokens, C1, ?N) end),
    IntMap   = bench("  Map ops",          fun() -> run_map(MapTokens, C1, ?N) end),
    IntProd  = bench("  Product type ops", fun() -> run_product(ProdTokens, C1, ?N) end),
    IntComp  = bench("  Compiled word chain", fun() -> run_composed(CompTokens, C1, ?N) end),

    %% Compile all words to native BEAM
    C2 = compile_words(C1),

    io:format("~n--- Native BEAM (compiled, ~p iterations) ---~n", [?N]),
    NatArith = bench("  Arithmetic chain", fun() -> run_arith(ArithTokens, C2, ?N) end),
    NatList  = bench("  List ops",         fun() -> run_list(ListTokens, C2, ?N) end),
    NatStr   = bench("  String ops",       fun() -> run_string(StringTokens, C2, ?N) end),
    NatMap   = bench("  Map ops",          fun() -> run_map(MapTokens, C2, ?N) end),
    NatProd  = bench("  Product type ops", fun() -> run_product(ProdTokens, C2, ?N) end),
    NatComp  = bench("  Compiled word chain", fun() -> run_composed(CompTokens, C2, ?N) end),

    %% Direct BEAM calls (no interpreter dispatch — raw compiled function performance)
    io:format("~n--- Direct BEAM (no interpreter, ~p iterations) ---~n", [?N]),
    DirArith = bench("  Arithmetic chain", fun() -> run_direct_arith(?N) end),
    DirList  = bench("  List ops",         fun() -> run_direct_list(?N) end),
    DirStr   = bench("  String ops",       fun() -> run_direct_string(?N) end),
    DirMap   = bench("  Map ops",          fun() -> run_direct_map(?N) end),
    DirProd  = bench("  Product type ops", fun() -> run_direct_product(?N) end),
    DirComp  = bench("  Compiled word chain", fun() -> run_direct_composed(?N) end),

    io:format("~n--- Speedup Summary ---~n"),
    report_speedup("  Arithmetic (interp vs native)", IntArith, NatArith),
    report_speedup("  List ops   (interp vs native)", IntList,  NatList),
    report_speedup("  String ops (interp vs native)", IntStr,   NatStr),
    report_speedup("  Map ops    (interp vs native)", IntMap,   NatMap),
    report_speedup("  Product    (interp vs native)", IntProd,  NatProd),
    report_speedup("  Word chain (interp vs native)", IntComp,  NatComp),

    IntTotal = IntArith + IntList + IntStr + IntMap + IntProd + IntComp,
    NatTotal = NatArith + NatList + NatStr + NatMap + NatProd + NatComp,
    report_speedup("  TOTAL      (interp vs native)", IntTotal, NatTotal),

    io:format("~n"),
    report_speedup("  Arithmetic (interp vs direct)", IntArith, DirArith),
    report_speedup("  List ops   (interp vs direct)", IntList,  DirList),
    report_speedup("  String ops (interp vs direct)", IntStr,   DirStr),
    report_speedup("  Map ops    (interp vs direct)", IntMap,   DirMap),
    report_speedup("  Product    (interp vs direct)", IntProd,  DirProd),
    report_speedup("  Word chain (interp vs direct)", IntComp,  DirComp),
    report_speedup("  Arithmetic (native vs direct)", NatArith, DirArith),
    report_speedup("  List ops   (native vs direct)", NatList,  DirList),
    report_speedup("  String ops (native vs direct)", NatStr,   DirStr),
    report_speedup("  Map ops    (native vs direct)", NatMap,   DirMap),
    report_speedup("  Product    (native vs direct)", NatProd,  DirProd),
    report_speedup("  Word chain (native vs direct)", NatComp,  DirComp),
    io:format("~n"),

    DirTotal = DirArith + DirList + DirStr + DirMap + DirProd + DirComp,
    report_speedup("  TOTAL      (interp vs direct)", IntTotal, DirTotal),
    report_speedup("  TOTAL      (native vs direct)", NatTotal, DirTotal),
    io:format("~n"),

    %% Machine-readable summaries for cross-language comparison
    io:format("BENCH_DATA[interp]: arith=~.3f list=~.3f string=~.3f map=~.3f product=~.3f wordchain=~.3f~n",
        [IntArith/?N, IntList/?N, IntStr/?N, IntMap/?N, IntProd/?N, IntComp/?N]),
    io:format("BENCH_DATA[native]: arith=~.3f list=~.3f string=~.3f map=~.3f product=~.3f wordchain=~.3f~n",
        [NatArith/?N, NatList/?N, NatStr/?N, NatMap/?N, NatProd/?N, NatComp/?N]),
    io:format("BENCH_DATA[direct]: arith=~.3f list=~.3f string=~.3f map=~.3f product=~.3f wordchain=~.3f~n",
        [DirArith/?N, DirList/?N, DirStr/?N, DirMap/?N, DirProd/?N, DirComp/?N]).

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

    %% List benchmark word (wraps inline list ops into a compilable word)
    C10 = eval(": list-bench -> Int ; nil 1 cons 2 cons 3 cons 4 cons 5 cons reverse nil 10 cons 20 cons append length .", C9),

    %% Map benchmark word (wraps inline map ops into a compilable word)
    C11 = eval(": map-bench -> Int ; map-new 1 \"a\" map-put 2 \"b\" map-put 3 \"c\" map-put map-keys length .", C10),

    %% Composed words
    C12 = eval(": compose-test Int -> Int ; dup square swap cube max abs .", C11),
    C12.

compile_words(C) ->
    %% Words with non-empty sig_in can be compiled via the A4 compile word
    Words = ["square", "cube", "arith-chain", "double", "compose-test", "str-work", "vec-mag2"],
    C2 = lists:foldl(fun(W, Acc) ->
        eval("\"" ++ W ++ "\" compile", Acc)
    end, C, Words),
    %% Words with empty sig_in (-> Type) must be compiled directly because
    %% the A4 compile word uses "word-name" which the interpreter dispatches
    %% as an op call (matching empty sig_in) instead of pushing a String literal.
    lists:foreach(fun(W) ->
        Defs = af_word_compiler:find_compiled_word_defs(W),
        Mod = list_to_atom("af_native_" ++ W),
        {ok, Mod} = af_word_compiler:compile_words_to_module(Mod, Defs)
    end, ["list-bench", "map-bench"]),
    C2.

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
    PerOp = Median / ?N,
    io:format("~s: ~8.1f ms  (~9.3f us/op)~n", [Label, Median / 1000.0, PerOp]),
    Median.

report_speedup(Label, SlowTime, FastTime) ->
    Speedup = SlowTime / max(FastTime, 1),
    io:format("~s: ~.1fx~n", [Label, Speedup]).

%% --- Benchmark runners: pre-parsed tokens, interpret N times ---

%% Arithmetic: push Int value on stack, then interpret "arith-chain drop"
run_arith(Tokens, C, N) ->
    lists:foreach(fun(I) ->
        C1 = C#continuation{data_stack = [{'Int', I}]},
        af_interpreter:interpret_tokens(Tokens, C1)
    end, lists:seq(1, N)).

%% List ops: no per-iteration params, just re-interpret pre-parsed tokens
run_list(Tokens, C, N) ->
    lists:foreach(fun(_) ->
        af_interpreter:interpret_tokens(Tokens, C)
    end, lists:seq(1, N)).

%% String ops: push string value, then interpret "str-work drop"
run_string(Tokens, C, N) ->
    lists:foreach(fun(_) ->
        C1 = C#continuation{data_stack = [{'String', <<"  hello world  ">>}]},
        af_interpreter:interpret_tokens(Tokens, C1)
    end, lists:seq(1, N)).

%% Map ops: same
run_map(Tokens, C, N) ->
    lists:foreach(fun(_) ->
        af_interpreter:interpret_tokens(Tokens, C)
    end, lists:seq(1, N)).

%% Product: push 3 Ints, then interpret "vec3 vec-mag2 drop drop"
run_product(Tokens, C, N) ->
    lists:foreach(fun(I) ->
        C1 = C#continuation{data_stack = [{'Int', I+2}, {'Int', I+1}, {'Int', I}]},
        af_interpreter:interpret_tokens(Tokens, C1)
    end, lists:seq(1, N)).

%% Composed: push Int, interpret "compose-test drop"
run_composed(Tokens, C, N) ->
    lists:foreach(fun(I) ->
        C1 = C#continuation{data_stack = [{'Int', I}]},
        af_interpreter:interpret_tokens(Tokens, C1)
    end, lists:seq(1, N)).

%% --- Direct BEAM calls: call compiled module functions directly ---

run_direct_arith(N) ->
    lists:foreach(fun(I) ->
        'af_native_arith-chain':'arith-chain'([{'Int', I}])
    end, lists:seq(1, N)).

run_direct_list(N) ->
    lists:foreach(fun(_) ->
        'af_native_list-bench':'list-bench'([])
    end, lists:seq(1, N)).

run_direct_string(N) ->
    lists:foreach(fun(_) ->
        'af_native_str-work':'str-work'([{'String', <<"  hello world  ">>}])
    end, lists:seq(1, N)).

run_direct_map(N) ->
    lists:foreach(fun(_) ->
        'af_native_map-bench':'map-bench'([])
    end, lists:seq(1, N)).

run_direct_product(N) ->
    lists:foreach(fun(I) ->
        %% Construct Vec3 instance directly (what the constructor does).
        %% New storage: positional tuple with raw values.
        Instance = {'Vec3', I, I+1, I+2},
        'af_native_vec-mag2':'vec-mag2'([Instance])
    end, lists:seq(1, N)).

run_direct_composed(N) ->
    lists:foreach(fun(I) ->
        'af_native_compose-test':'compose-test'([{'Int', I}])
    end, lists:seq(1, N)).
