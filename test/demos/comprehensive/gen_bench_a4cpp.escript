#!/usr/bin/env escript
%%! -pa _build/default/lib/actorforth/ebin

%% gen_bench_a4cpp.escript -- Generate C++ benchmark from ActorForth word definitions
%%
%% Compiles the same words used in bench_ops.escript to C++ via af_cpp_compiler,
%% then wraps them in a benchmark harness matching the other bench_ops_*.* files.
%%
%% Usage:
%%   escript test/demos/comprehensive/gen_bench_a4cpp.escript
%%   # writes bench_a4cpp.cpp to the same directory, then compiles and runs it

-include_lib("actorforth/include/token.hrl").
-include_lib("actorforth/include/continuation.hrl").

main(_) ->
    init_types(),

    %% Define the same words as bench_ops.escript
    C0 = af_interpreter:new_continuation(),
    C1 = eval(": square Int -> Int ; dup * .", C0),
    C2 = eval(": cube Int -> Int ; dup dup * * .", C1),
    C3 = eval(": arith-chain Int -> Int ; dup square swap cube + abs .", C2),
    C4 = eval(": str-work String -> String ; trim to-upper .", C3),
    C5 = eval("type Vec3 x Int y Int z Int .", C4),
    C6 = eval(": vec-mag2 Vec3 -> Vec3 Int ; x square swap y square rot + swap z square rot + .", C5),
    C7 = eval(": compose-test Int -> Int ; dup square swap cube max abs .", C6),
    _C8 = C7,

    %% Gather compiled words and product types
    WordDefs = af_cpp_compiler:find_all_compiled_words(),
    ProductTypes = af_cpp_compiler:find_all_product_types(),

    %% Generate the C++ library code
    CppLib = af_cpp_compiler:generate_cpp("a4_bench", #{words => WordDefs, types => ProductTypes}),

    %% Write the benchmark harness
    ScriptDir = filename:dirname(escript:script_name()),
    OutFile = filename:join(ScriptDir, "bench_a4cpp.cpp"),

    BenchCode = unicode:characters_to_binary(generate_benchmark(CppLib)),
    ok = file:write_file(OutFile, BenchCode),
    io:format("Generated: ~s~n", [OutFile]),

    %% Compile it
    Binary = filename:join(ScriptDir, "bench_a4cpp"),
    CompileCmd = lists:flatten(io_lib:format(
        "g++ -std=c++20 -O2 -o ~s ~s 2>&1", [Binary, OutFile])),
    case os:cmd(CompileCmd) of
        "" ->
            io:format("Compiled: ~s~n", [Binary]),
            %% Run it
            io:format("~n"),
            RunResult = os:cmd(Binary),
            io:format("~s~n", [RunResult]);
        Errors ->
            io:format("Compilation failed:~n~s~n", [Errors])
    end.

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
    af_type_tuple:init(),
    af_type_beam:init(),
    af_type_otp:init(),
    af_cpp_compiler:init().

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "bench"),
    af_interpreter:interpret_tokens(Tokens, Cont).

generate_benchmark(CppLib) ->
    [
        "// bench_a4cpp.cpp -- Generated from ActorForth via af_cpp_compiler\n"
        "//\n"
        "// This is the SAME A4 code as bench_ops.escript, compiled to C++.\n"
        "// It tests the quality of af_cpp_compiler's code generation.\n"
        "//\n"
        "// DO NOT EDIT -- regenerate with:\n"
        "//   escript test/demos/comprehensive/gen_bench_a4cpp.escript\n\n"
        "#define AF_NO_MAIN 1\n\n",

        %% Embed the generated A4 library
        "// === BEGIN GENERATED A4 CODE ===\n",
        CppLib,
        "// === END GENERATED A4 CODE ===\n\n",

        %% Benchmark harness
        "#include <algorithm>\n"
        "#include <chrono>\n"
        "#include <cstdio>\n"
        "#include <functional>\n"
        "#include <vector>\n\n"
        "static const int N = 100000;\n\n"

        "double bench(const char* label, std::function<void()> func, int n_iters = 5) {\n"
        "    func(); // warmup\n"
        "    std::vector<double> times;\n"
        "    times.reserve(n_iters);\n"
        "    for (int i = 0; i < n_iters; ++i) {\n"
        "        auto start = std::chrono::steady_clock::now();\n"
        "        func();\n"
        "        auto end = std::chrono::steady_clock::now();\n"
        "        double us = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count() / 1000.0;\n"
        "        times.push_back(us);\n"
        "    }\n"
        "    std::sort(times.begin(), times.end());\n"
        "    double median = times[n_iters / 2];\n"
        "    double per_op = median / N;\n"
        "    printf(\"%s: %8.1f ms  (%6.3f us/op)\\n\", label, median / 1000.0, per_op);\n"
        "    return median;\n"
        "}\n\n"

        %% Arithmetic: arith_chain (compiled word)
        "void run_arith(int n) {\n"
        "    using namespace af;\n"
        "    for (int i = 1; i <= n; ++i) {\n"
        "        Stack s;\n"
        "        push(s, Value::make_int(i));\n"
        "        arith_chain(s);\n"
        "        pop(s);\n"
        "    }\n"
        "}\n\n"

        %% List ops: nil 1 cons 2 cons 3 cons 4 cons 5 cons reverse
        %%           nil 10 cons 20 cons append length drop
        %% Same sequence as bench_ops.escript ListTokens
        "void run_list_ops(int n) {\n"
        "    using namespace af;\n"
        "    for (int i = 0; i < n; ++i) {\n"
        "        Stack s;\n"
        "        // nil 1 cons 2 cons 3 cons 4 cons 5 cons\n"
        "        push(s, Value::make_list({}));\n"
        "        push(s, Value::make_int(1));\n"
        "        { auto item = pop(s); auto lst = pop(s);\n"
        "          auto vec = lst.as_list();\n"
        "          vec.insert(vec.begin(), std::move(item));\n"
        "          push(s, Value::make_list(vec)); }\n"
        "        push(s, Value::make_int(2));\n"
        "        { auto item = pop(s); auto lst = pop(s);\n"
        "          auto vec = lst.as_list();\n"
        "          vec.insert(vec.begin(), std::move(item));\n"
        "          push(s, Value::make_list(vec)); }\n"
        "        push(s, Value::make_int(3));\n"
        "        { auto item = pop(s); auto lst = pop(s);\n"
        "          auto vec = lst.as_list();\n"
        "          vec.insert(vec.begin(), std::move(item));\n"
        "          push(s, Value::make_list(vec)); }\n"
        "        push(s, Value::make_int(4));\n"
        "        { auto item = pop(s); auto lst = pop(s);\n"
        "          auto vec = lst.as_list();\n"
        "          vec.insert(vec.begin(), std::move(item));\n"
        "          push(s, Value::make_list(vec)); }\n"
        "        push(s, Value::make_int(5));\n"
        "        { auto item = pop(s); auto lst = pop(s);\n"
        "          auto vec = lst.as_list();\n"
        "          vec.insert(vec.begin(), std::move(item));\n"
        "          push(s, Value::make_list(vec)); }\n"
        "        // reverse\n"
        "        { auto lst = pop(s);\n"
        "          auto vec = lst.as_list();\n"
        "          std::reverse(vec.begin(), vec.end());\n"
        "          push(s, Value::make_list(vec)); }\n"
        "        // nil 10 cons 20 cons\n"
        "        push(s, Value::make_list({}));\n"
        "        push(s, Value::make_int(10));\n"
        "        { auto item = pop(s); auto lst = pop(s);\n"
        "          auto vec = lst.as_list();\n"
        "          vec.insert(vec.begin(), std::move(item));\n"
        "          push(s, Value::make_list(vec)); }\n"
        "        push(s, Value::make_int(20));\n"
        "        { auto item = pop(s); auto lst = pop(s);\n"
        "          auto vec = lst.as_list();\n"
        "          vec.insert(vec.begin(), std::move(item));\n"
        "          push(s, Value::make_list(vec)); }\n"
        "        // append\n"
        "        { auto a = pop(s); auto b = pop(s);\n"
        "          auto vec = b.as_list();\n"
        "          auto& avec = a.as_list();\n"
        "          vec.insert(vec.end(), avec.begin(), avec.end());\n"
        "          push(s, Value::make_list(vec)); }\n"
        "        // length drop\n"
        "        { auto v = pop(s); push(s, Value::make_int(v.as_list().size())); }\n"
        "        pop(s);\n"
        "    }\n"
        "}\n\n"

        %% String ops: str_work (compiled word)
        "void run_string_ops(int n) {\n"
        "    using namespace af;\n"
        "    for (int i = 0; i < n; ++i) {\n"
        "        Stack s;\n"
        "        push(s, Value::make_string(\"  hello world  \"));\n"
        "        str_work(s);\n"
        "        pop(s);\n"
        "    }\n"
        "}\n\n"

        %% Map ops: map-new 1 "a" map-put 2 "b" map-put 3 "c" map-put
        %%          map-keys length drop
        %% Same sequence as bench_ops.escript MapTokens
        "void run_map_ops(int n) {\n"
        "    using namespace af;\n"
        "    for (int i = 0; i < n; ++i) {\n"
        "        Stack s;\n"
        "        // map-new\n"
        "        push(s, Value::make_map());\n"
        "        // 1 \"a\" map-put\n"
        "        push(s, Value::make_int(1));\n"
        "        push(s, Value::make_string(\"a\"));\n"
        "        { auto key = pop(s); auto val = pop(s); auto m = pop(s);\n"
        "          auto entries = m.as_fields();\n"
        "          entries.push_back({key.as_string(), val});\n"
        "          push(s, Value::make_map(entries)); }\n"
        "        // 2 \"b\" map-put\n"
        "        push(s, Value::make_int(2));\n"
        "        push(s, Value::make_string(\"b\"));\n"
        "        { auto key = pop(s); auto val = pop(s); auto m = pop(s);\n"
        "          auto entries = m.as_fields();\n"
        "          entries.push_back({key.as_string(), val});\n"
        "          push(s, Value::make_map(entries)); }\n"
        "        // 3 \"c\" map-put\n"
        "        push(s, Value::make_int(3));\n"
        "        push(s, Value::make_string(\"c\"));\n"
        "        { auto key = pop(s); auto val = pop(s); auto m = pop(s);\n"
        "          auto entries = m.as_fields();\n"
        "          entries.push_back({key.as_string(), val});\n"
        "          push(s, Value::make_map(entries)); }\n"
        "        // map-keys\n"
        "        { auto m = pop(s);\n"
        "          std::vector<Value> keys;\n"
        "          for (auto& [k, v] : m.as_fields())\n"
        "              keys.push_back(Value::make_string(k));\n"
        "          push(s, Value::make_list(keys)); }\n"
        "        // length drop\n"
        "        { auto v = pop(s); push(s, Value::make_int(v.as_list().size())); }\n"
        "        pop(s);\n"
        "    }\n"
        "}\n\n"

        %% Product type ops: vec3 vec-mag2 (compiled words)
        "void run_product_ops(int n) {\n"
        "    using namespace af;\n"
        "    for (int i = 1; i <= n; ++i) {\n"
        "        Stack s;\n"
        "        push(s, Value::make_int(i));\n"
        "        push(s, Value::make_int(i + 1));\n"
        "        push(s, Value::make_int(i + 2));\n"
        "        make_vec3_push(s);\n"
        "        vec_mag2(s);\n"
        "        pop(s); pop(s);\n"
        "    }\n"
        "}\n\n"

        %% Composed word chain: compose_test (compiled word)
        "void run_composed(int n) {\n"
        "    using namespace af;\n"
        "    for (int i = 1; i <= n; ++i) {\n"
        "        Stack s;\n"
        "        push(s, Value::make_int(i));\n"
        "        compose_test(s);\n"
        "        pop(s);\n"
        "    }\n"
        "}\n\n"

        "int main() {\n"
        "    printf(\"\\n=== A4->C++ Generated Operations Benchmark ===\\n\");\n"
        "    printf(\"    (ActorForth words compiled to C++ via af_cpp_compiler)\\n\\n\");\n"
        "    printf(\"--- A4->C++ -O2 (%d iterations, 5 timed runs, median) ---\\n\", N);\n\n"

        "    double t_arith = bench(\"  Arithmetic chain\",    [&]{ run_arith(N); });\n"
        "    double t_list  = bench(\"  List ops\",            [&]{ run_list_ops(N); });\n"
        "    double t_str   = bench(\"  String ops\",          [&]{ run_string_ops(N); });\n"
        "    double t_map   = bench(\"  Map ops\",             [&]{ run_map_ops(N); });\n"
        "    double t_prod  = bench(\"  Product type ops\",    [&]{ run_product_ops(N); });\n"
        "    double t_comp  = bench(\"  Compiled word chain\", [&]{ run_composed(N); });\n\n"

        "    double total = t_arith + t_list + t_str + t_map + t_prod + t_comp;\n"
        "    printf(\"\\n  TOTAL: %8.1f ms  (%6.3f us/op)\\n\\n\", total / 1000.0, total / N);\n\n"

        "    printf(\"BENCH_DATA: arith=%.3f list=%.3f string=%.3f map=%.3f product=%.3f wordchain=%.3f\\n\",\n"
        "        t_arith / N, t_list / N, t_str / N, t_map / N, t_prod / N, t_comp / N);\n\n"

        "    return 0;\n"
        "}\n"
    ].
