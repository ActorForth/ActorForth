/// bench_ops_cpp20.cpp — Comprehensive operations benchmark (C++20 equivalent)
///
/// Performs the same computational work as bench_ops.escript using native C++,
/// for cross-language performance comparison against A4 interpreted and compiled modes.
///
/// Reports microseconds per operation for direct cross-language comparison.
///
/// COMPILE & RUN
/// =============
///   g++ -std=c++20 -O2 -o bench_ops bench_ops_cpp20.cpp
///   ./bench_ops
///   ./bench_ops --bench 10   # more timed runs for stable results

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <map>
#include <numeric>
#include <string>
#include <vector>

static const int N = 100000;

// --- Arithmetic operations ---

inline int square(int x) { return x * x; }
inline int cube(int x) { return x * x * x; }
inline int arith_chain(int x) { return std::abs(square(x) + cube(x)); }

// --- List operations ---

inline int list_ops() {
    std::vector<int> lst = {1, 2, 3, 4, 5};
    std::reverse(lst.begin(), lst.end());
    lst.push_back(10);
    lst.push_back(20);
    return static_cast<int>(lst.size());
}

// --- String operations ---

static std::string trim(const std::string& s) {
    auto start = s.find_first_not_of(" \t\n\r");
    if (start == std::string::npos) return "";
    auto end = s.find_last_not_of(" \t\n\r");
    return s.substr(start, end - start + 1);
}

static std::string to_upper(const std::string& s) {
    std::string result = s;
    std::transform(result.begin(), result.end(), result.begin(), ::toupper);
    return result;
}

static std::string str_work(const std::string& s) {
    return to_upper(trim(s));
}

inline std::string string_ops() {
    return str_work("  hello world  ");
}

// --- Map operations ---

inline int map_ops() {
    std::map<std::string, int> m;
    m["a"] = 1;
    m["b"] = 2;
    m["c"] = 3;
    return static_cast<int>(m.size());
}

// --- Product type (Vec3) operations ---

struct Vec3 {
    int x, y, z;
};

inline int vec_mag2(const Vec3& v) {
    return square(v.x) + square(v.y) + square(v.z);
}

// --- Composed word chain ---

inline int compose_test(int x) {
    return std::abs(std::max(square(x), cube(x)));
}

// --- Benchmark harness ---

double bench(const std::string& label, std::function<void()> func, int n_iters = 5) {
    // Warmup
    func();

    std::vector<double> times;
    times.reserve(n_iters);

    for (int i = 0; i < n_iters; ++i) {
        auto start = std::chrono::steady_clock::now();
        func();
        auto end = std::chrono::steady_clock::now();
        double us = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count() / 1000.0;
        times.push_back(us);
    }

    std::sort(times.begin(), times.end());
    double median = times[n_iters / 2];
    double per_op = median / N;
    printf("%s: %8.1f ms  (%6.3f us/op)\n", label.c_str(), median / 1000.0, per_op);
    return median;
}

void run_arith(int n) {
    volatile int sink;
    for (int i = 1; i <= n; ++i) {
        sink = arith_chain(i);
    }
    (void)sink;
}

void run_list_ops(int n) {
    volatile int sink;
    for (int i = 0; i < n; ++i) {
        sink = list_ops();
    }
    (void)sink;
}

void run_string_ops(int n) {
    volatile std::string::size_type sink;
    for (int i = 0; i < n; ++i) {
        auto s = string_ops();
        sink = s.size();
    }
    (void)sink;
}

void run_map_ops(int n) {
    volatile int sink;
    for (int i = 0; i < n; ++i) {
        sink = map_ops();
    }
    (void)sink;
}

void run_product_ops(int n) {
    volatile int sink;
    for (int i = 1; i <= n; ++i) {
        sink = vec_mag2(Vec3{i, i + 1, i + 2});
    }
    (void)sink;
}

void run_compiled_words(int n) {
    volatile int sink;
    for (int i = 1; i <= n; ++i) {
        sink = compose_test(i);
    }
    (void)sink;
}

int main(int argc, char* argv[]) {
    int n_iters = 5;
    if (argc >= 3 && std::string(argv[1]) == "--bench") {
        n_iters = std::stoi(argv[2]);
    }

    printf("\n=== C++20 Native Operations Benchmark ===\n\n");
    printf("--- Native C++20 -O2 (%d iterations, %d timed runs, median) ---\n", N, n_iters);

    double t_arith = bench("  Arithmetic chain", [&]{ run_arith(N); }, n_iters);
    double t_list  = bench("  List ops",         [&]{ run_list_ops(N); }, n_iters);
    double t_str   = bench("  String ops",       [&]{ run_string_ops(N); }, n_iters);
    double t_map   = bench("  Map ops",          [&]{ run_map_ops(N); }, n_iters);
    double t_prod  = bench("  Product type ops", [&]{ run_product_ops(N); }, n_iters);
    double t_comp  = bench("  Compiled word chain", [&]{ run_compiled_words(N); }, n_iters);

    double total = t_arith + t_list + t_str + t_map + t_prod + t_comp;
    printf("\n  TOTAL: %8.1f ms  (%6.3f us/op)\n\n", total / 1000.0, total / N);

    // Machine-readable summary for cross-language comparison
    printf("BENCH_DATA: arith=%.3f list=%.3f string=%.3f map=%.3f product=%.3f wordchain=%.3f\n",
        t_arith / N, t_list / N, t_str / N, t_map / N, t_prod / N, t_comp / N);

    return 0;
}
