"""
bench_ops_python.py — Comprehensive operations benchmark (Python equivalent)

Performs the same computational work as bench_ops.escript using native Python,
for cross-language performance comparison against A4 interpreted and compiled modes.

Reports microseconds per operation for direct cross-language comparison.

Usage:
    python3 bench_ops_python.py
    python3 bench_ops_python.py --bench 10   # more timed runs for stable results
"""

import time
import statistics
import sys
from dataclasses import dataclass

N = 100000

# --- Arithmetic operations ---

def square(x: int) -> int:
    return x * x

def cube(x: int) -> int:
    return x * x * x

def arith_chain(x: int) -> int:
    return abs(square(x) + cube(x))


# --- List operations ---

def list_ops():
    lst = [1, 2, 3, 4, 5]
    lst = list(reversed(lst))
    lst = lst + [10, 20]
    return len(lst)


# --- String operations ---

def str_work(s: str) -> str:
    return s.strip().upper()

def string_ops():
    return str_work("  hello world  ")


# --- Map operations ---

def map_ops():
    m = {}
    m["a"] = 1
    m["b"] = 2
    m["c"] = 3
    return len(list(m.keys()))


# --- Product type (Vec3) operations ---

@dataclass
class Vec3:
    x: int
    y: int
    z: int

def vec_mag2(v: Vec3) -> int:
    return square(v.x) + square(v.y) + square(v.z)


# --- Composed word chain ---

def compose_test(x: int) -> int:
    return abs(max(square(x), cube(x)))


# --- Benchmark harness ---

def bench(label: str, func, n_iters: int = 5) -> float:
    """Run func, take median of n_iters timed runs. Returns median in microseconds."""
    # Warmup
    func()

    times = []
    for _ in range(n_iters):
        start = time.perf_counter()
        func()
        elapsed_us = (time.perf_counter() - start) * 1_000_000
        times.append(elapsed_us)

    median = statistics.median(times)
    per_op = median / N
    print(f"{label}: {median / 1000:8.1f} ms  ({per_op:6.3f} us/op)")
    return median


def run_arith(n: int):
    for i in range(1, n + 1):
        arith_chain(i)

def run_list_ops(n: int):
    for _ in range(n):
        list_ops()

def run_string_ops(n: int):
    for _ in range(n):
        string_ops()

def run_map_ops(n: int):
    for _ in range(n):
        map_ops()

def run_product_ops(n: int):
    for i in range(1, n + 1):
        vec_mag2(Vec3(i, i + 1, i + 2))

def run_compiled_words(n: int):
    for i in range(1, n + 1):
        compose_test(i)


def main():
    n_iters = 5
    if len(sys.argv) >= 3 and sys.argv[1] == "--bench":
        n_iters = int(sys.argv[2])

    print()
    print("=== Python Native Operations Benchmark ===")
    print()
    print(f"--- Native Python ({N} iterations, {n_iters} timed runs, median) ---")

    t_arith = bench("  Arithmetic chain", lambda: run_arith(N), n_iters)
    t_list  = bench("  List ops",         lambda: run_list_ops(N), n_iters)
    t_str   = bench("  String ops",       lambda: run_string_ops(N), n_iters)
    t_map   = bench("  Map ops",          lambda: run_map_ops(N), n_iters)
    t_prod  = bench("  Product type ops", lambda: run_product_ops(N), n_iters)
    t_comp  = bench("  Compiled word chain", lambda: run_compiled_words(N), n_iters)

    total = t_arith + t_list + t_str + t_map + t_prod + t_comp
    per_op = total / N
    print(f"\n  TOTAL: {total / 1000:8.1f} ms  ({per_op:6.3f} us/op)")
    print()

    # Machine-readable summary for cross-language comparison
    print(f"BENCH_DATA: arith={t_arith/N:.3f} list={t_list/N:.3f} string={t_str/N:.3f} "
          f"map={t_map/N:.3f} product={t_prod/N:.3f} wordchain={t_comp/N:.3f}")


if __name__ == "__main__":
    main()
