#!/usr/bin/env python3
"""
run_all_benchmarks.py — Run all comprehensive benchmarks and produce comparison table.

Executes benchmarks in C++, Python, Erlang, Elixir, and ActorForth (interpreted,
native BEAM, direct BEAM), then parses the BENCH_DATA output lines and produces
a formatted comparison table with us/op values.

Usage:
    python3 run_all_benchmarks.py           # from project root
    python3 run_all_benchmarks.py --quick   # skip slow benchmarks (A4 interpreted)
"""

import subprocess
import sys
import os
import re
import shutil
import tempfile

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, "..", "..", ".."))

CATEGORIES = ["arith", "list", "string", "map", "product", "wordchain"]
CATEGORY_LABELS = {
    "arith": "Arithmetic",
    "list": "List ops",
    "string": "String ops",
    "map": "Map ops",
    "product": "Product type",
    "wordchain": "Word chain",
}


def run_cmd(cmd, label, cwd=None, timeout=600):
    """Run a command, print its output live, return stdout."""
    print(f"\n{'='*60}")
    print(f"  Running: {label}")
    print(f"  Command: {' '.join(cmd)}")
    print(f"{'='*60}\n")

    try:
        result = subprocess.run(
            cmd, capture_output=True, text=True, cwd=cwd or PROJECT_ROOT, timeout=timeout
        )
        # Print output live
        if result.stdout:
            print(result.stdout)
        if result.returncode != 0 and result.stderr:
            print(f"STDERR: {result.stderr}", file=sys.stderr)
        return result.stdout, result.returncode == 0
    except FileNotFoundError:
        print(f"  SKIPPED: {cmd[0]} not found")
        return "", False
    except subprocess.TimeoutExpired:
        print(f"  TIMEOUT after {timeout}s")
        return "", False


def parse_bench_data(output, tag=None):
    """Parse BENCH_DATA lines from benchmark output.

    If tag is None, matches 'BENCH_DATA: ...'
    If tag is given, matches 'BENCH_DATA[tag]: ...'
    """
    if tag:
        pattern = rf"^BENCH_DATA\[{re.escape(tag)}\]:\s*(.+)$"
    else:
        pattern = r"^BENCH_DATA:\s*(.+)$"

    for line in output.splitlines():
        m = re.match(pattern, line.strip())
        if m:
            data = {}
            for pair in m.group(1).split():
                key, val = pair.split("=", 1)
                data[key] = None if val == "-" else float(val)
            return data
    return None


def compile_cpp():
    """Compile the C++ benchmark, return path to binary."""
    src = os.path.join(SCRIPT_DIR, "bench_ops_cpp20.cpp")
    binary = os.path.join(SCRIPT_DIR, "bench_ops_cpp20")

    # Check if g++ is available
    if not shutil.which("g++"):
        return None

    result = subprocess.run(
        ["g++", "-std=c++20", "-O2", "-o", binary, src],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        print(f"C++ compilation failed: {result.stderr}")
        return None
    return binary


def ensure_a4_built():
    """Make sure rebar3 compile has been run."""
    result = subprocess.run(
        ["rebar3", "compile"],
        capture_output=True, text=True, cwd=PROJECT_ROOT
    )
    return result.returncode == 0


def format_val(val):
    """Format a us/op value for the table."""
    if val is None:
        return "—"
    if val < 0.001:
        return "<0.001"
    if val < 1.0:
        return f"{val:.3f}"
    if val < 100.0:
        return f"{val:.1f}"
    return f"{val:.0f}"


def print_table(results):
    """Print the comparison table."""
    # Column order (left to right, fastest to slowest expected)
    columns = [
        ("C++ -O2", "cpp"),
        ("Elixir", "elixir"),
        ("Erlang", "erlang"),
        ("Python", "python"),
        ("A4 Direct", "a4_direct"),
        ("A4 Native", "a4_native"),
        ("A4 Interp", "a4_interp"),
    ]

    # Filter to only columns that have data
    active_cols = [(label, key) for label, key in columns if key in results]

    if not active_cols:
        print("\nNo benchmark data collected!")
        return

    # Calculate column widths
    cat_width = max(len(v) for v in CATEGORY_LABELS.values()) + 2
    col_width = max(max(len(label) for label, _ in active_cols), 8) + 2

    # Header
    print()
    print("=" * 70)
    print("  CROSS-LANGUAGE COMPREHENSIVE OPERATIONS BENCHMARK")
    print("  All values in microseconds per operation (us/op) — lower is better")
    print("=" * 70)
    print()

    # Table header
    header = f"{'Operation':<{cat_width}}"
    for label, _ in active_cols:
        header += f" | {label:>{col_width}}"
    print(header)
    header_sep = "-" * cat_width
    for _ in active_cols:
        header_sep += "-+-" + "-" * col_width
    print(header_sep)

    # Data rows
    totals = {key: 0.0 for _, key in active_cols}
    total_counts = {key: 0 for _, key in active_cols}

    for cat in CATEGORIES:
        row = f"{CATEGORY_LABELS[cat]:<{cat_width}}"
        for _, key in active_cols:
            data = results.get(key, {})
            val = data.get(cat) if data else None
            row += f" | {format_val(val):>{col_width}}"
            if val is not None:
                totals[key] += val
                total_counts[key] += 1
        print(row)

    # Total row
    sep = "-" * cat_width
    for _ in active_cols:
        sep += "-+-" + "-" * col_width
    print(sep)
    row = f"{'TOTAL':<{cat_width}}"
    for _, key in active_cols:
        val = totals[key] if total_counts[key] == len(CATEGORIES) else None
        row += f" | {format_val(val):>{col_width}}"
    print(row)
    print()

    # Speedup summary
    if "a4_interp" in results and "a4_native" in results:
        print("Speedup (interpreted -> native BEAM):")
        for cat in CATEGORIES:
            interp = results["a4_interp"].get(cat)
            native = results["a4_native"].get(cat)
            if interp and native and native > 0:
                print(f"  {CATEGORY_LABELS[cat]:<15} {interp/native:.1f}x")
        print()

    if "a4_native" in results:
        # Compare to closest native language (Erlang)
        ref_key = "erlang" if "erlang" in results else "elixir" if "elixir" in results else None
        if ref_key:
            ref_label = "Erlang" if ref_key == "erlang" else "Elixir"
            print(f"Overhead vs native {ref_label} (A4 native BEAM):")
            for cat in CATEGORIES:
                native = results["a4_native"].get(cat)
                ref = results[ref_key].get(cat)
                if native and ref and ref > 0:
                    print(f"  {CATEGORY_LABELS[cat]:<15} {native/ref:.0f}x")
            print()


def main():
    quick = "--quick" in sys.argv

    print("\n" + "=" * 60)
    print("  ActorForth Cross-Language Benchmark Suite")
    print("=" * 60)

    results = {}

    # 1. Compile and run C++
    cpp_binary = compile_cpp()
    if cpp_binary:
        out, ok = run_cmd([cpp_binary], "C++20 -O2")
        if ok:
            data = parse_bench_data(out)
            if data:
                results["cpp"] = data
        # Clean up binary
        try:
            os.remove(cpp_binary)
        except OSError:
            pass

    # 2. Python
    out, ok = run_cmd(
        [sys.executable, os.path.join(SCRIPT_DIR, "bench_ops_python.py")],
        "Python (native)"
    )
    if ok:
        data = parse_bench_data(out)
        if data:
            results["python"] = data

    # 3. Erlang
    if shutil.which("escript"):
        out, ok = run_cmd(
            ["escript", os.path.join(SCRIPT_DIR, "bench_ops_erlang.escript")],
            "Erlang (native)"
        )
        if ok:
            data = parse_bench_data(out)
            if data:
                results["erlang"] = data

    # 4. Elixir
    if shutil.which("elixir"):
        out, ok = run_cmd(
            ["elixir", os.path.join(SCRIPT_DIR, "bench_ops_elixir.exs")],
            "Elixir (native)"
        )
        if ok:
            data = parse_bench_data(out)
            if data:
                results["elixir"] = data

    # 5. ActorForth (requires rebar3 compile first)
    if ensure_a4_built():
        timeout = 120 if quick else 600
        out, ok = run_cmd(
            [os.path.join(SCRIPT_DIR, "bench_ops.escript")],
            "ActorForth (interpreted / native / direct)",
            timeout=timeout
        )
        if ok:
            for tag, key in [("interp", "a4_interp"), ("native", "a4_native"), ("direct", "a4_direct")]:
                data = parse_bench_data(out, tag)
                if data:
                    results[key] = data

    # Print the comparison table
    print_table(results)


if __name__ == "__main__":
    main()
