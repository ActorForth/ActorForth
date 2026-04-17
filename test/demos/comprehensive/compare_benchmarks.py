#!/usr/bin/env python3
"""
compare_benchmarks.py — Run the benchmark suite and diff against the last
saved baseline in results/.

Usage:
    python3 compare_benchmarks.py                # run, compare, do not save
    python3 compare_benchmarks.py --save         # run, compare, save new JSON
    python3 compare_benchmarks.py --quick        # pass through to runner
    python3 compare_benchmarks.py --baseline FILE  # compare against specific file
"""

import os
import sys
import json
import glob
import subprocess
import tempfile
import datetime

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
RESULTS_DIR = os.path.join(SCRIPT_DIR, "results")

CATEGORIES = ["arith", "list", "string", "map", "product", "wordchain"]
CATEGORY_LABELS = {
    "arith": "Arithmetic",
    "list": "List ops",
    "string": "String ops",
    "map": "Map ops",
    "product": "Product type",
    "wordchain": "Word chain",
}

# Languages in roughly "fastest expected" order — same as the runner
LANGUAGE_ORDER = [
    ("cpp", "C++ -O2"),
    ("a4_cpp", "A4->C++"),
    ("ts_native", "TS Native"),
    ("elixir", "Elixir"),
    ("erlang", "Erlang"),
    ("python", "Python"),
    ("a4_direct", "A4 Direct"),
    ("a4_native", "A4 Native"),
    ("a4_interp", "A4 Interp"),
    ("ts_interp", "A4/TS Interp"),
]


def parse_args(argv):
    opts = {"save": False, "quick": False, "baseline": None}
    i = 1
    while i < len(argv):
        a = argv[i]
        if a == "--save":
            opts["save"] = True
        elif a == "--quick":
            opts["quick"] = True
        elif a == "--baseline":
            i += 1
            opts["baseline"] = argv[i]
        elif a in ("--help", "-h"):
            print(__doc__)
            sys.exit(0)
        i += 1
    return opts


def find_latest_baseline():
    """Find the most recent results/*.json file by name (dates sort lexically)."""
    files = sorted(glob.glob(os.path.join(RESULTS_DIR, "*.json")))
    return files[-1] if files else None


def run_benchmark(quick):
    """Run the benchmark, write structured results to a temp JSON, return parsed data."""
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".json", delete=False, dir=SCRIPT_DIR
    ) as f:
        tmp = f.name
    try:
        cmd = [sys.executable, os.path.join(SCRIPT_DIR, "run_all_benchmarks.py")]
        if quick:
            cmd.append("--quick")
        cmd.extend(["--json-out", tmp])
        rc = subprocess.call(cmd)
        if rc != 0:
            print(f"Benchmark runner exited with status {rc}", file=sys.stderr)
        with open(tmp) as f:
            return json.load(f)
    finally:
        try:
            os.remove(tmp)
        except OSError:
            pass


def format_us(val):
    if val is None:
        return "—"
    if val < 0.001:
        return "<0.001"
    if val < 1.0:
        return f"{val:.3f}"
    if val < 100.0:
        return f"{val:.1f}"
    return f"{val:.0f}"


def format_delta(prev, now):
    """Return a (string, is_regression) pair for a side-by-side cell."""
    if prev is None or now is None:
        return "—", False
    if prev == 0:
        return "—", False
    pct = (now - prev) / prev * 100.0
    if abs(pct) < 1.0:
        return "  ≈", False
    sign = "+" if pct >= 0 else ""
    return f"{sign}{pct:.0f}%", pct > 5.0


def print_diff_table(prev_payload, now_payload):
    prev = prev_payload.get("results", {})
    now = now_payload.get("results", {})

    print()
    print("=" * 78)
    print("  BENCHMARK DIFF")
    print("=" * 78)
    print(f"  prev : {prev_payload.get('date', '?')}  "
          f"(sha {prev_payload.get('git_sha', '?')})")
    print(f"  now  : {now_payload.get('date', '?')}  "
          f"(sha {now_payload.get('git_sha', '?')})")
    print("=" * 78)
    print()

    langs = [(k, l) for k, l in LANGUAGE_ORDER if k in prev or k in now]
    if not langs:
        print("No overlapping data between runs.")
        return

    # One section per language — easier to read than a huge table.
    regressions = []
    for key, label in langs:
        p = prev.get(key, {})
        n = now.get(key, {})
        if not p and not n:
            continue
        print(f"  {label}")
        print(f"    {'Operation':<15} {'prev':>10} {'now':>10} {'delta':>8}")
        for cat in CATEGORIES:
            pv = p.get(cat)
            nv = n.get(cat)
            d, regressed = format_delta(pv, nv)
            print(f"    {CATEGORY_LABELS[cat]:<15} "
                  f"{format_us(pv):>10} {format_us(nv):>10} {d:>8}")
            if regressed:
                regressions.append((label, CATEGORY_LABELS[cat], pv, nv))
        print()

    if regressions:
        print("  Regressions (>5% slower):")
        for lang, cat, pv, nv in regressions:
            print(f"    - {lang:<12} {cat:<15}  "
                  f"{format_us(pv)} -> {format_us(nv)} us/op")
    else:
        print("  No regressions beyond ±5% noise floor.")
    print()


def main():
    opts = parse_args(sys.argv)

    os.makedirs(RESULTS_DIR, exist_ok=True)

    baseline_path = opts["baseline"] or find_latest_baseline()
    if baseline_path:
        with open(baseline_path) as f:
            baseline = json.load(f)
        print(f"Comparing against baseline: {baseline_path}")
    else:
        baseline = None
        print("No prior baseline found. Current run will be the first.")

    now_payload = run_benchmark(opts["quick"])

    if baseline:
        print_diff_table(baseline, now_payload)
    else:
        print("\nNo diff to show — this will become the baseline.")

    if opts["save"]:
        date = datetime.date.today().isoformat()
        out_path = os.path.join(RESULTS_DIR, f"{date}.json")
        # If today's file already exists, append a time suffix so we don't
        # silently overwrite.
        if os.path.exists(out_path):
            stamp = datetime.datetime.now().strftime("%H%M%S")
            out_path = os.path.join(RESULTS_DIR, f"{date}-{stamp}.json")
        with open(out_path, "w") as f:
            json.dump(now_payload, f, indent=2, sort_keys=True)
            f.write("\n")
        print(f"Saved: {out_path}")


if __name__ == "__main__":
    main()
