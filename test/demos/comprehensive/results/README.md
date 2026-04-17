# Benchmark result history

Each JSON file here is a saved cross-language benchmark run. Files are named
`YYYY-MM-DD.json` (with a `-HHMMSS` suffix if multiple runs happen in one
day). The most recent file by filename is treated as the current baseline.

## Run and compare

```bash
# Run the suite, show a diff against the latest baseline, do not save.
python3 test/demos/comprehensive/compare_benchmarks.py

# Run, diff, and save the new result as today's baseline.
python3 test/demos/comprehensive/compare_benchmarks.py --save

# Quick mode (skips the slow A4-interpreted paths — pass through).
python3 test/demos/comprehensive/compare_benchmarks.py --quick

# Diff against a specific prior file instead of the latest.
python3 test/demos/comprehensive/compare_benchmarks.py \
    --baseline test/demos/comprehensive/results/2026-04-17.json
```

## When to save a new baseline

After an interesting performance-relevant change:

- A new benchmark sample lands (extend `CATEGORIES` in the runner first)
- A compilation backend change (e.g. Core Erlang target feature parity)
- A hot-path refactor (e.g. stack unboxing)
- A dependency upgrade that touches VM/runtime versions

Don't save on every commit — this history is for "something worth looking at"
moments, not a per-commit log.

## JSON shape

```json
{
  "date":    "2026-04-17T09:35:42Z",
  "git_sha": "6ff5b13",
  "quick":   true,
  "results": {
    "cpp":       { "arith": 0.0, "list": 0.023, ... },
    "a4_native": { "arith": 11.002, ... },
    ...
  }
}
```

Values are microseconds per operation. `null` / missing category means
that language's benchmark didn't produce data for that category.
