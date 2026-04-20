#!/bin/bash
# run_bench.sh
#
# Orchestrate the HOS elevator throughput comparison.
#
# Runs the a4-on-BEAM bench (via rebar3 eunit), the Python correct
# bench (via python3), and the Rust correct bench (via rustc + ./out/),
# then prints a one-screen comparison table. Raw tool output and the
# final table are also appended to results.txt for reuse in the talk
# backgrounder.
#
# OCaml is skipped gracefully. The correct-reference OCaml elevator
# uses ocamlfind / threads which is not installed on the target
# machine; adding an OCaml bench is future work.
#
# Usage:
#   samples/hos/elevator/bench/run_bench.sh [N]
# where N is the number of requests in the timed run (default 1000).

set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$HERE/../../../.." && pwd)"
RESULTS="$HERE/results.txt"
N="${1:-1000}"

cd "$REPO_ROOT"

: > "$RESULTS"

say() {
    echo "$@"
    echo "$@" >> "$RESULTS"
}

section() {
    say ""
    say "=============================================================="
    say "$@"
    say "=============================================================="
}

# Snapshot stdout AND file; capture output for [BENCH] scraping.
capture() {
    local tmpfile
    tmpfile="$(mktemp)"
    "$@" 2>&1 | tee -a "$tmpfile"
    cat "$tmpfile" >> "$RESULTS"
    # Return the captured file path via the RESULT_FILE env.
    RESULT_FILE="$tmpfile"
}

extract_rate() {
    # Pull events/sec from the last [BENCH] line. Matches:
    #   [BENCH] <label> events/sec=NNNNN.NN cycles=M wall=T.TTTs
    # If a filter string is given in $2, only consider lines that
    # contain it (lets the Erlang harness run both a nowait number
    # and a timered smoke number without confusing the scraper).
    local file="$1"
    local filter="${2:-}"
    local line
    if [ -n "$filter" ]; then
        line="$(grep -E '\[BENCH\]' "$file" | grep -F "$filter" \
                  | tail -n 1)"
    else
        line="$(grep -E '\[BENCH\]' "$file" | tail -n 1)"
    fi
    echo "$line" | sed -E 's/.*events\/sec=([0-9.]+).*/\1/'
}

# ---------- Header ----------

section "HOS elevator throughput comparison (N=$N)"
say ""
say "Workload: one request drives the elevator through a full"
say "assign-move-open-load-close cycle. 16 events per cycle,"
say "counted identically in all three implementations. The Python"
say "and Rust benches start from the existing elevator_correct.*"
say "references with every thread sleep stripped out; state machine,"
say "lock discipline, and validated transitions are unchanged. The"
say "Erlang bench uses the HOS spec with after-0 timers so each"
say "self-send is still one mailbox round trip but no wall-clock"
say "wait."
say ""

BEAM_RATE=""
PY_RATE=""
RS_RATE=""

# ---------- Erlang / BEAM ----------

section "1/3: Erlang HOS (elevator_nowait.a4)"
say ""
if ! command -v rebar3 >/dev/null 2>&1; then
    say "[skip] rebar3 not found on PATH"
else
    capture rebar3 eunit --module=af_hos_bench_tests
    BEAM_RATE="$(extract_rate "$RESULT_FILE")"
fi

# ---------- Python ----------

section "2/3: Python correct (elevator_bench.py)"
say ""
if ! command -v python3 >/dev/null 2>&1; then
    say "[skip] python3 not installed"
else
    capture python3 "$HERE/elevator_bench.py" "$N"
    PY_RATE="$(extract_rate "$RESULT_FILE")"
fi

# ---------- Rust ----------

section "3/3: Rust correct (elevator_bench.rs)"
say ""
if ! command -v rustc >/dev/null 2>&1; then
    say "[skip] rustc not installed"
else
    capture bash "$HERE/run_rust.sh" "$N"
    RS_RATE="$(extract_rate "$RESULT_FILE")"
fi

# ---------- OCaml (skipped) ----------

section "OCaml correct"
say ""
if command -v ocamlfind >/dev/null 2>&1; then
    say "[skip] OCaml bench not implemented yet."
    say "       The comparison reference exists at"
    say "       samples/hos/elevator/comparison/elevator_correct.ml"
    say "       but the bench harness has not been ported."
else
    say "[skip] ocamlfind not installed."
fi

# ---------- Table ----------

section "Summary"
say ""

# Compute ratios relative to Python. If Python is missing, ratios
# display as N/A.
printf_row() {
    # args: label rate ratio
    local label="$1"
    local rate="$2"
    local ratio="$3"
    printf "  %-16s %14s    %s\n" "$label" "$rate" "$ratio" \
        | tee -a "$RESULTS"
}

fmt_int() {
    # Render a float as an integer with thousand separators, or "N/A".
    if [ -z "$1" ]; then
        echo "N/A"
    else
        # shellcheck disable=SC2059
        LC_ALL=C printf "%'d\n" "${1%.*}"
    fi
}

fmt_ratio() {
    # Render ratio = $1 / $2 as Nx, or "N/A" if either missing, or "1x"
    # if they're equal.
    local num="$1"
    local den="$2"
    if [ -z "$num" ] || [ -z "$den" ] || [ "$den" = "0" ]; then
        echo "N/A"
    else
        awk -v n="$num" -v d="$den" 'BEGIN {
            if (d == 0) { print "N/A"; exit }
            r = n / d
            if (r >= 100)      printf "%.0fx\n", r
            else if (r >= 10)  printf "%.1fx\n", r
            else               printf "%.2fx\n", r
        }'
    fi
}

say ""
say "  Language         Events/sec    Ratio vs Python"
say "  ---------------- ------------- ---------------"
printf_row "Erlang HOS" "$(fmt_int "$BEAM_RATE")" \
           "$(fmt_ratio "$BEAM_RATE" "$PY_RATE")"
printf_row "Rust correct" "$(fmt_int "$RS_RATE")" \
           "$(fmt_ratio "$RS_RATE" "$PY_RATE")"
printf_row "Python correct" "$(fmt_int "$PY_RATE")" \
           "$(fmt_ratio "$PY_RATE" "$PY_RATE")"
say ""
say "Raw output written to: $RESULTS"
