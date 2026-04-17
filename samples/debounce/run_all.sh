#!/usr/bin/env bash
#
# run_all.sh — Build, test, and benchmark all debounce demo implementations.
#
# Usage:
#   ./run_all.sh              # run all tests once
#   ./run_all.sh --bench N    # run each test N times IN-PROCESS and report
#                             # timing stats (excludes VM/interpreter startup)
#
# Runs from the ActorForth project root (auto-detected).

set -euo pipefail

# Close stdin so erl/python don't block waiting for input
exec 0</dev/null

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BUILD_DIR="$SCRIPT_DIR/.build"

BENCH_ITERS=0
if [[ "${1:-}" == "--bench" ]]; then
    BENCH_ITERS="${2:-10}"
fi

mkdir -p "$BUILD_DIR"

# Colors (if terminal supports them)
if [[ -t 1 ]]; then
    GREEN='\033[0;32m'
    RED='\033[0;31m'
    CYAN='\033[0;36m'
    BOLD='\033[1m'
    RESET='\033[0m'
else
    GREEN='' RED='' CYAN='' BOLD='' RESET=''
fi

PASS=0
FAIL=0
declare -A BENCH_MIN BENCH_AVG BENCH_MAX BENCH_N

pass() { PASS=$((PASS + 1)); echo -e "  ${GREEN}PASS${RESET}"; }
fail() { FAIL=$((FAIL + 1)); echo -e "  ${RED}FAIL${RESET}"; }

# Format microseconds to human-readable (us, ms, or s)
fmt_us() {
    local us="$1"
    if ((us >= 1000000)); then
        local ms=$((us / 1000))
        local s_int=$((ms / 1000))
        local s_frac=$((ms % 1000))
        printf "%d.%03ds" "$s_int" "$s_frac"
    elif ((us >= 1000)); then
        local ms_int=$((us / 1000))
        local ms_frac=$((us % 1000))
        printf "%d.%03dms" "$ms_int" "$ms_frac"
    else
        printf "%dus" "$us"
    fi
}

# Time a command (wall clock). Sets $ELAPSED_MS.
time_cmd() {
    local start end rc
    start=$(date +%s%N)
    "$@" >/dev/null 2>&1
    rc=$?
    end=$(date +%s%N)
    ELAPSED_MS=$(( (end - start) / 1000000 ))
    return $rc
}

# Extract BENCH_RESULT line from command output
extract_bench() {
    "$@" 2>/dev/null | grep "^BENCH_RESULT:" | head -1
}

# Parse BENCH_RESULT line and store in associative arrays
parse_bench() {
    local lang="$1" line="$2"
    local min avg max iters
    min=$(echo "$line" | sed -n 's/.*min=\([0-9]*\).*/\1/p')
    avg=$(echo "$line" | sed -n 's/.*avg=\([0-9]*\).*/\1/p')
    max=$(echo "$line" | sed -n 's/.*max=\([0-9]*\).*/\1/p')
    iters=$(echo "$line" | sed -n 's/.*iters=\([0-9]*\).*/\1/p')
    BENCH_MIN[$lang]="${min:-0}"
    BENCH_AVG[$lang]="${avg:-0}"
    BENCH_MAX[$lang]="${max:-0}"
    BENCH_N[$lang]="${iters:-0}"
}

# Collect erl lib paths from the rebar3 build
AF_EBIN="$PROJECT_ROOT/_build/default/lib/actorforth/ebin"
PY_EBIN="$PROJECT_ROOT/_build/default/lib/erlang_python/ebin"

echo -e "${BOLD}========================================${RESET}"
echo -e "${BOLD} Debounce Demo — All Implementations${RESET}"
echo -e "${BOLD}========================================${RESET}"
echo

# ------------------------------------------------------------------
# 0. Ensure ActorForth is compiled
# ------------------------------------------------------------------
echo -n "Compiling ActorForth... "
cd "$PROJECT_ROOT"
if rebar3 compile >/dev/null 2>&1; then
    echo "ok."
else
    echo "FAILED — run 'rebar3 compile' manually to see errors."
    exit 1
fi
echo

# Helper: run an ActorForth .a4 file via erl directly
run_a4() {
    erl -noshell -pa "$AF_EBIN" -pa "$PY_EBIN" \
        -eval "try af_repl:run_file(\"$1\"), halt(0) catch _:_ -> halt(1) end."
}

# Helper: benchmark ActorForth in-process (N iterations inside one BEAM)
bench_a4() {
    local n="$1" file="$2"
    erl -noshell -pa "$AF_EBIN" -pa "$PY_EBIN" -eval "
        af_repl:init_types(),
        Times = [begin
            {T, _} = timer:tc(fun() ->
                af_repl:run_file(\"$file\")
            end),
            T
        end || _ <- lists:seq(1, $n)],
        Min = lists:min(Times),
        Max = lists:max(Times),
        Avg = lists:sum(Times) div $n,
        io:format(\"BENCH_RESULT: min=~b avg=~b max=~b iters=~b~n\",
                  [Min, Avg, Max, $n]),
        halt(0).
    "
}

# ------------------------------------------------------------------
# 1. ActorForth
# ------------------------------------------------------------------
echo -e "${CYAN}[1/6] ActorForth${RESET} (debounce_demo.a4)"
echo -n "  Running... "

if time_cmd run_a4 "$SCRIPT_DIR/debounce_demo.a4"; then
    pass
else
    fail
fi

if ((BENCH_ITERS > 0)); then
    echo -n "  Benchmarking ($BENCH_ITERS iterations, in-process)... "
    RESULT=$(extract_bench bench_a4 "$BENCH_ITERS" "$SCRIPT_DIR/debounce_demo.a4")
    parse_bench "ActorForth" "$RESULT"
    echo "done."
fi

# ------------------------------------------------------------------
# 2. Erlang
# ------------------------------------------------------------------
echo -e "${CYAN}[2/6] Erlang/OTP${RESET} (debounce_erlang_equivalent.erl)"
echo -n "  Compiling... "

if erlc -o "$BUILD_DIR" "$SCRIPT_DIR/debounce_erlang_equivalent.erl" 2>/dev/null; then
    echo -n "ok. Running... "
    if time_cmd erl -noshell -pa "$BUILD_DIR" -eval \
        "debounce_erlang_equivalent:start_test(), halt(0)."; then
        pass
    else
        fail
    fi
else
    echo -n "compile failed. "; fail
fi

if ((BENCH_ITERS > 0)); then
    echo -n "  Benchmarking ($BENCH_ITERS iterations, in-process)... "
    RESULT=$(extract_bench erl -noshell -pa "$BUILD_DIR" -eval \
        "debounce_erlang_equivalent:bench($BENCH_ITERS), halt(0).")
    parse_bench "Erlang" "$RESULT"
    echo "done."
fi

# ------------------------------------------------------------------
# 3. Python
# ------------------------------------------------------------------
echo -e "${CYAN}[3/6] Python${RESET} (debounce_python_equivalent.py)"
echo -n "  Running... "

if time_cmd python3 "$SCRIPT_DIR/debounce_python_equivalent.py"; then
    pass
else
    fail
fi

if ((BENCH_ITERS > 0)); then
    echo -n "  Benchmarking ($BENCH_ITERS iterations, in-process)... "
    RESULT=$(extract_bench python3 "$SCRIPT_DIR/debounce_python_equivalent.py" \
        --bench "$BENCH_ITERS")
    parse_bench "Python" "$RESULT"
    echo "done."
fi

# ------------------------------------------------------------------
# 4. C++20
# ------------------------------------------------------------------
echo -e "${CYAN}[4/6] Elixir${RESET} (debounce_elixir_equivalent.exs)"
echo -n "  Running... "
if time_cmd elixir "$SCRIPT_DIR/debounce_elixir_equivalent.exs"; then
    pass
else
    fail
fi

# ------------------------------------------------------------------
# 5. TypeScript
# ------------------------------------------------------------------
echo -e "${CYAN}[5/6] TypeScript${RESET} (debounce_typescript_equivalent.ts)"
echo -n "  Running... "
if time_cmd bash -c "cd '$PROJECT_ROOT/ts' && npx tsx '$SCRIPT_DIR/debounce_typescript_equivalent.ts'"; then
    pass
else
    fail
fi

# ------------------------------------------------------------------
# 6. C++20
# ------------------------------------------------------------------
echo -e "${CYAN}[6/6] C++20${RESET} (debounce_cpp20_equivalent.cpp)"
echo -n "  Compiling... "

CPP_BIN="$BUILD_DIR/debounce_cpp20"
if g++ -std=c++20 -O2 -pthread -o "$CPP_BIN" \
    "$SCRIPT_DIR/debounce_cpp20_equivalent.cpp" 2>/dev/null; then
    echo -n "ok. Running... "
    if time_cmd "$CPP_BIN"; then
        pass
    else
        fail
    fi
else
    echo -n "compile failed. "; fail
fi

if ((BENCH_ITERS > 0)); then
    echo -n "  Benchmarking ($BENCH_ITERS iterations, in-process)... "
    RESULT=$(extract_bench "$CPP_BIN" --bench "$BENCH_ITERS")
    parse_bench "C++20" "$RESULT"
    echo "done."
fi

# ------------------------------------------------------------------
# Line count comparison
# ------------------------------------------------------------------
echo
echo -e "${BOLD}Line Counts (excluding blank lines and comments)${RESET}"
echo -e "${BOLD}──────────────────────────────────────────────────${RESET}"

count_lines() {
    local file="$1"
    local ext="${file##*.}"
    case "$ext" in
        a4)   grep -cvE '^\s*$|^\s*#' "$file" ;;
        erl)  grep -cvE '^\s*$|^\s*%%' "$file" ;;
        py)   grep -cvE '^\s*$|^\s*#|^\s*"""' "$file" | head -1 ;;
        cpp)  grep -cvE '^\s*$|^\s*//|^\s*/?\*' "$file" ;;
        exs)  grep -cvE '^\s*$|^\s*#' "$file" ;;
        ts)   grep -cvE '^\s*$|^\s*//|^\s*/?\*' "$file" ;;
    esac
}

for pair in \
    "ActorForth:$SCRIPT_DIR/debounce_demo.a4" \
    "Erlang:$SCRIPT_DIR/debounce_erlang_equivalent.erl" \
    "Python:$SCRIPT_DIR/debounce_python_equivalent.py" \
    "Elixir:$SCRIPT_DIR/debounce_elixir_equivalent.exs" \
    "TypeScript:$SCRIPT_DIR/debounce_typescript_equivalent.ts" \
    "C++20:$SCRIPT_DIR/debounce_cpp20_equivalent.cpp"; do
    label="${pair%%:*}"
    file="${pair#*:}"
    lines=$(count_lines "$file" 2>/dev/null || echo "?")
    printf "  %-14s %s lines of code\n" "$label" "$lines"
done

# ------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------
echo
echo -e "${BOLD}========================================${RESET}"
echo -e "${BOLD} Results: ${GREEN}$PASS passed${RESET}, ${RED}$FAIL failed${RESET}"
echo -e "${BOLD}========================================${RESET}"

if ((BENCH_ITERS > 0)); then
    echo
    echo -e "${BOLD}Timing Summary (in-process, excludes VM startup)${RESET}"
    echo -e "${BOLD}──────────────────────────────────────────────────────────────────────${RESET}"
    printf "  ${BOLD}%-14s %12s %12s %12s   %s${RESET}\n" "Language" "Min" "Avg" "Max" "Iterations"
    echo -e "  ${BOLD}──────────────────────────────────────────────────────────────────${RESET}"
    for lang in "ActorForth" "Erlang" "Python" "C++20"; do
        if [[ -n "${BENCH_AVG[$lang]:-}" ]]; then
            local_min=$(fmt_us "${BENCH_MIN[$lang]}")
            local_avg=$(fmt_us "${BENCH_AVG[$lang]}")
            local_max=$(fmt_us "${BENCH_MAX[$lang]}")
            local_n="${BENCH_N[$lang]}"
            printf "  %-14s %12s %12s %12s   %s\n" \
                "$lang" "$local_min" "$local_avg" "$local_max" "$local_n"
        fi
    done
fi

# Exit with failure if any tests failed
((FAIL == 0))
