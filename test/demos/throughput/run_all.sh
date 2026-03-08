#!/usr/bin/env bash
#
# run_all.sh — Build, test, and benchmark throughput demo implementations.
#
# Usage:
#   ./run_all.sh              # run all tests once (10000 messages each)
#   ./run_all.sh --bench N    # run each test N times IN-PROCESS and report
#                             # timing stats (excludes VM/interpreter startup)
#
# Runs from the ActorForth project root (auto-detected).

set -euo pipefail

# Close stdin so erl/python don't block waiting for input
exec 0</dev/null

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
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
declare -A TIMES

pass() { PASS=$((PASS + 1)); echo -e "  ${GREEN}PASS${RESET}"; }
fail() { FAIL=$((FAIL + 1)); echo -e "  ${RED}FAIL${RESET}"; }

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

# Collect erl lib paths from the rebar3 build
AF_EBIN="$PROJECT_ROOT/_build/default/lib/actorforth/ebin"
PY_EBIN="$PROJECT_ROOT/_build/default/lib/erlang_python/ebin"

echo -e "${BOLD}========================================${RESET}"
echo -e "${BOLD} Throughput Demo — All Implementations${RESET}"
echo -e "${BOLD}========================================${RESET}"
echo -e " 10000 actor messages per run, no sleep delays"
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
        io:format(\"BENCH_RESULT: min=~bus avg=~bus max=~bus (~b iterations)~n\",
                  [Min, Avg, Max, $n]),
        halt(0).
    "
}

# ------------------------------------------------------------------
# 1. ActorForth
# ------------------------------------------------------------------
echo -e "${CYAN}[1/4] ActorForth${RESET} (throughput_demo.a4)"
echo -n "  Running... "

if time_cmd run_a4 "test/demos/throughput/throughput_demo.a4"; then
    echo -n "(${ELAPSED_MS}ms) "; pass
else
    fail
fi

if ((BENCH_ITERS > 0)); then
    echo -n "  Benchmarking ($BENCH_ITERS iterations, in-process)... "
    RESULT=$(extract_bench bench_a4 "$BENCH_ITERS" "test/demos/throughput/throughput_demo.a4")
    TIMES["ActorForth"]="${RESULT#BENCH_RESULT: }"
    echo "${TIMES[ActorForth]}"
fi

# ------------------------------------------------------------------
# 2. Erlang
# ------------------------------------------------------------------
echo -e "${CYAN}[2/4] Erlang/OTP${RESET} (throughput_erlang.erl)"
echo -n "  Compiling... "

if erlc -o "$BUILD_DIR" "$SCRIPT_DIR/throughput_erlang.erl" 2>/dev/null; then
    echo -n "ok. Running... "
    if time_cmd erl -noshell -pa "$BUILD_DIR" -eval \
        "throughput_erlang:start_test(), halt(0)."; then
        echo -n "(${ELAPSED_MS}ms) "; pass
    else
        fail
    fi
else
    echo -n "compile failed. "; fail
fi

if ((BENCH_ITERS > 0)); then
    echo -n "  Benchmarking ($BENCH_ITERS iterations, in-process)... "
    RESULT=$(extract_bench erl -noshell -pa "$BUILD_DIR" -eval \
        "throughput_erlang:bench($BENCH_ITERS), halt(0).")
    TIMES["Erlang"]="${RESULT#BENCH_RESULT: }"
    echo "${TIMES[Erlang]}"
fi

# ------------------------------------------------------------------
# 3. Python
# ------------------------------------------------------------------
echo -e "${CYAN}[3/4] Python${RESET} (throughput_python.py)"
echo -n "  Running... "

if time_cmd python3 "$SCRIPT_DIR/throughput_python.py"; then
    echo -n "(${ELAPSED_MS}ms) "; pass
else
    fail
fi

if ((BENCH_ITERS > 0)); then
    echo -n "  Benchmarking ($BENCH_ITERS iterations, in-process)... "
    RESULT=$(extract_bench python3 "$SCRIPT_DIR/throughput_python.py" \
        --bench "$BENCH_ITERS")
    TIMES["Python"]="${RESULT#BENCH_RESULT: }"
    echo "${TIMES[Python]}"
fi

# ------------------------------------------------------------------
# 4. C++20
# ------------------------------------------------------------------
echo -e "${CYAN}[4/4] C++20${RESET} (throughput_cpp20.cpp)"
echo -n "  Compiling... "

CPP_BIN="$BUILD_DIR/throughput_cpp20"
if g++ -std=c++20 -O2 -pthread -o "$CPP_BIN" \
    "$SCRIPT_DIR/throughput_cpp20.cpp" 2>/dev/null; then
    echo -n "ok. Running... "
    if time_cmd "$CPP_BIN"; then
        echo -n "(${ELAPSED_MS}ms) "; pass
    else
        fail
    fi
else
    echo -n "compile failed. "; fail
fi

if ((BENCH_ITERS > 0)); then
    echo -n "  Benchmarking ($BENCH_ITERS iterations, in-process)... "
    RESULT=$(extract_bench "$CPP_BIN" --bench "$BENCH_ITERS")
    TIMES["C++20"]="${RESULT#BENCH_RESULT: }"
    echo "${TIMES[C++20]}"
fi

# ------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------
echo
echo -e "${BOLD}========================================${RESET}"
echo -e "${BOLD} Results: ${GREEN}$PASS passed${RESET}, ${RED}$FAIL failed${RESET}"
echo -e "${BOLD}========================================${RESET}"

if ((BENCH_ITERS > 0)); then
    echo
    echo -e "${BOLD}Timing Summary (in-process, excludes startup)${RESET}"
    echo -e "${BOLD}──────────────────────────────────────────────${RESET}"
    for lang in "ActorForth" "Erlang" "Python" "C++20"; do
        if [[ -n "${TIMES[$lang]:-}" ]]; then
            printf "  %-14s %s\n" "$lang" "${TIMES[$lang]}"
        fi
    done
fi

# Exit with failure if any tests failed
((FAIL == 0))
