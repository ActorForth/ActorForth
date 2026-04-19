#!/bin/bash
# run_python.sh
#
# Runs the naive and correct Python elevator reference demos.
# Each demo issues two floor requests, waits a few seconds, and
# prints final car state. Exits non-zero if the toolchain is
# missing; skips gracefully with a message otherwise.
#
# Running does NOT prove the naive is broken. FLAWs 1-4 are
# TOCTOU / ordering races that do not always manifest. The proof
# that the naive is broken lives in its FLAW source comments and
# in the a4 comparison tests. Running is for "it executes."

set -uo pipefail
cd "$(dirname "$0")"

if ! command -v python3 >/dev/null 2>&1; then
    echo "[skip] python3 not installed. To run this comparison:"
    echo "       Debian/Ubuntu: apt install python3"
    echo "       macOS:          brew install python3"
    exit 0
fi

echo "--- Python naive (elevator_naive.py) ---"
python3 elevator_naive.py
echo ""

echo "--- Python correct (elevator_correct.py) ---"
python3 elevator_correct.py
echo ""
