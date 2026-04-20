#!/bin/bash
# run_rust.sh
#
# Compile and run the Rust bench. Output directory is ./out/ (same
# convention as samples/hos/elevator/comparison/run_rust.sh). The
# first argument is N, the number of timed requests (default 1000).
#
# If rustc is not installed, exits 0 with a skip message so the
# orchestrator can proceed without the Rust number.

set -uo pipefail
cd "$(dirname "$0")"

if ! command -v rustc >/dev/null 2>&1; then
    echo "[skip] rustc not installed. To run this comparison:"
    echo "       curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    exit 0
fi

mkdir -p out
RUSTC_FLAGS="-O -A warnings"
rustc $RUSTC_FLAGS elevator_bench.rs -o out/elevator_bench || exit 1

N="${1:-1000}"
./out/elevator_bench "$N"
