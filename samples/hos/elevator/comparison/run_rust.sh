#!/bin/bash
# run_rust.sh
#
# Compiles and runs the naive and correct Rust elevator reference
# demos as single-file programs. Each demo issues two floor
# requests, sleeps ~5 seconds, and prints final car state.
#
# Both files are single source files; no Cargo.toml is required.
# rustc produces binaries in ./out/ (gitignored).
#
# Running does NOT prove the naive is broken. The naive's header
# comment is explicit: Rust's borrow checker makes several Python-
# style flaws harder to write. The comparison value is in reading
# the source alongside the a4 spec and seeing how much less
# scaffolding the a4 version needs.

set -uo pipefail
cd "$(dirname "$0")"

if ! command -v rustc >/dev/null 2>&1; then
    echo "[skip] rustc not installed. To run this comparison:"
    echo "       curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    exit 0
fi

mkdir -p out

# -A warnings: silence Rust's dead-code / unused-variant warnings.
# The naive file intentionally declares some methods it does not
# invoke (they would be used under worse-case interleaving); the
# correct file has minor unused-import noise. Neither is a defect.
RUSTC_FLAGS="-O -A warnings"

echo "--- Rust naive (elevator_naive.rs) ---"
rustc $RUSTC_FLAGS elevator_naive.rs -o out/elevator_naive
./out/elevator_naive
echo ""

echo "--- Rust correct (elevator_correct.rs) ---"
rustc $RUSTC_FLAGS elevator_correct.rs -o out/elevator_correct
./out/elevator_correct
echo ""
