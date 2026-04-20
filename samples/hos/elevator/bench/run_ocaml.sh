#!/bin/bash
# run_ocaml.sh
#
# Compile and run the OCaml correct bench. Uses ocamlfind with
# threads.posix + unix. First argument is N (default 1000).
#
# If ocamlfind is not installed, exits 0 with a skip message so
# the orchestrator can proceed without the OCaml number.

set -uo pipefail
cd "$(dirname "$0")"

if ! command -v ocamlfind >/dev/null 2>&1; then
    echo "[skip] ocamlfind not installed. To run this comparison:"
    echo "       Debian/Ubuntu: apt install ocaml ocaml-findlib"
    exit 0
fi

mkdir -p out
ocamlfind ocamlopt -package threads.posix,unix -linkpkg -thread \
    elevator_bench.ml -o out/elevator_bench_ocaml 2>&1 \
    | sed 's/^/  /'
if [ ! -x out/elevator_bench_ocaml ]; then
    echo "  (compilation failed; see messages above)"
    exit 1
fi

N="${1:-1000}"
./out/elevator_bench_ocaml "$N"
