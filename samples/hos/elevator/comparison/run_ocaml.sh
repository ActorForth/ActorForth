#!/bin/bash
# run_ocaml.sh
#
# Compiles and runs the naive and correct OCaml elevator reference
# demos. Requires ocamlfind and the threads.posix package. Each
# demo issues two floor requests, sleeps a few seconds, and prints
# final car state.
#
# Running does NOT prove the naive is broken. The naive's header
# comment is explicit: OCaml's module system and strong typing
# push several flaws away; the author had to go out of their way
# to reproduce the Python-style bugs. Read the source alongside
# the a4 spec to see the difference in scaffolding.

set -uo pipefail
cd "$(dirname "$0")"

if ! command -v ocamlfind >/dev/null 2>&1; then
    echo "[skip] ocamlfind not installed. To run this comparison:"
    echo "       Debian/Ubuntu: apt install ocaml ocaml-findlib"
    echo "       macOS:          brew install ocaml opam && opam install ocamlfind"
    exit 0
fi

mkdir -p out

compile_and_run() {
    local src="$1"
    local bin="out/$(basename "$src" .ml)"
    echo "--- OCaml $(basename "$src") ---"
    ocamlfind ocamlopt -package threads.posix -linkpkg -thread \
        "$src" -o "$bin" 2>&1 | sed 's/^/  /'
    if [ -x "$bin" ]; then
        "$bin"
    else
        echo "  (compilation failed; see messages above)"
    fi
    echo ""
}

compile_and_run elevator_naive.ml
compile_and_run elevator_correct.ml
