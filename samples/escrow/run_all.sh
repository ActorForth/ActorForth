#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
PROJECT_ROOT="$(cd ../.. && pwd)"

echo "=== ActorForth ==="
cat > /tmp/run_af_escrow.escript << EOF
#!/usr/bin/env escript
%%! -pa $PROJECT_ROOT/_build/default/lib/actorforth/ebin -pa $PROJECT_ROOT/_build/default/lib/jsx/ebin
main(_) ->
    af_repl:init_types(),
    af_repl:run_file("$PROJECT_ROOT/samples/escrow/escrow_test.a4").
EOF
escript /tmp/run_af_escrow.escript 2>&1 | grep -E "escrow "
rm -f /tmp/run_af_escrow.escript

echo "=== Python ==="
python3 escrow.py

echo "=== Erlang ==="
erlc escrow.erl
erl -noshell -s escrow run -s init stop
rm -f escrow.beam

echo "=== Elixir ==="
elixir escrow.exs

echo "=== TypeScript ==="
(cd "$PROJECT_ROOT/ts" && npx tsx ../samples/escrow/escrow.ts)

echo "=== C++ ==="
g++ -std=c++20 -O2 -o /tmp/escrow_cpp escrow.cpp
/tmp/escrow_cpp
rm -f /tmp/escrow_cpp

echo "=== Solidity reference: Escrow.sol (not executed here) ==="

echo "=== ALL PASSED ==="
