#!/usr/bin/env bash
# Run every language implementation of the auction demo and confirm
# each prints its pass message. Used as a smoke test for the comparison set.
set -euo pipefail
cd "$(dirname "$0")"
PROJECT_ROOT="$(cd ../.. && pwd)"

echo "=== ActorForth ==="
cat > /tmp/run_af_auction.escript << EOF
#!/usr/bin/env escript
%%! -pa $PROJECT_ROOT/_build/default/lib/actorforth/ebin -pa $PROJECT_ROOT/_build/default/lib/jsx/ebin
main(_) ->
    af_repl:init_types(),
    af_repl:run_file("$PROJECT_ROOT/samples/auction/auction_test.a4").
EOF
escript /tmp/run_af_auction.escript 2>&1 | grep -E "auction test:"
rm -f /tmp/run_af_auction.escript

echo "=== Python ==="
python3 auction.py

echo "=== Erlang ==="
erlc auction.erl
erl -noshell -s auction run -s init stop
rm -f auction.beam

echo "=== Elixir ==="
elixir auction.exs

echo "=== TypeScript ==="
(cd "$PROJECT_ROOT/ts" && npx tsx ../samples/auction/auction.ts)

echo "=== C++ ==="
g++ -std=c++20 -O2 -pthread -o /tmp/auction_cpp auction.cpp
/tmp/auction_cpp
rm -f /tmp/auction_cpp

echo "=== ALL PASSED ==="
