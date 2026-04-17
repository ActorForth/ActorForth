#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
PROJECT_ROOT="$(cd ../.. && pwd)"

echo "=== ActorForth ==="
cat > /tmp/run_af_skip.escript << EOF
#!/usr/bin/env escript
%%! -pa $PROJECT_ROOT/_build/default/lib/actorforth/ebin -pa $PROJECT_ROOT/_build/default/lib/jsx/ebin
main(_) ->
    af_repl:init_types(),
    af_repl:run_file("$PROJECT_ROOT/samples/skip_list/skip_list_test.a4").
EOF
escript /tmp/run_af_skip.escript 2>&1 | grep "All skip list tests passed"
rm -f /tmp/run_af_skip.escript

echo "=== Python ==="
python3 skip_list.py

echo "=== Erlang ==="
erlc skip_list.erl
erl -noshell -s skip_list run -s init stop
rm -f skip_list.beam

echo "=== Elixir ==="
elixir skip_list.exs

echo "=== TypeScript ==="
(cd "$PROJECT_ROOT/ts" && npx tsx ../samples/skip_list/skip_list.ts)

echo "=== C++ ==="
g++ -std=c++20 -O2 -o /tmp/skip_list_cpp skip_list.cpp
/tmp/skip_list_cpp
rm -f /tmp/skip_list_cpp

echo "=== ALL PASSED ==="
