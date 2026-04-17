# Skip list demo

A probabilistic skip list with MAX_LEVEL=4. Insert, delete, lookup,
and ordered iteration. Coin-flip promotion to higher levels gives
O(log n) expected search. Same algorithm in every language — no
shortcuts, no library calls, no actor magic.

## Run the tests across all languages

```
./run_all.sh
```

Every implementation runs its inline tests plus edge-case coverage
(empty queries, duplicate inserts, delete-absent-key, overwrite,
mixed insert/delete, sorted-order invariant). The a4 test file
`skip_list_test.a4` adds ~94 lines of edge tests beyond the module
inline tests.

## Code-size comparison

Non-blank, non-comment lines of the implementation file:

| Language     | Impl lines |
|--------------|-----------:|
| Elixir       | 89         |
| Erlang       | 90         |
| Python       | 99         |
| TypeScript   | 100        |
| C++20        | 100        |
| **a4**       | **147**    |

**a4 is longer here, and that's expected.** A skip list is pure data
structure manipulation — no actors, no state machine validation, no
message protocol. Everything a4 buys you (actor-as-type, guarded
dispatch, typed messages) doesn't apply. What's left is recursion over
a nested list-of-lists, and concatenative syntax shines less at that
than expression-oriented languages with dedicated pattern matching on
recursive structures.

The demo is included anyway because honest comparisons require it.
a4 is not faster or shorter than Elixir for plain functional data
structures; the actor-heavy demos (`auction`, `escrow`, `debounce`)
are where the language earns its line count.

## Files in this directory

- `skip_list.a4` — ActorForth implementation (147 lines)
- `skip_list_test.a4` — extended edge-case tests (94 lines)
- `skip_list.exs` — Elixir reference (89 lines)
- `skip_list.erl` — Erlang reference (90 lines)
- `skip_list.py` — Python reference (99 lines)
- `skip_list.ts` — TypeScript reference (100 lines)
- `skip_list.cpp` — C++20 reference (100 lines)
- `run_all.sh` — runs every implementation and checks the pass message
