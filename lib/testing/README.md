# ActorForth Test DSL

Test files in this directory (`*.test.a4`) drive the a4-native test
runner. Run them with:

```
rebar3 shell --eval 'af_test_runner:run(["lib/testing"]).'
```

or via `a4c test` once a release is built. `rebar3 eunit` also runs
this suite automatically (via `test/a4_suite_tests.erl`).

## Size: a4 tests are on average 41% smaller than Erlang equivalents

Across the 14 suites migrated from Erlang EUnit to the a4 test DSL,
unit tests implemented in a4 are on average **41% smaller** than the
equivalent Erlang code. Individual reductions ranged from +8%
(pattern tests, which needed an extra assertion per test) to −75%
(FFI tests, where the wrapper boilerplate vanished).

Detailed breakdown in `docs/design/test-dsl-plan.md` under
"Measured result."

## Vocabulary in brief

| word | shape |
|---|---|
| `list_ops group … .` | open a group scope (atom name); `.` closes |
| `list_ops group-serial … .` | group whose tests run sequentially |
| `"name" test : … ;` | positive test |
| `"name" test-raises ErrType : … ;` | negative test |
| `"name" "reason" skip-test : … ;` | registered but skipped |
| `"name" test-compiled : … ;` | flag body for native compile |
| `setup : … ;` / `teardown : … ;` | per-test fixture inside a group |
| `"label" tag` | tag accumulates onto the next test |
| `N max-depth` | auto-assert peak data-stack depth |
| `assert` / `assert-eq` / `assert-max` / `assert-min` | assertions |

Full plan and design decisions: `docs/design/test-dsl-plan.md`.

## Runner flags

```
--quiet / --verbose / --dashboard / --stream
--trace                    enable per-dispatch recording
--match PATTERN            substring filter on test names
--tag LABEL                filter by tag
--seed N                   fix scheduler seed (reproducible order)
--tap                      TAP 13 to stdout
--junit PATH               JUnit XML
--json PATH                JSON results
--coverage                 enable coverage accumulation
--lax-coverage             disable the threshold gate
--coverage-threshold N     custom floor (default 95)
```

## .test.a4 semantics

- Filename becomes the implicit outer group when the file doesn't
  open with an explicit `<atom> group`. `samples/list_ops.test.a4`
  wraps into `list_ops group … .` automatically.
- Excluded from the production build glob (`rebar3 actorforth
  compile` skips `**/*.test.a4`).
- Helper-word definitions inside a `.test.a4` file are file-local
  by virtue of the runner's per-file `af_type:reset()`.

## Known parallelism caveat

Tests that define their own product types or compiled words share
the ETS type registry across worker actors. If two `.test.a4` files
both `type Counter x Int .`, the parallel run can race. Workarounds:

1. Use unique type/word names per file (e.g. `ActorCnt` instead of
   `Counter`).
2. Declare the group `group-serial` if its internal state is
   sensitive.
3. For actor-heavy test sets, put the file under `samples/` so it
   doesn't join the default parallel run.
