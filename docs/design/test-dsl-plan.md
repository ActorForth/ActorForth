# ActorForth Test DSL — Design & Implementation Plan

**Status:** Design locked through 2026-04-18 session. Not yet
implemented. Phase 1 starts next.

**Purpose:** Capture every decision from the design conversation so
implementation can resume (possibly after context compaction) from
this single reference. If a decision is in conversation but not here,
it isn't locked.

---

## Motivation (one paragraph)

Our 20,858 lines of Erlang test code sit on top of eight recurring
patterns — stack assertions, chained evals, negative tests, actor
lifecycle, compile roundtrips, `foreach` wrappers, multi-clause
dispatch, temp-file I/O. Per-test boilerplate is ~60% of each test's
LOC; actual tested behavior is ~40%. A test DSL written *in* a4 —
with tests as actors, parallelism by default, and a4-native coverage —
shrinks each test to its literal scenario, reinforces DSL-first as the
canonical a4 workflow, and makes the test framework itself a flagship
demo of actors + homoiconicity + structured concurrency.

---

## Three-world test split (locked)

| world | language | tests what | coverage | runner |
|---|---|---|---|---|
| **a4 unit** | a4 (in `.test.a4`) | a4 user code — stdlib words, user words, demos, DSLs | **a4 coverage** | `af_test_runner` (new) |
| **Erlang impl unit** | Erlang (EUnit) | `af_interpreter`, `af_parser`, `af_word_compiler`, `af_ring0/1/2`, `af_lsp`, and every other internal module | `rebar3 cover` | `rebar3 eunit` (existing) |
| **Implementation FAT / integration** | Erlang CT + a4 scenarios | end-to-end pipelines, the whole stack | both | `rebar3 ct` (existing) |

**Invariants:**
- A4 unit tests never exercise af_interpreter **as subject** — only as VM.
- Erlang unit tests never load an a4 file.
- When the Erlang side is rewritten in a4 (or goes away entirely),
  world 2 shrinks naturally; worlds 1 and 3 are unaffected.

**Migration:** current `af_script_SUITE` (hybrid: CT harness loading
a4 scripts to drive EUnit-like assertions) moves to world 1 when the
DSL is ready. CT goes back to pure integration/end-to-end scenarios.

---

## `.test.a4` file extension — semantics (locked)

The extension is a **mode**, not a naming convention. A `.test.a4`
file differs from a regular `.a4` file in the following concrete ways:

1. **Test vocabulary auto-loaded** on file open: `group`, `test`,
   `test-raises`, `setup`, `teardown`, `stack-equals`, `raises`,
   `skip`, `tag`, `assert`, `assert-eq`, `assert-max`, `assert-min`,
   `max-depth`, `max-return-depth`. No `import` needed at top.
2. **Helper words file-local.** `: helper ... ;` inside a `.test.a4`
   is scoped to that file; not published globally; cannot collide
   with helpers of the same name in other test files.
3. **Implicit outer group.** The file name becomes an outer group
   wrapper automatically. `samples/list_ops.test.a4` wraps its tests
   in `group "list_ops" :`. Nested `group "..." :` inside the file
   just deepens the path.
4. **Coverage mode auto-on** when loaded by the test runner.
   Regular `.a4` files run production-fast when loaded outside the
   runner.
5. **Excluded from production builds.** `a4c build` globs `**/*.a4`
   but excludes `**/*.test.a4`. No manifests, no `.testignore` files.
6. **LSP/tooling differentiation.** Completion inside `.test.a4`
   suggests test vocab; inside regular `.a4` it does not. Syntax
   highlighter can color test vocab distinctly.
7. **Lint clarity.** Lint warns: `.test.a4` with no registered tests;
   regular `.a4` file using test vocabulary.

**Location:** co-located with code they test. `lib/skip_list.a4` +
`lib/skip_list.test.a4`. Test runner globs `**/*.test.a4`.

---

## DSL vocabulary (locked)

### Core words

| word | shape | replaces |
|---|---|---|
| `group "name" :` … `;` | open a named scope; can nest | `{foreach, Setup, Teardown, Tests}` |
| `group-serial "name" :` … `;` | like `group` but runs tests serially (opt-out of parallel default) | — (parallel always) |
| `setup :` … `;` | fixture run before every test in the group; leaves state on stack | `fun setup/0` |
| `teardown :` … `;` | post-test cleanup; implicit if omitted (stack discarded, actors killed) | explicit delete/stop lines |
| `"name" test :` … `;` | define a positive test | `fun(_) -> {"name", fun() -> … end} end` |
| `"name" test-raises ErrType :` … `;` | expect the body to raise `ErrType` | `?assertError(#af_error{type = T}, …)` |
| `"name" test-compiled :` … `;` | defs inside get `compile`'d before body runs | word_def + compile + direct call |
| `skip "name" "reason" test :` … `;` | register but skip (skip-prefix reads Forth-natural) | `?_skip(...)` |
| `tag "label" ...` | attach a tag to the next test/group; **multiple `tag`s accumulate** | — |

### Assertion words

| word | shape | purpose |
|---|---|---|
| `assert` | `( Bool -- )` | existing; unchanged |
| `assert-eq` | `( Any Any -- )` | existing; unchanged |
| `stack-equals [v1 v2 …]` | `( -- )` | match the full data stack against a literal shape |
| `raises ErrType <word>` | inline negative assertion on one call | catch `ErrType` thrown by the next invocation |
| `N assert-max` | `( n -- )` | fail if current value exceeds N |
| `N assert-min` | `( n -- )` | fail if current value below N |

### Depth / introspection words

| word | shape | reads |
|---|---|---|
| `data-max-depth` | `( -- Int )` | peak data-stack depth during this test |
| `data-final-depth` | `( -- Int )` | current data-stack depth (at call site) |
| `data-avg-depth` | `( -- Int )` | running average, floor(sum/count) |
| `return-max-depth` | `( -- Int )` | peak return-stack depth |
| `depth-stats` | `( -- Map )` | full stats map for custom checks |

### Group/test directives

| directive | scope | effect |
|---|---|---|
| `max-depth N` | group/test | auto-assert peak data-stack ≤ N at test end |
| `max-return-depth N` | group/test | auto-assert peak return-stack ≤ N |

### Execution-stack words (also usable outside tests)

| word | shape | purpose |
|---|---|---|
| `execution-trace on/off` | `( -- )` | toggle per-dispatch exec-stack recording |
| `exec-depth` | `( -- Int )` | events in exec stack |
| `exec-peek` | `( -- Event )` | newest event (doesn't pop) |
| `exec-pop` | `( -- Event )` | newest event, removed |
| `exec-drop` | `( -- )` | discard newest |
| `exec-clear` | `( -- )` | empty the exec stack |
| `exec-reverse` | `( -- )` | oldest-first for replay |
| `capture-exec` | `( -- List )` | copy the exec stack as a list value |
| `capture-data` | `( -- List )` | copy the data stack as a list value |
| `capture-return` | `( -- List )` | copy the return stack as a list value |

---

## Data structures (locked)

### Execution-stack event

Each dispatch in traced mode pushes:

```erlang
%% 4-tuple, ~40 bytes + shared structure
{exec, TokenRef, StackRef, Kind}
```

- `TokenRef` : pointer to the shared `#token{}` record (file, line, col, value, quoted)
- `StackRef` : pointer to the stack-after list; structural sharing with previous event
- `Kind`    : one of
  - `{tos, Op}` — found in TOS type's dict
  - `{any, Op}` — found in Any dict
  - `{handler, TosType}` — dispatched via handler
  - `{literal, TypedValue}` — parsed as literal
  - `atom` — fallthrough to `{'Atom', Value}`
  - `{cache_hit, Entry}` — dispatch cache hit

### Depth stats

Fixed-size record on `#continuation{}` in traced mode:

```erlang
#depth_stats{
    data_max    = 0 :: non_neg_integer(),
    data_sum    = 0 :: non_neg_integer(),
    return_max  = 0 :: non_neg_integer(),
    count       = 0 :: non_neg_integer()
}
```

Updated per dispatch. `avg = data_sum div count` computed on read.

### Provenance — derived from exec stack on demand (traced mode only)

Stack items stay `{Type, Value}` — we preserve the 2-tuple invariant
every type module's pattern matching depends on. Provenance is
reconstructed from the exec stack on demand: the diagnostic engine
walks backward, simulating pushes/pops per event, to attribute each
item at the final stack to its push site (file, line, col, token).

O(N) in exec-event count but only invoked on test failure, so cost
is negligible. Benefit: no stack op needs modification; every piece
of code that pattern-matches `{Type, Value}` continues to work
unchanged. The exec stack is the single source of truth for where
values came from.

Gated by `#continuation.tracing` flag — same flag that enables the
exec stack. Off in production, off in non-test continuations.

### Continuation record additions (locked)

```erlang
-record(continuation, {
    data_stack      = [] :: list(),
    return_stack    = [] :: list(),
    current_token   = undefined,
    debug           = false,
    word_trace      = [],
    dictionary      = undefined :: map() | undefined,
    dispatch_cache  = #{} :: map(),       %% already added
    %% --- new in test DSL ---
    tracing         = false :: boolean(), %% master test-mode flag
    exec_stack      = [] :: list(),       %% only populated when tracing
    depth_stats     = undefined :: undefined | #depth_stats{}
}).
```

---

## Runtime gating — production cost (locked)

**Production runtime cost must be zero.** Achieved by gating at
`interpret_tokens/2` entry, not per-token:

```erlang
interpret_tokens(Tokens, #continuation{tracing = false} = C) ->
    interpret_fast(Tokens, C);
interpret_tokens(Tokens, #continuation{tracing = true} = C) ->
    interpret_traced(Tokens, C).
```

`interpret_fast` is identical to today's hot path. `interpret_traced`
pushes an exec event and updates depth stats after each dispatch.
Production continuations default `tracing = false`, never reach the
traced path.

**Memory cost in production:** ~24 bytes per continuation (three new
fields at default values). Trivial.

**Why not compile-time excise:** operators and tools should be able
to flip `execution-trace on` in a live REPL or production actor for
ad-hoc diagnosis without a rebuild. Same reasoning as the existing
`debug` flag.

---

## Interpreter path coverage (locked)

a4 runs on three dispatch paths today:

1. **Parasitic interpreter** (`af_interpreter`) — legacy path,
   ETS-backed type registry.
2. **Ring 0** (`af_ring0:run`) — self-hosted VM path, continuation-
   local dict.
3. **Native-compiled BEAM** — direct function calls; no dispatch loop.

**Phase 1 instruments only path 1.** The traced path lives in
`af_interpreter`. Coverage and exec-stack granularity apply to
interpreted dispatch only. Native-compiled words are counted
atomically — the call site appears in the exec stack, but internal
token steps are invisible.

**Implication for authors:** a test that verifies an already-compiled
production word sees it as a black box. Fine for behavioral tests
(most tests). A test that *requires* internal token visibility must
load the subject's source file in test mode so it runs interpreted.

**Ring 0 instrumentation deferred** until the self-hosted path is the
primary runtime. When that lands, `af_ring0:run` grows the same
tracing hooks, sharing the exec-stack event format.

---

## Parallelism and isolation (locked)

- **Every test is an actor.** No exceptions.
- **Runner = top-level supervisor.** Groups = sub-supervisors,
  `one_for_one`. Tests = workers, `transient`. Actors spawned *by* a
  test are children of that test's supervisor and die with it —
  automatic cleanup.
- **Maximal concurrency by default.** All tests in a group spawned
  simultaneously; completion order is arbitrary per run. This
  exercises unbounded-non-determinism correctness — a test that
  secretly depends on another will start flaking immediately, which
  is the point.
- **`group-serial` to opt out** for tests sharing external state.
- **Test isolation:** each test actor gets a fresh continuation
  with a snapshot of the dictionary at test-runner boot. Definitions
  made during a test never leak into ETS or into sibling tests.
- **ETS:** avoid during test execution. The dispatch cache is
  already per-continuation. Definitions during tests route through
  `dict_*` paths (continuation-local map), not through
  `af_type:add_op` (ETS). This audit is part of Phase 1.

---

## Coverage (locked)

**Four metrics reported, one gated:**

| metric | threshold gate? |
|---|---|
| Line coverage (% of tokens visited) | **yes — default 95%** |
| Word coverage (% of defined words called) | no, reported |
| Clause coverage (% of multi-clause branches hit) | no, reported |
| Guard coverage (% of guards evaluated both true and false) | no, reported |

**CLI flags:**
- `--lax-coverage` — disable threshold gate.
- `--coverage-threshold N` — custom floor; `--coverage-threshold 0` ≡ `--lax-coverage`.

**Exclusions:** `.test.a4` files excluded from coverage metrics
(tests don't measure themselves). `lib/testing/*.a4` — the DSL
itself — is likewise excluded: the DSL is exercised by construction
when any test runs, and including it would inflate numbers without
signal. Library files in `lib/` (non-testing) and `src/bootstrap/`
count. Samples count when exercised.

**Implementation:** coverage is another sink on the traced dispatch
path. Each event contributes to per-file `{line, col}` sets and the
word/clause/guard counters.

---

## Runner UX (locked)

### Four verbosity levels

1. **Default (TTY).** Live dashboard:

```
  ActorForth Test Runner
  Supervisor: <0.84.0> → 14 groups → 247 test actors — 8 cores, 8 running
  ════════════════════════════════════════════════════════════════════════

  Running now (8):                    │ Completed (47 / 247, 19%)
    <0.112> list_ops  edge/cons       │  ✓ <0.103> list_ops cons       2ms
    <0.118> auction   bid rejected    │  ✓ <0.104> list_ops head       1ms
    ...                               │  ✗ <0.105> list_ops tail/head  2ms
                                      │  ...
  ════════════════════════════════════════════════════════════════════════
  [===========>                                              ] 47/247  2.1s
```

Refresh ~10 Hz. Real BEAM pids shown (actor story is visible).

2. **`--quiet`:** only the final summary. Non-zero exit on fail. CI.
3. **`--verbose`:** per-test timing, group breakdown.
4. **`--trace`:** per-token dispatch trace. Pair with `--match` to
   scope to a single failing test.

### Stream mode (non-TTY)

One line per completion, TAP-compatible:

```
ok     <0.103> list_ops/cons          2ms
not ok <0.105> list_ops/tail/head     2ms
```

### Structured output

- `--tap` — TAP 13.
- `--junit=path.xml` — JUnit XML.
- `--json=results.json` — structured results.

### Summary blocks

Every run, default output tail:

```
247 tests, 245 passed, 1 failed, 1 skipped    2.31s
a4 coverage: 88.4% (1,419 / 1,605 lines) — 3 files below 80%

Stack depths (data):
  median peak: 3     p95: 7     max: 14 (samples/lib/parser.test.a4  "large corpus")
  median final: 0    tests leaving residue: 0
```

Verbose gets "Deepest tests" and "Slow tests" tables.

---

## Diagnostic engine categories (locked)

Six categories of stack-mismatch failure. Each is an a4 word that
walks the exec stack backward and produces a structured diagnostic.
All written in a4 (in `lib/testing/diagnose.a4`), eating our own
dogfood.

1. **Extra atom on stack** — token fell through to `{'Atom', _}`
   because no op matched. Levenshtein against known names in scope;
   suggest top 2–3.
2. **Missing consumer** — extra value at depth N whose producer was
   identified in the exec stack, never consumed.
3. **Wrong order** — actual stack is a permutation of expected and
   can be reached with 1–2 stack ops; suggest `swap`/`rot`.
4. **Wrong clause fired** — multi-clause dispatch took a branch the
   author didn't expect; show the stack at dispatch and which clause
   matched.
5. **Silent type mismatch** — expected `Int`, got `String 7`;
   identify the push site.
6. **Suddenly deep stack** — current test's max-depth exceeds its
   group's historical p95 by >2x; correlate depth-spike with dispatch
   kinds to spot uncluded map/filter/reduce results.

### Example diagnostic output

```
✗ "filter then reduce"  samples/lib/list_ops.test.a4:43

  expected: [Int 15]
  actual:   [Int 15, Int 10, Int 4, List[..], Atom "sum"]  (depth 5)

  diagnosis: stack depth jumped from typical 2 to peak 5 during this test.
  
  first abnormal growth at line 44:
    line 44: map     →  [List ..., List ..., Atom "add1"]   ← depth 2 → 3
    line 44: filter  →  [List ..., List ..., Atom "pos?"]   ← depth 3 still
    line 45: reduce  →  [Int 15, List ..., List ..., Atom "sum"]  ← depth 4

  likely cause: map/filter/reduce left quoted-word atoms behind.
  compare signatures — each expects its atom consumed.
```

---

## VM primitives to add — Phase 1 (locked)

New module: **`src/af_test_prim.erl`** (dedicated, separate from
`af_type_any.erl` for clean separation of test-only machinery).

| Erlang function | a4 word | signature | purpose |
|---|---|---|---|
| `capture_stack/1` | `capture-data` | `( -- List )` | stack as list value |
| `capture_return/1` | `capture-return` | `( -- List )` | return stack as list |
| `capture_exec/1` | `capture-exec` | `( -- List )` | exec stack as list |
| `trace_on/1` | `execution-trace on` | `( -- )` | flip `tracing = true`, init `exec_stack = []` + `depth_stats` |
| `trace_off/1` | `execution-trace off` | `( -- )` | flip `tracing = false`, clear fields |
| `get_trace/1` | `get-trace` | `( -- List )` | alias for `capture-exec` |
| `get_coverage/1` | `get-coverage` | `( -- Map )` | accumulated coverage data |
| `depth_stats_map/1` | `depth-stats-map` | `( -- Map )` | `#depth_stats{}` as map (keys: `data_max`, `data_sum`, `return_max`, `count`) |
| `dict_snapshot/1` | `dict-snapshot` | `( -- DictRef )` | snapshot current continuation dict |
| `dict_restore/2` | `dict-restore` | `( DictRef -- )` | revert to snapshot |

All primitives are thin FFI over continuation fields. No primitive
here requires new VM machinery — the heavy lifting is the dispatch
split and event accounting in `interpret_traced/2`.

### Phase 1 explicitly does NOT include:

These were in an earlier draft; moved to Phase 2 because they compose
from existing a4 vocabulary:

| word | Phase 2 implementation |
|---|---|
| `ms-timeout` | a4, built on `spawn` + `receive-timeout` — no new primitive |
| `data-max-depth` | a4: `depth-stats-map "data_max" map-get` |
| `data-final-depth` | a4: `capture-data length` |
| `data-avg-depth` | a4: read `data_sum` / `count` from stats map |
| `return-max-depth` | a4: `depth-stats-map "return_max" map-get` |
| `depth-stats` | a4 alias for `depth-stats-map` |
| `stack-equals`, `raises`, `assert-max`, `assert-min` | a4 in `lib/testing/assertions.a4` |

---

## DSL source — Phase 2 (locked)

New files under **`lib/testing/`** (it's just a4 code, like stdlib):

```
lib/testing/
  test-dsl.a4          # types + handlers for group/test/setup/teardown/test-raises
  assertions.a4        # stack-equals, raises, assert-max, assert-min
  depth.a4             # depth-tracking words
  diagnose.a4          # stack-mismatch category analyzers
  runner-bootstrap.a4  # what .test.a4 files get as implicit prelude
```

Types introduced (each with a handler):

- `Group` — scope with name + nested children
- `TestDef` — a registered test
- `SetupDef` — fixture body
- `TeardownDef` — teardown body
- `RaisesErrType` — expected error type for negative tests
- `Tag` — string label attached to a TestDef

Handlers implement the scope-via-token-dispatch mechanism (same
pattern as `WordDefinition` / `CodeCompile` in the current compiler
types). `;` pops one scope.

---

## Test runner — Phase 3 (locked)

New module: **`src/af_test_runner.erl`**. Entry points:

- `run/1` — takes a config map (file globs, parallel setting,
  coverage threshold, output mode, filter args) and runs.
- `run_file/2` — single file, programmatic use.
- CLI entry via `a4c test [args]`.

### Responsibilities

1. **Discovery:** glob `**/*.test.a4` respecting `--file`, `--match`,
   `--tag`, `--group` filters.
2. **Loading:** for each file, start an actor with the bootstrap
   continuation (test vocab preloaded), evaluate the file to fill the
   test registry.
3. **Spawning:** under a top-level supervisor, spawn per-group
   sub-supervisors; per-test actors under those. All parallel unless
   `group-serial` declared.
4. **Collecting:** each test actor sends a result message with pass
   / fail / skip / error, timings, depth stats, coverage contribution,
   and exec stack if failed.
5. **Rendering:** dashboard / stream / TAP / JUnit / JSON per flags.
6. **Aggregating:** sum coverage maps, compute threshold, exit code.

### Rough size

~300 lines Erlang. Mostly process orchestration + format rendering.

---

## Phased implementation plan (locked)

### Phase 1 — VM primitives (≈200–250 lines Erlang + ≈100 lines tests)

- Extend `#continuation{}` with `tracing`, `exec_stack`, `depth_stats`;
  define `#depth_stats{}` record in `include/continuation.hrl`.
- Split `interpret_tokens/2` into `interpret_fast/2` + `interpret_traced/2`.
  `interpret_fast` must be bit-for-bit identical to current hot path.
- New module `af_test_prim.erl` with primitives from table above.
- **ETS-mutation audit:** enumerate every test-DSL surface op that
  mutates dict state via `af_type:add_op` / `add_type`; rewire each
  through continuation-local dict paths; document remaining ETS uses.
- **Dispatch cache in traced mode:** cache short-circuits must emit
  `{cache_hit, Entry}` exec events; verify no event loss on hit.
- EUnit tests for each primitive and each continuation-field path.
- **Exit criteria:**
  - `trace_on` → run tokens → `get-trace` returns correctly-structured
    event list.
  - Depth stats updated correctly across a representative mix of
    dispatches (literal, Any op, TOS op, handler, cache-hit).
  - All 2146 existing tests still pass.
  - Production `interpret_fast` unchanged (bit-for-bit identical to
    current hot path).
  - ETS audit complete: no test-DSL primitive writes to ETS.
  - Cache correctness verified in both fast and traced modes.

### Phase 1.5 — Diagnostic tape capture (≈100 lines Erlang + ≈300 lines a4)

- Provenance attached to stack items in traced mode.
- Depth-stats delta-tracking optimized to O(1) per dispatch (using
  dispatch-kind arity instead of `length/1`).
- Category analyzers in a4 under `lib/testing/diagnose.a4`.
- **Exit criteria:** given a failed trace, the diagnostic engine
  produces at least one category-specific analysis per example in the
  six categories above.

### Phase 2 — DSL core (≈250–300 lines a4)

- Types with handlers in `lib/testing/test-dsl.a4` (Group, TestDef,
  SetupDef, TeardownDef, RaisesErrType, Tag). Includes `ms-timeout`
  built on `spawn` + `receive-timeout`.
- Assertion words in `lib/testing/assertions.a4` (`stack-equals`,
  `raises`, `assert-max`, `assert-min`).
- Depth accessor words in `lib/testing/depth.a4` (`data-max-depth`,
  `data-final-depth`, `data-avg-depth`, `return-max-depth`,
  `depth-stats`) — all one-liners over `depth-stats-map`.
- **Exit criteria:** a hand-written `.test.a4` file can `group`,
  `test`, `test-raises`, `setup`, `teardown`, `ms-timeout`, all
  interpreted correctly (no runner yet — use REPL to verify).

### Phase 3 — Test runner (≈300 lines Erlang)

- `af_test_runner.erl` with discovery / loading / spawning /
  collecting / rendering.
- CLI integration via `a4c test`.
- Default dashboard mode, stream mode, TAP/JUnit/JSON output.
- **`--seed N` flag** to fix scheduler seed for reproducible actor
  ordering. Default is randomized; seed reported in every summary so
  any failing run can be replayed.
- **Exit criteria:** `a4c test` against a directory with a single
  `.test.a4` file shows dashboard, passes all tests, exit code 0.
  A seeded re-run produces identical completion order.

### Phase 4 — Pilot migration (≈3 days)

- Port `test/af_list_tests.erl` → `lib/list_ops.test.a4` (or
  equivalent location alongside list-ops implementation).
- Port `test/af_pattern_tests.erl` → `lib/pattern.test.a4`.
- **Exit criteria:**
  - LOC reduction ≥ 3x per file
  - All ported tests pass under new runner
  - Failure messages ≥ EUnit's informativeness
  - Runtime within 2x of EUnit (parallelism should compensate)

### Phase 5 — Broader migration + docs

- Port `af_type_any_tests`, `af_actor_tests`, `af_integration_SUITE`.
- **Keep in Erlang:** `af_word_compiler_tests`, `af_lsp_tests`,
  `af_interpreter_coverage_tests`, `af_error_tests` (they test
  Erlang-side APIs with no a4 surface).
- Write the testing chapter in `docs/IntroToActorForth.md`.
- Update talk slides.

---

## Deferred — with trigger conditions

These are design decisions we *considered* and consciously deferred.
Trigger conditions specify when to revisit.

| item | trigger to revisit |
|---|---|
| **Ring-mode exec stack** (`execution-trace ring N`) | a real test hits OOM from unbounded exec growth, and `exec-clear` + selective tracing are insufficient |
| **Depth-history regression detection** | after Phase 5, when suite is stable enough that baseline comparison adds signal |
| **Fine-grained `max-depth` overrides** (per-test rather than per-group) | users want it; Phase 2 directives are group-level only |
| **Testing chapter in intro book** | after Phase 3 ships and vocabulary is stable |
| **LLM-fit chapter** | after testing chapter |
| **VS Code extension for `.test.a4`** | after Sublime-based talk demo on 2026-04-30 |
| **Provenance-on-items in production mode** (for operator debugging of live actors) | when an operator actually needs it on a live system |

---

## Rules & invariants (locked, as stated in conversation)

- **Maximum concurrency is the default.** Different completion
  orders every run. Display makes the concurrency visible (pids,
  supervisor tree, streaming completions).
- **Tests run as actors under a supervisor tree.** Crashes become
  structured failure reports. Spawned actors die with their test.
- **Helpers in `.test.a4` files are file-local.** No leakage between
  test files.
- **ETS avoidance during tests.** Definitions route through
  continuation-local dict paths. Audit is a Phase 1 task.
- **Coverage default threshold is 95%.** `--lax-coverage` /
  `--coverage-threshold N` to adjust.
- **Good a4 has shallow stacks.** Track + report + allow assertions
  on depth. "Suddenly deep" is its own diagnostic category.
- **Production cost must be zero.** Gate at `interpret_tokens/2`
  entry, not per-token. Traced-path code exists but is never reached
  in production.
- **Execution stack is a first-class stack, not a side-car object.**
  Manipulated by ordinary a4 stack vocabulary. Per-continuation.
  Homoiconic.
- **Everything possible should be written in a4** (user's rule):
  the DSL itself, the diagnostic analyzers, the coverage renderer.
  The Erlang side is limited to primitives that need VM support.

---

## Resume checklist — Phase 1

When resuming Phase 1 implementation:

1. Read this plan.
2. Read `src/af_interpreter.erl`, `include/continuation.hrl`,
   `src/af_type.erl`.
3. Add `tracing`, `exec_stack`, `depth_stats` to
   `#continuation{}`; define `#depth_stats{}` record.
4. Split `interpret_tokens/2` into fast + traced paths at entry.
5. Implement `interpret_traced/2` with per-dispatch event push and
   depth-stats update.
6. Create `src/af_test_prim.erl` with primitives from the table above.
7. Register the primitives under `af_type_any` on init (or behind an
   explicit `init_test_prims()` call that test runner makes).
8. Add EUnit tests in `test/af_test_prim_tests.erl` verifying each
   primitive in isolation.
9. Run `./run_tests.sh` — all 2146 existing tests must still pass.
10. Commit as "Phase 1: VM primitives for test DSL (traced dispatch,
    exec stack, depth stats)". Push on user approval.

---

## Resume checklist — Phase 1.5 onwards

Each subsequent phase has its Exit Criteria listed above. After
Phase 1 lands, the natural next step is Phase 1.5 (provenance +
diagnostic engine skeleton) so that Phase 2's DSL has good failure
messages from day one.
