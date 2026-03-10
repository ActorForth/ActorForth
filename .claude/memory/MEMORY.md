# ActorForth Memory

## Project Context
- Homoiconic, strongly-typed, stack-based, actor-concurrent language targeting BEAM
- Prior implementations: Python (most complete), C++ (partial)
- Current: Erlang implementation — fully self-hosted compiler toolchain
- End goal: self-hosting with BEAM assembler in ActorForth itself
- Design philosophy: minimal primitive ISA (~35 ops), types/pattern-matching/actors as primitives, dictionary as inspectable data
- Related project: `/home/scherrey/projects/elearning` — Erlang/Elixir elearning app, target for af_server bridge integration

## Branch Structure (as of 2026-03-10)
- `master` — stable, all tests passing
- `a4_version3` — merged self-hosted into master, massive coverage push (target >95%)
- `self-hosted` — Ring architecture + 100% self-hosted bootstrap (merged into a4_version3)
- `a4_version2.4` through `a4_version2.6` — older feature branches

## Key Architecture Decisions
- Type-driven dispatch: ops looked up by name + TOS type, "Any" as generic fallback
- Stack items are `{TypeName :: atom(), Value :: term()}` tuples
- Compilation IS interpretation — compiler is type handlers that change token meaning
- Word definition: `: name InputTypes -> OutputTypes ; body .`
- sig_in convention: TOS-first ordering (rightmost in source = TOS)
- Words registered in first sig_in type's dict (= TOS type)
- Compilation via Erlang abstract forms + compile:forms/1 (not Core Erlang directly)

## Type System
- Int, Bool: literal handlers auto-convert tokens
- String: wraps Erlang binaries; `"quoted"` strings auto-convert via `quoted` flag on token record
- Map: wraps Erlang maps; keys/values are full `{Type, Val}` stack items
- List: wraps Erlang cons cells; heterogeneous
- Product types: `type Name field Type .` with auto-generated constructor, getters, setters (source=auto)
- Message: tagged envelope `#{tag => binary(), data => stack_item()}`, created by `msg`

## Value Constraints in Signatures
- `: factorial 0 Int -> Int ;` creates sig_in with `{Int, 0}` value constraint
- Compiler buffers literal values and combines with following type name
- Works in both main clause and sub-clause handlers
- match_sig matches `{Type, Value}` constraints against stack items

## Actor Model
- `server` word: type instance -> Actor (stateful, uses actor_loop/cast/call protocol)
- `<< ... >>` send protocol; non-vocab tokens run locally, vocab tokens dispatch to actor
- Cast (no returns) = async, Call (has returns) = sync
- Auto-generated ops excluded from vocab — state is private
- Type-checked dispatch: validates arg types in `<<` blocks
- Raw primitives: `spawn` (word -> Actor), `send`/`!`, `receive`, `receive-timeout`
- Message protocol: `msg` creates tagged msgs, `receive-match` does selective receive by tag
- Three non-interfering protocols: raw ({af_msg, _}), server ({cast/call, _}), vocab (<<>>)

## BEAM Compilation (Milestone 8)
- af_word_compiler.erl: simulates stack with typed abstract form expressions `{Expr, Type}`
- **Tagged-value model**: compiled functions take/return `[{Type, Value}, ...]` stack lists
- Pattern matching in function heads (not body matches) for multi-clause dispatch
- After inter-word calls, stack becomes "opaque" — remaining ops dispatch via `apply_impl`
- Same-module words still called directly even in opaque stack mode
- `rest_var` threaded through context to preserve unconsumed stack items
- Multi-clause: same-name words → multi-clause Erlang functions
- Value constraints → literal patterns in head (e.g., `factorial([{Int, 0} | Rest])`)
- `compile` word: on-demand compilation of interpreted words to native BEAM
- make_wrapper: passes full tagged stack to compiled function, gets back modified stack

## OTP Bridge (af_server)
- gen_server wrapping ActorForth interpreter
- Loads .a4 file in init, dispatches call/cast to AF words
- af_term handles bidirectional Erlang <-> AF term conversion at boundaries
- Supervisable, callable via erpc from Elixir

## Structured Errors (af_error)
- `#af_error{}` record: type, message, token, stack, word_trace
- Compiled words push/pop trace frames for call chain
- REPL pretty-prints; af_server returns structured errors

## Python Interop
- erlang_python uses `/home/scherrey/python312/` (Python 3.12), NOT system Python 3.11
- `libpython3.12.so` is at `/home/scherrey/python312/lib/` (needs LD_LIBRARY_PATH for pip)
- Install packages: `LD_LIBRARY_PATH=/home/scherrey/python312/lib pip install ...`
- `requirements.txt` in project root lists direct deps (openai)
- `.env` file has OPENAI_API_KEY; loaded by `af_repl:load_env/0`
- `os:putenv` doesn't propagate to Python — use `py:exec(<<"import os; os.environ[...]=...">>)`
- `py:exec` and `py:eval` have separate scopes — can't share variables between calls
- Sample Python modules in `samples/python/`
- NOTE: Python paths are machine-specific — pixies may differ from current machine

## Test Infrastructure
- ~1900+ EUnit tests after coverage push, 43 CT tests
- Test setups: just call `af_type:reset()` — it calls `af_repl:init_types()` internally
- PropEr property tests: `test/prop_parser_tests.erl`, `test/prop_type_tests.erl`
- CT suites: `af_script_SUITE` (runs .a4 samples including Python demos), `af_integration_SUITE` (actor/multi-word scenarios), `stack_SUITE`
- Python CT tests need CWD set to project root (py-import uses relative paths)
- LLM demo test loads .env and pushes API key into Python's os.environ
- Coverage target: >95% total across all modules

## Ring Architecture — SELF-HOSTED COMPILATION COMPLETE
- **Ring 0** (`af_ring0.erl`): ~80 primitive instructions — stack, data, list, map, string, arithmetic, comparison, logic, control flow, types, pattern matching, actors, I/O, file, tuple, product types, FFI, conversion
- **Ring 1** (`af_ring1.erl`): BEAM target backend. Two modes: interpreted (wraps af_ring0:run) and native (direct abstract forms). Native inlining for: stack ops, arithmetic, comparison, logic, select_clause (case expressions), product types, list ops, map ops, string concat, I/O, assertions. Multi-clause head patterns.
- **Ring 2** (`af_ring2.erl`): A4 compiler → Ring 0. Two paths: parasitic (uses af_parser/interpreter) and selfhosted (uses af_r0_parser/af_r0_compiler)
- **af_r0_parser** (`af_r0_parser.erl`): Self-hosted tokenizer. All A4 syntax including floats, strings, comments
- **af_r0_compiler** (`af_r0_compiler.erl`): Self-hosted word compiler. Words, product types, sub-clauses (Int/Bool/String constraints), file loading, 80+ primitive translations
- **Self-hosted pipeline**: af_r0_parser → af_r0_compiler → af_ring2:compile_selfhosted → af_ring1(native) → BEAM. Zero dependency on af_parser, af_interpreter, af_type_compiler, ETS.
- **ETS eliminated**: Ring 0 `apply_impl` is self-contained — no `af_compile:apply_impl` dependency
- **Native mode default** for Ring 2: `compile_selfhosted` uses `[{mode, native}]`
- **rot/over/2dup inlined**: Ring 1 `translate_body` recognizes patterns and generates direct BEAM code
- **retranslate_bodies recurses into select_clause**: Sub-clause bodies now have `apply_impl` → `call` conversion

## sig_in Convention (CRITICAL)
- **af_r0_compiler stores sig_in as-is from accumulation** (NOT reversed). TOS-first: last token in source = first in list = TOS.
- Ring 1 `build_head_pattern` treats position 0 as TOS
- **Interpreter** reverses source-order to TOS-first: `lists:reverse(SigIn0)`. Same convention.

## Bootstrap — 100% SELF-HOSTED
- `src/bootstrap/parser.a4` — A4 tokenizer in A4 (49 words, ~223 lines)
- `src/bootstrap/compiler.a4` — A4 compiler in A4 (99 words, ~576 lines)
- `src/bootstrap/codegen.a4` — BEAM code generator in A4 (~97 lines). Converts compiler map defs to tuples, calls `af_ring0:compile_interp_module` via xcall
- `src/bootstrap/driver.a4` — Compilation driver in A4 (~64 lines). Orchestrates: read file → parse → compile → codegen
- **Full self-hosted bootstrap verified**: 3-stage test in af_bootstrap_tests.erl
- Only runtime deps: af_ring0.erl (VM), compile:forms (BEAM compiler), code:load_binary (module loader)
- Key codegen fixes: tuple construction (swap cons not rot cons), combine_sig handles value constraints, expand_select_clauses auto-inserts select_clause after clause literals
- Key lessons: setters expect `[NewVal, Instance]` (NewVal=TOS); sig_in TOS-first

## LSP Language Server
- `src/af_lsp.erl` — Erlang transport: stdio JSON-RPC, stack inference (50+ ops), hover/completion/definition
- `src/bootstrap/lsp/lsp_types.a4` — Product types for LSP protocol objects
- `src/bootstrap/lsp/lsp_stack_infer.a4` — Stack inference engine (A4)
- `src/bootstrap/lsp/lsp_server.a4` — LSP method dispatch (A4)
- `editor/sublime/ActorForth.sublime-syntax` — Syntax highlighting for .a4 files
- `editor/sublime/LSP-ActorForth.sublime-settings` — Sublime LSP client config
- `editor/EDITOR_SUPPORT_PLAN.md` — Comprehensive plan for extending editor support
- Killer feature: hover shows stack picture at each token position
- Type-aware completion: suggests ops from TOS type dictionary + Any
- Dependency: `jsx` (pure Erlang JSON library) added to rebar.config
- TODO: Diagnostics (stack underflow detection), incremental inference, multi-file support

## Recent Session Work (2026-03-10)
- Merged `self-hosted` branch into new `a4_version3` branch (off master)
- Resolved 4 merge conflicts (af_interpreter, af_type_any, af_word_compiler, af_word_compiler_tests)
- Massive coverage push: 6 parallel agents boosted coverage from 83% toward >95%
  - af_lsp: 41% → 75% (stdio transport is ceiling for unit tests)
  - af_type_any: 71% → 97%
  - af_word_compiler: 74% → 95.6%
  - af_ring0: 80% → 97%
  - af_type: 80% → 100%
  - af_actor_worker: 68% → 100%
  - af_type_list: 77% → 100%
  - af_type_actor, af_type_compiler, af_type_check, af_type_otp, af_r0_compiler, af_r0_parser, af_type_int: all pushed toward 95%+
- All 1544+ tests passing before coverage push (1900+ after)
- Wrote `editor/EDITOR_SUPPORT_PLAN.md` documenting current LSP features and future plans
- Pre-existing test failure: `af_compiler_tests:sub_clause_test_` — needs investigation

## Roadmap
- DONE: String/List/Map ops, native BEAM compilation, PropEr/CT tests, language docs
- DONE: Self-hosted compilation pipeline (no interpreter dependency)
- DONE: 100% self-hosted bootstrap (all 4 modules compile themselves)
- DONE: LSP language server v0.1 (hover, completion, go-to-definition)
- DONE: Full bootstrap (compiler toolchain written in A4, self-compiling)
- TODO: LSP diagnostics, incremental inference, multi-file support
- TODO: VS Code extension (see editor/EDITOR_SUPPORT_PLAN.md)
- TODO: Guard expressions for type constraints in compilation
- TODO: Core Erlang target
- TODO: Replace ETS in af_type.erl (persistent_term + forget)
- TODO: `forget` word (Forth-style dictionary rollback)

## Known Issues
- Product type constructor sig_in must be `lists:reverse(FieldTypes)`
- Stale beam files: `rebar3 clean && rebar3 compile` if edits don't take effect
- Type registry (ETS) is global — shared across all af_server instances
- `.venv` in project root is broken (built for Python 3.12 but libpython3.12.so not in linker path)
- Pre-existing `af_compiler_tests:sub_clause_test_` failure on a4_version3 — needs investigation

## User Preferences
- Prefers minimalistic, simple design
- Values homoiconicity and self-hosting as core goals
- Wants actors as first-class primitives, not bolted on
- NEVER put credits/co-authored-by in commit messages
- Type definitions: put each field name/type pair on its own line for readability
