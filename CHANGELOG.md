# Changelog

All notable changes to ActorForth are documented in this file.

ActorForth has been (partially) implemented three times across three languages
since New Year's Eve 2018 on a vacation in Bali. Each attempt
deepened the understanding of what the language needed to be. The Python
implementation proved the core semantics. The C++ attempt validated the type
system in a compiled context but stalled on tooling. The Erlang implementation
is the real one — self-hosted, compiling to BEAM, with actors as primitives.

---

## [Unreleased] — Standalone CLI & Cross-Platform Distribution

### Added
- `af_cli.erl` — standalone `a4c` command-line tool with `compile`, `build`, `run`, and `repl` subcommands
- Zig wrapper that embeds an OTP release into a single native binary with zstd compression and cache-based extraction
- Cross-compilation support for Linux, macOS, and Windows via `zig-wrapper/build.sh --target`
- OTP release configuration (`rebar.config` relx, `config/sys.config`, `config/vm.args`)
- Forth-style aliases: `pop` (drop), `.` (print TOS with type), `.s` (show stack)
- Skip list data structure demo (`test/demos/skip_list/`)
- `run_tests.sh` — project test runner script
- 1,956 tests passing, 91% coverage

---

## [0.10.0] — 2026-03-10 — Self-Hosted Master

The compiler toolchain compiles itself. All roads merge here.

### Added
- **100% self-hosted bootstrap**: parser, compiler, code generator, and driver — all written in ActorForth — compile themselves through a 3-stage verified pipeline
- Bootstrap modules: `parser.a4` (49 words), `compiler.a4` (99 words), `codegen.a4`, `driver.a4`
- Only runtime dependencies: `af_ring0.erl` (VM), `compile:forms` (BEAM compiler), `code:load_binary` (loader)
- 1,951 EUnit tests, 43 CT tests, 95% total coverage, 15 modules at 100%

### Merged
- `self-hosted` branch into `a4_version3`, then into `master`
- C++ and TypeScript cross-language targets into `master`

---

## [0.9.0] — 2026-03-09 — Ring Architecture & Self-Hosted Compilation

### Added
- **Ring 0** (`af_ring0.erl`): ~80 primitive instructions forming a minimal virtual machine — stack, arithmetic, comparison, logic, control flow, types, pattern matching, actors, I/O, FFI, tuples, product types
- **Ring 1** (`af_ring1.erl`): BEAM target backend with two modes — interpreted (wraps Ring 0) and native (direct Erlang abstract forms). Native inlining for ~30 operation categories
- **Ring 2** (`af_ring2.erl`): A4-to-Ring-0 compiler with parasitic and self-hosted paths
- **Self-hosted parser** (`af_r0_parser.erl`): tokenizer reimplemented without `af_parser` dependency
- **Self-hosted compiler** (`af_r0_compiler.erl`): word compiler with product types, sub-clauses (Int/Bool/String constraints), file loading, 80+ primitive translations
- ETS dependency eliminated — Ring 0 `apply_impl` is fully self-contained
- Self-hosted `.beam` file generation via `compile_to_dir_selfhosted`
- Working A4 parser written in A4 itself

---

## [0.8.0] — 2026-03-09 — Cross-Language Targets

### Added
- **C++ target** (`af_cpp_compiler.erl`): A4-to-C++20 compiler with coroutine-based actor runtime, stack/arithmetic/comparison/logic/list/map operation categories, 8 tests
- **TypeScript target**: full interpreter with JS FFI, compiler, 182 tests at >90% coverage
- Cross-language benchmark suite with equivalent programs in A4, Python, C++, Erlang, and Elixir
- Unified benchmark runner (`run_all_benchmarks.py`) with comparison tables

---

## [0.7.0] — 2026-03-09 — Compiler Optimizations & Named Type Variables

### Added
- Named type variables (`_name`) for positional type tracking in signatures — `swap` is `_a _b -> _b _a` where each variable is tracked independently through permutations
- Native BEAM compilation for ~30 type-specific operations
- Loop unboxing, native actor cache, GC tuning
- Higher-order list operations, standard library words, auto-compile on demand
- Local dictionary dispatch (type dictionary moved into continuation)

---

## [0.6.0] — 2026-03-07 — Typed Actor Messages & Multi-Clause BEAM Compilation

### Added
- **Typed actor messages**: `Message` type with `msg` constructor, `msg-tag`/`msg-data` accessors, `receive-match`/`receive-match-timeout` for selective receive by tag
- Multi-clause BEAM compilation: same-name words with different signatures compile to multi-clause Erlang functions with literal patterns for value constraints
- BEAM compiler refactored to tagged-value model — compiled functions take/return `[{Type, Value}, ...]` stack lists
- Comprehensive test coverage push to 90%+

---

## [0.5.0] — 2026-03-07 — OTP Integration & Python Bridge

### Added
- **OTP behaviour generation** (`af_type_otp.erl`): `gen-server-module` compiles ActorForth product types into real gen_server modules
- **OTP application packaging**: `build-app` and `build-release` words
- **rebar3 plugin** (`rebar3_actorforth.erl`): automatic `.a4` compilation during build
- **Standalone `.a4` compiler** (`af_compile_file.erl`)
- **Python-BEAM integration** via erlang_python for AI/bot development
- Working OpenAI integration with `.env` key management
- `.env` loading, tail call optimization for word calls, `import` word
- Compile-time type checking and inference (`af_type_check.erl`)

---

## [0.4.0] — 2026-03-07 — BEAM Compilation & Native Execution

### Added
- **Automatic word-to-BEAM compilation** (`af_word_compiler.erl`): stack simulation with typed abstract form expressions, tail call detection (self-recursive and inter-word), transparent native wrappers
- **Core Erlang compilation** target
- **OTP supervision** for actor processes (`af_actor_sup.erl`, `af_actor_worker.erl`)
- **Float type** (`af_type_float.erl`): literal auto-detection, mixed Float/Int arithmetic
- **Tuple type** (`af_type_tuple.erl`): `make-tuple`, `ok-tuple`, `error-tuple`, `is-ok`, `unwrap-ok` — essential for Erlang API interop
- **Erlang FFI** (`af_type_ffi.erl`): `erlang-apply`, `erlang-call`, `erlang-new` — call any Erlang/Elixir function
- `see` word for word introspection
- Inter-word native compilation within same module

---

## [0.3.0] — 2026-03-07 — Rich Types & Structured Errors

### Added
- **String type** (`af_type_string.erl`): wrapping Erlang binaries, quoted strings auto-convert, `concat`, `length`, `to-atom`, `to-int`
- **Map type** (`af_type_map.erl`): wrapping Erlang maps, `map-new`, `map-put`, `map-get`, `map-delete`, `map-has?`, `map-keys`, `map-values`
- **List type** (`af_type_list.erl`): `nil`, `cons`, `head`, `tail`, `length`
- **Structured errors** (`af_error.erl`): `#af_error{}` record with type, message, token, stack, word trace
- **af_server** (`af_server.erl`): gen_server bridge wrapping ActorForth interpreter as OTP citizen
- **Term conversion** (`af_term.erl`): bidirectional Erlang <-> ActorForth
- Non-destructive product type getters (leave instance on stack)
- File loading (`load` word)
- Pattern matching sub-clauses with value constraints on Int, Bool, String

---

## [0.2.0] — 2026-03-06 — Actor Model & Type-Driven Dispatch

### Added
- **Actor model** (`af_type_actor.erl`): `server` word (type instance -> Actor), `<<`/`>>` send protocol, cast/call auto-classification, state privacy via vocabulary filtering
- Raw actor primitives: `spawn`, `send`/`!`, `receive`, `receive-timeout`
- **Product types** (`af_type_product.erl`): `type Name field Type .` with auto-generated constructors, getters, setters
- **Word definition compiler** (`af_type_compiler.erl`): `: name Types -> Types ; body .` with late-binding dispatch
- **Bool type** (`af_type_bool.erl`): comparisons `==`, `!=`, `<`, `>`, `<=`, `>=`, `not`
- **Int type** (`af_type_int.erl`): constructor, arithmetic `+`, `-`, `*`, `/`
- Literal handlers for automatic Atom-to-Int conversion
- sig_in convention fixed to standard Forth: leftmost=deepest, rightmost=TOS
- Global operations in Any dictionary: `dup`, `drop`, `swap`, `rot`, `over`, `2dup`, `print`, `stack`, `words`, `types`, `assert`, `assert-eq`

---

## [0.1.0] — 2025-01-17 — Erlang Implementation Begins

After a two-year gap following the C++ attempt, the project restarts on the
Erlang BEAM VM — the natural home for a language with actor concurrency as a
primitive.

### Added
- Initial Erlang project with rebar3
- Token record (`include/token.hrl`), continuation record (`include/continuation.hrl`)
- Parser (`af_parser.erl`): pure tokenizer handling whitespace, self-delimiting punctuation, comments, quoted strings
- Interpreter (`af_interpreter.erl`): 4-step outer interpreter — search TOS type dict, check handler, search Any dict, push as Atom
- REPL (`af_repl.erl`)
- ETS-backed type registry (`af_type.erl`)
- Stack module (`stack.erl`)
- EUnit test suite

### Why Erlang?
The Python implementation proved the language design but hit a wall: actor
concurrency had to be bolted on. The C++ implementation (see below) tried to
get closer to the metal but got stuck on build tooling and product type
dynamics. Erlang's BEAM VM provides exactly what ActorForth needs natively:
lightweight processes, message passing, pattern matching, hot code loading, and
OTP supervision. Actors are no longer a feature to implement — they're the
substrate.

---

## C++ Implementation — 2021-01-09 to 2023-08-26

> **Status: Abandoned.** Tagged as `last-cpp-master`.
>
> The C++ implementation validated that the type system and stack signature
> dispatch model work in a compiled, statically-typed context. It got further
> than expected — a working REPL with primitive types, arithmetic, and product
> types — but ultimately stalled on the gap between C++'s type system and
> ActorForth's dynamic product type requirements. The `filepos` branch spent
> months trying to make dynamic attribute access work cleanly with C++ structs
> and never fully resolved it. Build tooling friction (coroutine library
> changes, clang version drift) added to the burden.

### Added
- Parser with coroutine-based token generation (`co_yield`)
- Stack class with signature matching
- Type system: Any, Atom, Bool, Int, String with operations
- REPL and interpreter
- Product types with attributes and instances
- doctest unit testing with 100% stack coverage
- Parallel builds, Windows compatibility

### What was learned
- Coroutine-based parsing is elegant but fragile across compiler versions
- C++ product types need runtime dynamism that fights the language's grain
- The build tooling problem is real — 2+ years of clang drift broke the build

---

## Python Implementation — 2018-12-31 to 2020-09-24

> **Status: Superseded.** Tagged as `last-python-master`.
>
> The Python implementation is where ActorForth was designed. It proved every
> core concept: type-driven dispatch, stack signature matching, the
> compiler-as-type-handlers insight, product types with auto-generated
> operations, pattern matching via overloaded signatures, and file loading.
> It never got actor concurrency — that was always the goal but Python's
> threading model made it impractical. The Python implementation remains the
> most thorough reference for the language semantics.

### 2020-09 — Product Types & Application Code
- User-defined types with automatic constructors and named attribute accessors
- List type implementation
- BCH SLP transaction parsing (`af_bchslp.py`) — first real-world application code
- Loadable `.a4` library files (`btc.a4`)

### 2020-08 — Flow Control & Code Loading
- Return stack for flow control
- Program counter in continuation
- Branching operations (`af_branch.py`)
- Looping implementation
- String parsing and comments
- `assert` operation
- File loading (`load` word) — programs can load other programs
- `countdown.a4`, `testloop.a4` sample programs

### 2020-07 — Pattern Matching
- `WordDeclaration` and `TMatchPattern` types
- Pattern matching syntax and compilation
- Generic type specialization
- `find_ops_named_this_for_scope` — scoped operation lookup
- **`fib()` finally works** — the moment pattern matching proved itself
- `WhyActorForth.odp` presentation created

### 2020-06 — Compiler Hardening
- Literal compilation working — "All unit tests pass. No more skips"
- `debug` on/off toggle
- `see` word for introspection
- `check_stack_effect` with value matching
- INTRO documentation stages 1-5

### 2020-03 — Compilation Works
- **"Goodness gracious - it compiles and runs"** — compiled words executing for the first time
- `Operation` class for compiled words
- `continuation.py` — central state threading
- Compiled words composing (calling other compiled words)
- Literals and type handler compilation

### 2020-02 — Core Language Design
- Word dictionary refactored: all dictionaries live inside Type class
- `TypeSignature` for stack type checking
- Constructor registration in Any dictionary
- Interpret/compile mode switching
- Bool type with equality and comparison operators
- `dup`, `swap`, `drop`, `2dup` stack operations
- Compiler types (`TWordDefinition`, input/output signature types)
- Word definition syntax: `: name InputTypes -> OutputTypes ; body .`
- `ActorForthDefinition.md` — the language specification

### 2020-01 — REPL & Interpreter
- REPL with stdin reading
- Int arithmetic (`+`, `-`, `*`, `/`)
- Generic type matching for operations
- File parameter support

### 2019 — Foundations
- Parser with 100% test coverage
- Literal and Nat types
- Initial whitepaper
- `primitives.py` — first operation definitions
- `access_token.a4` — state machine actor design sketch (aspirational)
- Dataclass refactor, mypy type checking
- Stack and operation signature matching prototypes

### 2018-12-31 — Project Inception
- Initial commit
- "barely getting started."

---

## Legend

- **Ring 0/1/2**: The self-hosting architecture. Ring 0 is the primitive VM,
  Ring 1 is the BEAM code generator, Ring 2 is the A4 compiler. Each ring
  only depends on the one below it.
- **Self-hosted**: Code written in ActorForth itself, compiled by ActorForth.
- **BEAM**: The Erlang virtual machine (Bogdan/Björn's Erlang Abstract Machine).
- **Product type**: A user-defined composite type, like a struct.
- **TOS**: Top of stack — the value that determines which type dictionary to search.
- **sig_in / sig_out**: Input and output stack signatures that define a word's type contract.
