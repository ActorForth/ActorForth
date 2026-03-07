# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ActorForth is a homoiconic, concatenative, stack-based, strongly typed language with actor concurrency as a primitive, targeting the Erlang BEAM VM. Prior implementations exist in Python (most complete — had type system, compiler, product types) and C++ (partial — type system and basic interpreter) in earlier git history. This Erlang implementation aims to be the real one: first as an Erlang-hosted interpreter, then compiling to Core Erlang, and ultimately self-hosting with a BEAM assembler written in ActorForth itself.

No inherent syntax beyond primitives — tokens that don't match operations become Atoms on the stack, meaning there are no syntax errors, only type errors. Types own their operation dictionaries; dispatch is by operation name + input stack signature match.

- `docs/ActorForthDefinition.md` — Language specification (references Python env, needs updating)
- `docs/IMPLEMENTATION_PLAN.md` — Full implementation plan with milestones

## Build & Test Commands

```bash
rebar3 compile              # Build
rebar3 eunit --cover        # Run all EUnit tests with coverage
rebar3 eunit --module=X     # Run single test module
rebar3 ct                   # Run Common Test suites
rebar3 cover                # Generate coverage report (HTML in _build/test/cover/)
rebar3 shell                # Start shell, then af_repl:start(). for REPL
```

## Architecture

### Core Dispatch Mechanism

The **outer interpreter** processes each token:
1. Search TOS type's dictionary for the token
2. If not found, check TOS type's **handler** (compiler states intercept here)
3. If no handler, search **Any** (global) dictionary
4. If still not found, push as `{Atom, Value}`

This means the TOS type IS the interpreter's state. The compiler is just four types (`WordDefinition`, `InputTypeSignature`, `OutputTypeSignature`, `CodeCompile`) whose handlers change what tokens mean — no mode switch needed.

### Source Files

- **`include/token.hrl`** — `#token{}` record (value, line, column, file, quoted)
- **`include/continuation.hrl`** — `#continuation{}` record (data_stack, return_stack, current_token, debug, word_trace)
- **`include/operation.hrl`** — `#operation{}` record (name, sig_in, sig_out, impl, source)
- **`include/af_type.hrl`** — `#af_type{}` record (name, ops map, handler)
- **`src/af_type.erl`** — ETS-backed type registry. `find_op/2`, `find_op_in_tos/2`, `find_op_in_any/2`, `find_op_by_name/2`, `match_sig/2`. Central to everything.
- **`src/af_parser.erl`** — Pure tokenizer. `parse(String, Filename) -> [#token{}]`. Handles whitespace, `:` `;` as self-delimiting, `.` as self-delimiting except inside float literals (e.g., `3.14`), `#` comments, `"..."` strings.
- **`src/af_interpreter.erl`** — The outer interpreter. `interpret_token/2` implements the 4-step dispatch above. Pure (no I/O).
- **`src/af_type_int.erl`** — Int type: constructor `int` (Atom->Int), arithmetic `+`,`-`,`*`,`/`
- **`src/af_type_bool.erl`** — Bool type: constructor `bool`, comparisons `==`,`!=`,`<`,`>`,`<=`,`>=`, `not`
- **`src/af_type_any.erl`** — Global ops: `dup`, `drop`, `swap`, `rot`, `over`, `2dup`, `print`, `stack`, `words`, `types`, `see`, `load`, `debug`, `assert`, `assert-eq`, `load` (file loading)
- **`src/af_type_compiler.erl`** — Word definition compiler. `: name Types -> Types ; body .` Compiled word bodies use late binding (dispatch through interpreter at runtime). Sub-clause pattern matching on Int, Bool, String values with right-aligned partial matching.
- **`src/af_type_product.erl`** — Product type definition. `type Point x Int y Int .` Auto-generates constructor (`point`), non-destructive getters (`x`, `y`), and setters (`x!`, `y!`). Product instances are `{TypeName, #{field => {Type, Val}}}`. Getters leave the instance on the stack.
- **`src/af_type_float.erl`** — Float type wrapping Erlang floats. Literal auto-detection (`3.14`), constructor `float`, `to-float`, `to-int`, `to-string`. Full arithmetic with mixed Float/Int operations.
- **`src/af_type_tuple.erl`** — Tuple type wrapping Erlang tuples. `make-tuple`, `to-tuple`, `from-tuple`, `tuple-get`, `tuple-size`, `ok-tuple`, `error-tuple`, `is-ok`, `unwrap-ok`. Essential for Erlang API interop.
- **`src/af_type_ffi.erl`** — Erlang FFI: `erlang-apply` (with args list), `erlang-apply0` (zero args), `erlang-call` (N args from stack), `erlang-call0` (zero args shorthand), `erlang-new`. Calls any Erlang/Elixir function with automatic term conversion.
- **`src/af_type_string.erl`** — String type wrapping Erlang binaries. Quoted strings auto-convert. `concat`, `length`, `to-atom`, `to-int`, `to-string`.
- **`src/af_type_map.erl`** — Map type wrapping Erlang maps. `map-new`, `map-put`, `map-get`, `map-delete`, `map-has?`, `map-keys`, `map-values`, `map-size`.
- **`src/af_type_list.erl`** — List type wrapping Erlang cons cells. `nil`, `cons`, `length`, `head`, `tail`.
- **`src/af_type_actor.erl`** — Actor model: `server` and `supervised-server` (type instance -> Actor), `<<`/`>>` send protocol, cast/call auto-classification, state privacy via vocab filtering. Raw primitives: `spawn` (word -> Actor), `send`/`!` (value Actor ->), `receive` (-> value), `receive-timeout` (ms -> value Bool). `Message` type with `msg` constructor, `msg-tag`/`msg-data` accessors, `receive-match`/`receive-match-timeout` for selective receive by tag. Type-checked dispatch validates args in `<<` blocks.
- **`src/af_actor_sup.erl`** — OTP supervisor for actor processes. `simple_one_for_one` strategy with `transient` restart.
- **`src/af_actor_worker.erl`** — gen_server wrapper for supervised actors. Handles cast/call messages through ActorForth word dispatch.
- **`src/af_compile.erl`** — Word compilation: closure-based (`compile_word/4`) and BEAM module generation (`compile_module/2`). Optimizes known primitives inline.
- **`src/af_type_check.erl`** — Compile-time type inference. `check_word/4` and `infer_stack/2`. Resolves `Any` to concrete types via substitution.
- **`src/af_type_beam.erl`** — BEAM assembler types + high-level compilation words. `compile-to-beam`, `compile-all`, `save-module`, `build-escript`, `build-app`, `build-release`.
- **`src/af_word_compiler.erl`** — Automatic word-to-BEAM compiler. Simulates the stack with abstract form expressions. Multi-clause compilation: same-name words with different signatures (pattern matching) compile to multi-clause Erlang functions with literal patterns for value constraints. Inter-word calls within the same module compile to direct BEAM calls. Cross-module native calls resolved automatically. `make_wrapper/4` generates transparent wrappers. `compile` word (in af_type_any) compiles interpreted words to native BEAM on demand.
- **`src/af_type_otp.erl`** — OTP behaviour generation. `gen-server-module` compiles ActorForth product types into real gen_server modules for OTP supervision trees.
- **`src/af_otp_dispatch.erl`** — Bridges ActorForth word dispatch into gen_server callbacks. `call_word/3`, `cast_word/3`.
- **`src/af_compile_file.erl`** — Standalone .a4 file compiler. `compile/1`, `compile/2`, `compile_to_dir/2`.
- **`src/rebar3_actorforth.erl`** — rebar3 plugin entry point for automatic .a4 compilation.
- **`src/rebar3_actorforth_compile.erl`** — rebar3 provider: compiles .a4 files in src/ to .beam during build.
- **`src/af_server.erl`** — gen_server bridge: wraps ActorForth interpreter as OTP citizen. `start_link`, `call`, `cast`, `eval`, `stop`. Term conversion at boundaries.
- **`src/af_term.erl`** — Bidirectional Erlang <-> ActorForth term conversion. `to_stack_item/1`, `from_stack_item/1`. Handles Int, Float, Bool, String, Atom, Tuple, List, Map, Actor, product types.
- **`src/af_error.erl`** — Structured error records with location, word trace, stack snapshot. `raise/3`, `format/1`.
- **`include/af_error.hrl`** — `#af_error{}` record (type, message, token, stack, word_trace)
- **`src/af_repl.erl`** — Interactive REPL. Wraps interpreter with I/O. Pretty-prints structured errors.
- **`src/stack.erl`** — Generic typed stack operations.

### Key Conventions

- Stack items are `{TypeName :: atom(), Value :: term()}` tuples
- Records defined in `include/*.hrl`, shared via `-include()` — no cross-include between .hrl files
- `#continuation{}` is the central state threaded through all interpretation
- Type modules export `init/0` which registers the type and its operations
- Constructors (like `int`) are registered in the Any dictionary with typed sig_in constraints
- Type-specific ops (like `+`) are registered in that type's dictionary
- Pattern matching via overloaded word signatures preferred over if/else
- Value constraints in signatures: `: factorial 0 Int -> Int ;` creates `{Int, 0}` in sig_in
- Compiled word bodies use late binding: each token dispatches through interpreter at runtime
- Compiled words store their body ops in `#operation.source = {compiled, Body}` for later BEAM compilation via `compile-to-beam` or `compile`
- Tail calls (self-recursive and to other compiled words) are detected and restructured for BEAM TCO
- ActorForth script files use `.a4` extension (see `samples/`)
