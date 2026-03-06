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
- **`src/af_parser.erl`** — Pure tokenizer. `parse(String, Filename) -> [#token{}]`. Handles whitespace, `.` `:` `;` as self-delimiting, `#` comments, `"..."` strings.
- **`src/af_interpreter.erl`** — The outer interpreter. `interpret_token/2` implements the 4-step dispatch above. Pure (no I/O).
- **`src/af_type_int.erl`** — Int type: constructor `int` (Atom->Int), arithmetic `+`,`-`,`*`,`/`
- **`src/af_type_bool.erl`** — Bool type: constructor `bool`, comparisons `==`,`!=`,`<`,`>`,`<=`,`>=`, `not`
- **`src/af_type_any.erl`** — Global ops: `dup`, `drop`, `swap`, `2dup`, `print`, `stack`, `words`, `types`, `load` (file loading)
- **`src/af_type_compiler.erl`** — Word definition compiler. `: name Types -> Types ; body .` Compiled word bodies use late binding (dispatch through interpreter at runtime). Sub-clause pattern matching on Int, Bool, String values with right-aligned partial matching.
- **`src/af_type_product.erl`** — Product type definition. `type Point x Int y Int .` Auto-generates constructor (`point`), non-destructive getters (`x`, `y`), and setters (`x!`, `y!`). Product instances are `{TypeName, #{field => {Type, Val}}}`. Getters leave the instance on the stack.
- **`src/af_type_ffi.erl`** — Erlang FFI: `erlang-apply` (with args list) and `erlang-apply0` (zero args). Calls any Erlang function with automatic term conversion.
- **`src/af_type_string.erl`** — String type wrapping Erlang binaries. Quoted strings auto-convert. `concat`, `length`, `to-atom`, `to-int`, `to-string`.
- **`src/af_type_map.erl`** — Map type wrapping Erlang maps. `map-new`, `map-put`, `map-get`, `map-delete`, `map-has?`, `map-keys`, `map-values`, `map-size`.
- **`src/af_type_list.erl`** — List type wrapping Erlang cons cells. `nil`, `cons`, `length`, `head`, `tail`.
- **`src/af_type_actor.erl`** — Actor model: `server` and `supervised-server` (type instance -> Actor), `<<`/`>>` send protocol, cast/call auto-classification, state privacy via vocab filtering.
- **`src/af_actor_sup.erl`** — OTP supervisor for actor processes. `simple_one_for_one` strategy with `transient` restart.
- **`src/af_actor_worker.erl`** — gen_server wrapper for supervised actors. Handles cast/call messages through ActorForth word dispatch.
- **`src/af_compile.erl`** — Word compilation: closure-based (`compile_word/4`) and BEAM module generation (`compile_module/2`). Optimizes known primitives inline.
- **`src/af_type_check.erl`** — Compile-time type inference. `check_word/4` and `infer_stack/2`. Resolves `Any` to concrete types via substitution.
- **`src/af_type_beam.erl`** — BEAM assembler types. `BeamModule` and `BeamFunction` for building and compiling BEAM modules from ActorForth. Also provides `compile-to-beam` ( Atom Atom -- Atom ) to compile a defined ActorForth word directly to a native BEAM module.
- **`src/af_word_compiler.erl`** — Automatic word-to-BEAM compiler. Takes word definitions (name, sig_in, sig_out, body ops) and compiles them to native BEAM functions via abstract forms. Simulates the stack with expression nodes instead of runtime values. Handles stack ops, arithmetic, comparisons, boolean, and integer literals. Unknown ops fall back to runtime dispatch.
- **`src/af_server.erl`** — gen_server bridge: wraps ActorForth interpreter as OTP citizen. `start_link`, `call`, `cast`, `eval`, `stop`. Term conversion at boundaries.
- **`src/af_term.erl`** — Bidirectional Erlang <-> ActorForth term conversion. `to_stack_item/1`, `from_stack_item/1`.
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
- Compiled word bodies use late binding: each token dispatches through interpreter at runtime
- Compiled words store their body ops in `#operation.source = {compiled, Body}` for later BEAM compilation via `compile-to-beam`
- Tail self-calls in word bodies are detected and restructured for BEAM TCO (trace popped before tail call)
- ActorForth script files use `.a4` extension (see `samples/`)
