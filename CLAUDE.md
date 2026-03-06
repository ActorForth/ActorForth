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

- **`include/token.hrl`** — `#token{}` record (value, line, column, file)
- **`include/continuation.hrl`** — `#continuation{}` record (data_stack, return_stack, current_token)
- **`include/operation.hrl`** — `#operation{}` record (name, sig_in, sig_out, impl, source)
- **`include/af_type.hrl`** — `#af_type{}` record (name, ops map, handler)
- **`src/af_type.erl`** — ETS-backed type registry. `find_op/2`, `find_op_in_tos/2`, `find_op_in_any/2`, `find_op_by_name/2`, `match_sig/2`. Central to everything.
- **`src/af_parser.erl`** — Pure tokenizer. `parse(String, Filename) -> [#token{}]`. Handles whitespace, `.` `:` `;` as self-delimiting, `#` comments, `"..."` strings.
- **`src/af_interpreter.erl`** — The outer interpreter. `interpret_token/2` implements the 4-step dispatch above. Pure (no I/O).
- **`src/af_type_int.erl`** — Int type: constructor `int` (Atom->Int), arithmetic `+`,`-`,`*`,`/`
- **`src/af_type_bool.erl`** — Bool type: constructor `bool`, comparisons `==`,`!=`,`<`,`>`,`<=`,`>=`, `not`
- **`src/af_type_any.erl`** — Global ops: `dup`, `drop`, `swap`, `2dup`, `print`, `stack`, `words`, `types`
- **`src/af_type_compiler.erl`** — Word definition compiler. `: name Types -> Types ; body .` Compiled word bodies use late binding (dispatch through interpreter at runtime).
- **`src/af_type_product.erl`** — Product type definition. `type Point x Int y Int .` Auto-generates constructor (`point`), getters (`x`, `y`), and setters (`x!`, `y!`). Product instances are `{TypeName, #{field => {Type, Val}}}`.
- **`src/af_type_actor.erl`** — Actor primitives mapped to Erlang processes. `spawn` (Atom->Actor), `self` (->Actor), `send`/`!` (Any Actor->), `receive` (->Any). Each actor has its own continuation; messages are typed stack items.
- **`src/af_repl.erl`** — Interactive REPL. Wraps interpreter with I/O.
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
- ActorForth script files use `.a4` extension (see `samples/`)
