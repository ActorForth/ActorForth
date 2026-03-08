# ActorForth Erlang Implementation Plan

## Vision

ActorForth is a homoiconic, strongly-typed, concatenative, stack-based language with actor concurrency as a primitive. It targets the Erlang BEAM VM. The implementation progresses in phases:

1. **Prototype** — ActorForth interpreter written in Erlang
2. **Self-hosting** — ActorForth compiler that emits BEAM bytecode, written in ActorForth itself
3. **Bootstrap** — The language contains its own BEAM assembler and can compile itself

This plan covers Phase 1: a working prototype interpreter in Erlang that implements the core language semantics proven in the Python implementation, extended with actor primitives that leverage BEAM's native process model.

## What We Learned From Prior Implementations

### Python (most complete)
- Type-driven dispatch works: operations looked up by name + TOS type, with "Any" as generic fallback
- Stack signatures (input types -> output types) are the core contract mechanism
- Compilation is interpretation: the compiler is just another set of type handlers that change what tokens mean (type names vs word references vs pattern constraints)
- Word definition state machine: `: name InputTypes -> OutputTypes ; body .`
- Product types (user-defined types) with named attributes, auto-generated constructors/getters/setters
- Pattern matching via overloaded words with different stack signatures
- No actor/concurrency features were implemented

### C++ (partial)
- Confirmed the type registry + stack signature dispatch model works in a compiled context
- Product type attributes with positional storage + name lookup
- Got stuck on build tooling and didn't complete the compiler/word-definition system

### Key Design Decisions Carried Forward
- Everything is a typed stack item: `{Type, Value}`
- Operations dispatch on name + input stack signature (TOS type context)
- "Any" type is the generic matcher
- Unrecognized tokens become Atoms (no syntax errors, only type errors)
- Types own their operation dictionaries
- Compilation uses the same execution engine with different type handlers

## Architecture Overview

```
                    Input Stream (text/file)
                           |
                      [ Parser ]
                           |
                    Token stream
                           |
                   [ Interpreter ]
                      |        |
              Word Lookup    make_atom
              (by name +     (fallback)
               stack sig)
                      |
                 [ Operation ]
                      |
              Continuation transform
              (stack + next_op + token)
                      |
                 [ Continuation ]
                 data_stack, return_stack,
                 next_op, current_token
```

For actors:
```
    ActorForth Process        ActorForth Process
    +-----------------+       +-----------------+
    | Continuation    |       | Continuation    |
    | (own stacks)    |       | (own stacks)    |
    | Mailbox (BEAM)  |  <->  | Mailbox (BEAM)  |
    +-----------------+       +-----------------+
         Erlang Process            Erlang Process
```

Each ActorForth actor IS an Erlang process with its own continuation (stacks + execution state). Message passing maps directly to Erlang message passing.

---

## The Inner and Outer Interpreters

Traditional Forth has two interpreters: the **outer interpreter** reads tokens and decides what to do with them, and the **inner interpreter** executes compiled word bodies. ActorForth unifies these through its type-driven dictionary mechanism, making the "compiler" almost trivially simple — just a few words.

### The Outer Interpreter

The outer interpreter is the token processing loop. For each token it:

1. Looks at the TOS type to determine the **current dictionary context**
2. Searches that type's dictionary for the token
3. Falls back to the global ("Any") dictionary
4. If still not found, pushes the token as an `{Atom, "token-text"}` — there are no syntax errors

This is the entire mechanism. The TOS type **is** the interpreter's state. There is no separate "STATE variable" like in traditional Forth that switches between interpreting and compiling. Instead, when `:` pushes a `WordDefinition` type onto the stack, the dictionary context changes. Now every subsequent token is looked up in `WordDefinition`'s dictionary first, which contains words like `->` that know how to accumulate type signatures. When `->` transitions to output signatures, it changes the TOS type to `OutputTypeSignature`. When `;` starts code compilation, it changes TOS to `CodeCompile`. Each of these types has a small dictionary of words that know what tokens mean **in that context**.

This is why building a compiler in ActorForth takes only a few words: each compilation phase is just a type with a handful of operations. The outer interpreter doesn't change — it always does the same thing. The types on the stack change what the tokens *mean*.

```
Normal interpretation:     TOS is Int, Bool, Atom, etc.
                           Tokens looked up as executable operations

: myword                   TOS becomes WordDefinition
                           Next token captured as word name

  Int Int                  TOS becomes InputTypeSignature
                           Tokens looked up as type names, accumulated into input sig

  ->                       Found in InputTypeSignature's dictionary
                           TOS becomes OutputTypeSignature

  Int                      Token accumulated into output sig

  ;                        Found in OutputTypeSignature's dictionary
                           TOS becomes CodeCompile

  dup +                    Tokens resolved to operation references and appended
                           to compiled word body (not executed, just recorded)

  .                        Found in CodeCompile's dictionary
                           Saves compiled word, pops compiler state off stack
                           Back to normal interpretation
```

The entire compiler is: the `:` word, the `->` word, the `;` word, and the `.` word, plus the type dictionaries for `WordDefinition`, `InputTypeSignature`, `OutputTypeSignature`, and `CodeCompile`. That's it. Any new syntax can be created the same way — define a type whose dictionary gives tokens new meaning.

### The Inner Interpreter

The inner interpreter executes compiled word bodies. A compiled word is a list of operation references. Execution steps through this list, applying each operation to the continuation.

In Erlang, this is naturally a fold over the operation list:

```erlang
execute_word([], Cont) -> Cont;
execute_word([Op | Rest], Cont) ->
    NewCont = apply_op(Op, Cont),
    execute_word(Rest, NewCont).
```

When a compiled word calls another compiled word, the return address (remaining operations) is pushed onto the return stack. When that word finishes, execution resumes from the return stack. This is identical to traditional Forth threaded code.

The inner interpreter is also where **pattern matching dispatch** happens. When a word has multiple definitions with different stack signatures, the inner interpreter selects the most specific match:

```
: factorial  0 Int -> Int ; drop 1 .
: factorial  Int -> Int ; dup 1 - factorial * .
```

Both are registered under the name `factorial` in the `Int` type dictionary. When `factorial` is invoked, the dispatcher tries each definition's input signature against the actual stack. The definition with `0 Int` (value + type constraint) is more specific than `Int` alone, so it's tried first. This replaces `if` statements with declarative pattern matching — the same word name, different behaviors based on what's on the stack.

Prefer this pattern matching style over explicit branching wherever possible:

```
# PREFER: pattern matching via overloaded signatures
: collatz  1 Int -> Int ; .                          # base case
: collatz-even  Int -> Int ; 2 / collatz .           # even case
: collatz-odd   Int -> Int ; 3 * 1 + collatz .       # odd case

# AVOID: explicit if/else branching
: collatz  Int -> Int ; dup 1 == if ... else ... then .
```

Pattern matching is more declarative, composes better, and maps naturally to BEAM's native pattern matching when we compile to Core Erlang.

### Why This Matters for Self-Hosting

Because the compiler is just types with dictionaries, and types can be defined in ActorForth itself (via Product Types and word definitions), the language can define its own compiler. The BEAM assembler is just another set of types whose dictionaries give tokens meaning in the context of emitting bytecode. A `BeamModule` type on the stack means tokens are interpreted as module-level directives. A `BeamFunction` type means tokens are function-level instructions. The same outer interpreter drives it all.

---

## Phase 1: Core Language (Erlang Prototype)

### Milestone 1: Type System & Operation Dispatch

**Goal:** Replace the current "push everything as atom" interpreter with proper type-driven dispatch.

#### 1.1 Type Registry (`src/af_type.erl`)

The type registry is a central ETS table (or module-level state via a gen_server) that maps type names to their operation dictionaries.

```erlang
%% Type representation
-record(af_type, {
    name        :: atom(),           %% e.g. 'Atom', 'Int', 'Bool', 'Any'
    ops         :: #{op_name() => [operation()]},  %% name -> list of ops (overloaded)
    handler     :: fun((continuation()) -> continuation()),  %% execution handler
    attributes  :: [attribute()]     %% for product types
}).

%% Stack item — the universal currency
-type stack_item() :: {TypeName :: atom(), Value :: term()}.

%% Operation with stack signature
-record(operation, {
    name        :: string(),
    sig_in      :: [type_constraint()],   %% required input stack types
    sig_out     :: [type_constraint()],   %% declared output types
    impl        :: fun((continuation()) -> continuation()) | [operation()],  %% native or compiled
    source      :: #token{}               %% where defined
}).

%% Type constraint in a signature
-type type_constraint() :: atom()          %% type name (e.g. 'Int')
                         | {atom(), term()} %% type + specific value
                         | 'Any'.           %% matches anything
```

Key behaviors:
- `af_type:register(Name, Opts)` — register a new type
- `af_type:find_op(TypeName, OpName, Stack)` — find operation matching name + current stack signature
- `af_type:add_op(TypeName, Operation)` — add operation to a type's dictionary
- Global "Any" type operations are checked as fallback for all types
- Type lookup order: TOS type dictionary -> "Any" dictionary

#### 1.2 Stack Signature Matching (`src/af_type.erl`)

The matching algorithm from Python:
1. Get the TOS type
2. Look up operations with matching name in that type's dictionary
3. For each candidate, check if the stack satisfies the input signature
4. "Any" in a signature matches any type
5. If a specific value is in the constraint, it must match too
6. If no match in TOS type, fall back to "Any" type dictionary
7. If still no match, apply `make_atom` (push as Atom)

#### 1.3 Built-in Types (`src/af_types/`)

Each type module registers itself and its operations:

- **`af_type_atom.erl`** — The Atom type. No operations of its own. Created by `make_atom` when a token doesn't match any operation.
- **`af_type_int.erl`** — Constructor `int` takes `Atom` -> `Int`. Operations: `+`, `-`, `*`, `/`
- **`af_type_bool.erl`** — Constructor `bool` takes `Atom` -> `Bool`. Operations: `not`, `==`, `!=`, `<`, `>`, `<=`, `>=`
- **`af_type_any.erl`** — Global operations: `dup`, `drop`, `swap`, `2dup`, `print`, `stack`, `words`, `types`, `debug`

### Milestone 2: Parser

**Goal:** Proper tokenizer with source location tracking.

#### 2.1 Parser (`src/af_parser.erl`)

Tokenization rules (from Python impl):
- Whitespace delimiters: space, tab, newline
- Special punctuation tokens (self-delimiting): `.` `:` `;`
- Comments: `#` to end of line (discarded or kept as metadata)
- Quoted strings: `"..."` preserved as single token
- Everything else: accumulated as a word token
- Each token tagged with `{Value, Line, Column, Filename}`

The parser should be a pure function: `parse(InputString, Filename) -> [#token{}]`

For REPL use, also support incremental/line-at-a-time parsing.

### Milestone 3: Interpreter Core

**Goal:** The interpret loop with proper type-driven dispatch.

#### 3.1 Interpreter (`src/af_interpreter.erl`)

Replaces current `repl.erl`. The core loop:

```
interpret_token(Token, Continuation) ->
    1. Look up Token.value as operation name against current stack
    2. If found: execute operation, return new Continuation
    3. If not found: push {atom, Token.value} onto data_stack
    4. Return updated Continuation
```

The continuation record needs to be extended:

```erlang
-record(continuation, {
    data_stack    :: [stack_item()],
    return_stack  :: [term()],
    next_op       :: op_ref() | 'end',
    current_token :: #token{},
    debug         :: boolean(),
    call_depth    :: non_neg_integer()
}).
```

#### 3.2 REPL (`src/af_repl.erl`)

Interactive read-eval-print loop:
- Reads a line from stdin
- Parses into tokens
- Interprets each token against current continuation
- Prints prompt with status
- Handles `^C` gracefully

### Milestone 4: Word Definition (Compiler)

**Goal:** User-defined words with type signatures, compiled within the interpreter.

The compiler is not a separate system — it's just four words (`:`, `->`, `;`, `.`) and four types whose dictionaries give tokens context-dependent meaning. See "The Inner and Outer Interpreters" section above for the full explanation of why this works.

```
: word-name  InputType1 InputType2 -> OutputType1 ; body-word1 body-word2 .
```

#### 4.1 Compiler Types (`src/af_types/af_type_compiler.erl`)

Each compiler phase is a type. The outer interpreter doesn't change — it always does TOS-dictionary lookup. These types just redirect what tokens mean:

| Type on TOS | Pushed by | Tokens mean | Transitions to |
|---|---|---|---|
| `WordDefinition` | `:` | Next token = word name | `InputTypeSignature` |
| `InputTypeSignature` | (after name capture) | Type names for input sig | `OutputTypeSignature` (via `->`) |
| `OutputTypeSignature` | `->` | Type names for output sig | `CodeCompile` (via `;`) |
| `CodeCompile` | `;` | Word references to compile | (pops all via `.`) |

Each of these types has a tiny dictionary — typically just the transition word and a default handler for accumulating. The `CodeCompile` handler is the most interesting: it resolves each token to an operation reference (by looking it up against the declared input types) and appends it to the word body being built. Unresolved tokens compile as `make_atom`.

The `.` word in `CodeCompile`'s dictionary finishes compilation: it pops all the compiler state off the stack, packages the accumulated operation list + signatures into an Operation record, and registers it in the appropriate type's dictionary (determined by the first input type in the signature, or "Any" if none).

#### 4.2 Pattern Matching via Overloaded Signatures

Multiple definitions of the same word with different signatures:

```
: factorial 0 Int -> Int ; drop 1 .
: factorial Int -> Int ; dup 1 - factorial * .
```

Both are registered under `factorial` in `Int`'s dictionary. At dispatch time, the most specific matching signature wins — value constraints (`0 Int`) beat type-only constraints (`Int`). This is the preferred control flow mechanism over if/else branching.

### Milestone 5: Product Types (User-Defined Types)

**Goal:** Named composite types with attributes.

```
type Point x Int y Int .
10 5 point              # constructor (lowercase = type name)
x                       # getter — pushes x value
Point -> x 100 =        # setter via reference
```

#### 5.1 Product Type Definition (`src/af_types/af_type_usertype.erl`)

When `type` is invoked:
1. Next token = type name (must start uppercase)
2. Subsequent tokens = attribute pairs: `name Type name Type ...`
3. `.` finishes definition

Auto-generated operations:
- **Constructor** (lowercase type name): takes N typed values from stack, creates instance
- **Getters**: one per attribute, pushes attribute value
- **Reference + Setter**: `->` creates reference, attribute name with reference = setter

Internal representation:
```erlang
-record(product_instance, {
    type_name :: atom(),
    values    :: #{atom() => stack_item()}   %% attribute_name => typed value
}).
```

### Milestone 6: Control Flow

**Goal:** Branching and looping via pattern matching and recursion.

The primary control flow mechanism is **pattern matching via overloaded word signatures**, not if/else branching. This is both more natural for a strongly-typed stack language and maps directly to BEAM's native pattern matching when we eventually compile.

#### 6.1 Pattern Matching Dispatch (Primary)

Multiple definitions of the same word with different stack signatures:

```
: abs  Int -> Int ; dup 0 < not .            # non-negative: identity
: abs  Int -> Int ; dup 0 < swap 0 swap - .  # negative: negate
```

More specific signatures (value constraints) are tried before general ones. This is the preferred way to express conditional logic.

#### 6.2 Tail-Recursive Words (Primary)

Recursion with pattern matching base cases — natural on BEAM:

```
: factorial  0 Int -> Int ; drop 1 .
: factorial  Int -> Int ; dup 1 - factorial * .
```

The inner interpreter should detect tail position calls and optimize (or rely on BEAM's tail call optimization once we compile).

#### 6.3 Return-Stack Loops (Secondary)

For cases where imperative looping is genuinely clearer:
- **`countdown`/`loop`** — Return-stack loop counter (from Python impl)

#### 6.4 Traditional Branching (Low Priority)

- **`if`/`else`/`then`** — Only if pattern matching proves insufficient for some use case. These are compile-time words in `CodeCompile`'s dictionary that emit branch operations into the compiled word body. Avoid reaching for these when pattern matching works.

---

## Phase 2: Actor Primitives

### Milestone 7: Actor Model

**Goal:** Each ActorForth actor is an Erlang process with its own continuation.

This is where ActorForth diverges from traditional Forth and leverages BEAM's killer feature.

#### 7.1 Actor Primitives

| Word | Signature | Description |
|---|---|---|
| `spawn` | `( word -- Actor )` | Spawn new process executing word, returns actor reference |
| `send` | `( value Actor -- )` | Send typed value to actor's mailbox |
| `receive` | `( -- value )` | Block until message arrives, push onto stack |
| `self` | `( -- Actor )` | Push current actor's reference |
| `!` | `( value Actor -- )` | Alias for send |
| `?` | `( Actor -- value )` | Request/reply pattern |

#### 7.2 Actor Implementation (`src/af_actor.erl`)

Each actor is:
```erlang
actor_loop(Continuation, WordOps) ->
    receive
        {msg, Value} ->
            %% Push Value onto data_stack, execute actor's word
            NewCont = push_and_execute(Value, Continuation, WordOps),
            actor_loop(NewCont, WordOps);
        {request, From, Value} ->
            NewCont = push_and_execute(Value, Continuation, WordOps),
            {Reply, FinalCont} = pop_reply(NewCont),
            From ! {reply, Reply},
            actor_loop(FinalCont, WordOps);
        stop -> ok
    end.
```

Key design decisions:
- Actor state = its continuation (stacks persist between messages)
- Messages are typed stack items
- An actor's "behavior" is a compiled word that processes each message
- Supervision trees can be built as ActorForth words wrapping OTP supervisor

#### 7.3 Actor Types

**Status: Done.**

New types and primitives for the actor system:

- **`Actor`** — wraps an Erlang pid, typed by what messages it accepts. Created by `server`, `supervised-server`, or `spawn`.
- **`Message`** — typed envelope for inter-actor communication. Created by `msg` from a value and tag string. Non-destructive accessors `msg-tag` and `msg-data`.
- **Selective receive** via `receive-match` (by message tag) and `receive-match-timeout`.
- **Raw actor primitives**: `spawn` (word name -> Actor), `send`/`!` (value Actor ->), `receive` (-> value), `receive-timeout` (ms -> value Bool).
- **Type-checked dispatch**: `<<` blocks now validate argument types against vocab signatures, raising errors for mismatches.

---

### Milestone 7.5: Python Interop

**Goal:** Call Python code from ActorForth via the `erlang_python` library, enabling access to Python's AI/ML ecosystem.

**Status: Done.**

#### 7.5.1 Python Bridge (`src/af_type_python.erl`)

Words for embedding Python in the BEAM VM:
- `py-start` — Ensure the Python runtime is started
- `py-call` — Call `module.function(args...)` with N args from the stack
- `py-call0` — Zero-arg shorthand for `module.function()`
- `py-eval` — Evaluate a Python expression, push result
- `py-exec` — Execute Python statements (no return)
- `py-import` — Add directory to Python's `sys.path`
- `py-venv` — Activate a Python virtual environment
- `py-register` — Expose a compiled ActorForth word as callable from Python

Key design: `af_to_python/1` converts AF stack items to Python-friendly values; `py_to_stack_item/1` converts results back. Module-level state (via `py:call`) is the working pattern since `py:exec` and `py:eval` run in separate scopes.

#### 7.5.2 Sample Python Modules

- `samples/python/text_tools.py` — Word count, slugify, reverse words, Levenshtein distance
- `samples/python/ai_stub.py` — Fake embeddings, cosine similarity, JSON parsing
- `samples/python/llm_client.py` — OpenAI/Anthropic chat, embeddings, structured output
- `samples/python/http_client.py` — REST client with auth and rate limiting
- `samples/python/worker.py` — Stateful key-value store
- `samples/python/actor_bridge.py` — Module-level state pattern for actor-like Python objects

### Milestone 7.6: Version 2.3 Improvements

**Goal:** Production-readiness improvements — better type safety, module system, optimization.

**Status: Done.**

#### 7.6.1 .env Loading

The REPL automatically loads a `.env` file on startup via `af_repl:load_env/0`. Supports comments (`#`), quoted values (`"value"` and `'value'`), and blank lines. Missing `.env` files are silently ignored.

#### 7.6.2 Extended Tail Call Optimization

TCO now applies to all compiled word calls in tail position, not just self-recursive calls. The word trace frame is popped before the tail call, putting it in Erlang tail position. The BEAM's native TCO prevents stack growth.

#### 7.6.3 Inter-Module Imports

The `import` word compiles a `.a4` file to a native BEAM module and registers its words:
```
"lib_math.a4" import    # -> Atom("lib_math")
```
Uses `af_compile_file:compile/1` internally. Words become native BEAM functions callable from both ActorForth and Erlang.

#### 7.6.4 Compile-Time Type Checking Enforcement

Type checking is now enforced at compile time:
- **Errors** for verified type mismatches (fully-resolved types that don't match declared signatures)
- **Warnings** for incomplete inference (unknown tokens from product type ops, different stack depth)
- Type inference improved to recognize integer and float literals
- Value-constrained types (`{Int, 1}` vs `{Int, 100}`) considered compatible (same base type)
- `af_type_check.erl` handles `Any` as a type variable with concrete substitution

### Milestone 7.7: Version 2.4 — Typed Messages & Compilation

**Goal:** Complete the actor type system and add on-demand native compilation.

**Status: Done.**

#### 7.7.1 Typed Actor Messages (Milestone 7.3)

Raw actor primitives for spawn-based (non-server) actors:
- `spawn` — `( Atom -- Actor )` spawn process running a named word
- `send` / `!` — `( Any Actor -- )` send value to actor's mailbox
- `receive` — `( -- Any )` blocking receive from own mailbox
- `receive-timeout` — `( Int -- Any Bool )` receive with timeout in ms

Message type for tagged inter-actor communication:
- `msg` — `( Any String -- Message )` create tagged message
- `msg-tag` — `( Message -- Message String )` get tag (non-destructive)
- `msg-data` — `( Message -- Message Any )` get data (non-destructive)
- `receive-match` — `( String -- Any )` selective receive by tag
- `receive-match-timeout` — `( String Int -- Any Bool )` selective receive with timeout

Type-checked dispatch: `<<` blocks validate argument types against the actor's vocabulary signatures, raising `actor_type_error` for mismatches.

#### 7.7.2 Value Constraints in Signatures

The compiler now supports value constraints in main clause signatures:
```
: factorial 0 Int -> Int ; drop 1 .    # {Int, 0} value constraint
: factorial Int -> Int ; dup 1 - factorial * .
```
Previously this only worked in sub-clause syntax. The `handle_input_sig` and `handle_output_sig` handlers now buffer literal values (integer, float, bool) and combine them with the following type name.

#### 7.7.3 On-Demand Native Compilation

The `compile` word compiles interpreted words to native BEAM functions:
```
"factorial" compile    # replaces interpreted ops with native wrapper
```
Multi-clause words compile to multi-clause Erlang functions with literal patterns for value constraints.

### Milestone 7.8: Server-Side Actor Message Validation

**Goal:** Enforce typed message validation on the *receiving* side of actor dispatch, not just the sending side (`<<` blocks).

**Status: TODO.**

**Problem:** Currently, type checking only happens client-side in the `<< >>` send protocol (`validate_actor_args` in `af_type_actor.erl`). If a message is sent directly to the actor's pid — via raw Erlang `Pid ! {cast, ...}`, FFI, or a malicious/buggy external process — it bypasses type checking entirely. The actor loop in `actor_loop/2` and the supervised worker in `af_actor_worker.erl` accept any message shape and attempt to execute the word, crashing on type mismatches instead of cleanly rejecting them.

**Implementation (3 tiers, in priority order):**

#### 7.8.1 Actor Loop Validation (Near-term)
Add arg validation in `actor_loop/2` before calling `execute_actor_word`:
- Look up the word name in the actor's vocab
- Validate incoming args against the vocab's `args` type list using the existing `validate_actor_args/3` function
- On mismatch: log a warning and skip the message (cast) or reply with `{error, type_mismatch}` (call)
- ~10 lines of change in `af_type_actor.erl`

Same check in `af_actor_worker.erl` `handle_cast` and `handle_call` callbacks for supervised actors.

#### 7.8.2 Unknown Word Rejection (Near-term)
Reject messages for word names not in the actor's vocabulary:
- Currently `actor_loop` would crash with a badmatch trying to interpret an unknown token
- Should cleanly reject with `{error, unknown_word}` instead of crashing

#### 7.8.3 Capability-Based Actor References (Future)
Replace raw pid exposure with opaque actor references that include a validation token:
- Actor reference includes a signed/hashed capability token generated at `server` creation
- Actor loop validates the token before processing any message
- Prevents pid-guessing attacks in distributed BEAM environments
- Heavier mechanism, appropriate for production security hardening

---

## Phase 3: BEAM Bytecode Compilation

### Milestone 8: BEAM Compilation via Abstract Forms

**Goal:** Compile ActorForth words to native BEAM functions.

**Status: Done (initial implementation).**

Strategy: Target **Erlang abstract forms** as the compilation representation. The Erlang compiler (`compile:forms/1`) handles optimization and bytecode emission. This gives us all of BEAM's optimizations for free.

```erlang
%% ActorForth word  : double Int -> Int ; dup + .
%% Compiles to Erlang abstract form equivalent of:
%% double(X) -> X + X.
```

#### 8.1 Compilation Strategy

Implemented in `af_word_compiler.erl`:
1. Stack simulation: each operation's effect is computed on an expression stack
2. Argument patterns: value constraints become literal patterns, type-only become variables
3. Multi-clause support: same-name words with different signatures compile to multi-clause functions
4. Inter-word calls: resolved to local (same module) or remote (cross-module) calls
5. `compile:forms/2` produces .beam binaries

The `compile` word (in `af_type_any.erl`) allows on-demand compilation:
```
: factorial 0 Int -> Int ; drop 1 .
: factorial Int -> Int ; dup 1 - factorial * .
"factorial" compile    # compiles to native BEAM with pattern matching
10 int factorial       # runs at native speed
```

Natively compiled operations:
- Stack: `dup`, `drop`, `swap`, `rot`, `over`, `2dup`
- Arithmetic: `+`, `-`, `*`, `/` (integer and float)
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Boolean: `not`
- Literals: integer, float

#### 8.2 Future Compilation Work

- Product type accessors (getters/setters) as native ops
- `print` and other side-effecting ops
- Automatic compilation of all defined words
- Guard expressions for type constraints
- Core Erlang target for more control over optimization

### Milestone 9: BEAM Assembler in ActorForth

**Goal:** Write the BEAM assembler in ActorForth itself.

Once ActorForth can compile to Core Erlang and has file I/O:
1. Implement BEAM binary format writer as ActorForth words
2. Implement instruction encoding (genop.tab opcodes)
3. Write register allocator in ActorForth
4. Replace Core Erlang backend with direct BEAM emission
5. Self-host: compile the compiler with itself

Key references:
- [The BEAM Book](https://github.com/happi/theBeamBook) by Erik Stenman — Chapter on BEAM file format and instruction set
- [BEAM instruction set](https://www.cs-lab.org/historical_beam_instruction_set.html)
- [genop.tab](https://github.com/erlang/otp/blob/master/lib/compiler/src/genop.tab) — canonical opcode definitions
- [codec-beam](https://hackage.haskell.org/package/codec-beam) — Haskell BEAM assembler (reference implementation)

---

## Implementation Order & Dependencies

```
M1: Type System ──────┐
                      ├── M3: Interpreter ── M4: Compiler ── M5: Product Types
M2: Parser ───────────┘                          |
                                                  ├── M6: Control Flow
                                                  │
                                                  └── M7: Actors
                                                          |
                                                     M8: Core Erlang
                                                          |
                                                     M9: Self-hosting
```

**Suggested sprint plan:**

| Sprint | Milestone | Deliverable |
|---|---|---|
| 1 | M1 + M2 | Type registry, stack sig matching, parser with source locations |
| 2 | M3 | Working interpreter with `int`, `bool`, `atom`, stack ops, arithmetic |
| 3 | M4 | Word definition `: name types -> types ; body .` |
| 4 | M5 | Product types with auto-generated constructors/accessors |
| 5 | M6 | Control flow: if/else/then, recursion, loops |
| 6 | M7 | Actor spawn/send/receive mapped to Erlang processes |
| 7 | M8 | Core Erlang emission for compiled words |
| 8 | M9 | BEAM assembler written in ActorForth |

## Testing Strategy

- **EUnit** for unit tests of individual modules (type registry, parser, stack ops)
- **Common Test** for integration tests (multi-word programs, actor interactions)
- **ActorForth script tests** — run `.a4` sample files and check output (regression suite)
- **Property-based testing** (PropEr) for parser and type matching invariants
- Port the Python test suite as ActorForth scripts where applicable

## File Structure

```
src/
  af_type.erl           # Type registry and stack signature matching
  af_parser.erl         # Tokenizer
  af_interpreter.erl    # Core interpret loop
  af_repl.erl           # Interactive REPL
  af_compiler.erl       # Word definition state machine
  af_actor.erl          # Actor primitives
  af_types/
    af_type_atom.erl    # Atom type
    af_type_int.erl     # Int type + arithmetic
    af_type_bool.erl    # Bool type + comparisons
    af_type_any.erl     # Global operations (dup, swap, drop, print, etc.)
    af_type_compiler.erl # Compiler state types (WordDefinition, etc.)
    af_type_usertype.erl # Product type support
    af_type_actor.erl   # Actor type
include/
  token.hrl             # Token record (exists)
  continuation.hrl      # Continuation record (exists, will be extended)
  operation.hrl         # Operation record
  af_type.hrl           # Type records
test/
  af_type_tests.erl
  af_parser_tests.erl
  af_interpreter_tests.erl
  af_compiler_tests.erl
  af_actor_tests.erl
samples/
  *.a4                  # ActorForth scripts (exist)
```

## Reference Materials

- [Starting Forth](https://www.forth.com/starting-forth/) by Leo Brodie — Forth fundamentals
- [Thinking Forth](http://thinking-forth.sourceforge.net/) by Leo Brodie — Forth philosophy and design
- [The BEAM Book](https://github.com/happi/theBeamBook) by Erik Stenman — BEAM VM internals
- [BEAM instruction set](https://www.cs-lab.org/historical_beam_instruction_set.html)
- [A peak into the Erlang compiler and BEAM bytecode](https://gomoripeti.github.io/beam_by_example/)
- [codec-beam](https://hackage.haskell.org/package/codec-beam) — Reference BEAM assembler in Haskell
- [ActorForth Language Definition](docs/ActorForthDefinition.md) — Language spec (needs updating from Python refs)
