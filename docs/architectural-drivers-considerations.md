# Architectural Drivers & Design Considerations

This document captures the *why* behind ActorForth: the driving principles, the design decisions made and considered, and the tradeoffs accepted. It serves both language implementors and users as a record of intent — what the language is trying to be, what it deliberately avoids, and where open questions remain.

---

## Part I: Origin and Motivation

### The Impedance Mismatch

> *"When we take the position that it is not only the programmer's responsibility to produce a correct program but also to demonstrate its correctness in a convincing manner, then the above remarks have a profound influence on the programmer's activity: the object he has to produce must be usefully structured."*
> — Dijkstra (1970) "Notes On Structured Programming" (EWD249), Section 3

The application of imperative, shared-state, deterministic, single-thread concurrency model programming languages to address distributed, decentralized, non-deterministic problem spaces creates an **impedance mismatch**. Systems implemented in such languages are fundamentally incapable of fully modeling the problem space or delivering a correct solution.

This impedance mismatch forces the developer to simultaneously maintain at least **two conflicting mental models** — the problem domain's reality and the language's computational model — and the additional effort of reconciling their conflicts effectively triples the effort necessary to deliver the desired functionality. While the cost of complexity increases linearly, the product quality deficit and associated risks increase **exponentially**.

Imperative style code structures focus too much on the "How" and confusing abstractions hide the "What." Programming languages should put the developer into a correct mental state to properly reason about the problem space, align the programmer's mental model with the actual implementation of the solution, and yet be simple enough in its implementation to be fully understood by the developer.

### The Consequences Are Real

> *"I didn't come up in computer science; I used to be a physicist. That transition gives me a rather specific perspective on this situation: that computer science is a field which hasn't yet encountered consequences."*
> — Yonatan Zunger, 2018

Computer science has historically operated without material consequences for software defects. When a Facebook app crashes, users reload their browsers. But when three innocuous lines of code in the wrong order can wipe away millions of dollars of wealth and utterly destroy an organization, you've chosen the wrong architectural model upon which to build your future.

**The DAO Exploit (June 17, 2017):** One year after ERC-20 was released, programmers experienced real consequences. The DAO's `splitDAO` function sent tokens to a split contract *before* updating local balances — a reentrancy vulnerability that allowed an attacker to recursively drain funds. Over $50 million in ETH was lost. An organization valued at greater than $150 million was destroyed.

**Solidity's contributions to this defect:**
- Imperative programming model combined with implicit "magic" object-like model hides non-obvious behaviors
- Lack of atomic paired operations allows contracts to enter indeterminate state (does the term "ledger" not imply something fundamental that must be maintained?)
- Language designed around ECMAScript syntax for "familiarity" rather than correctness

A Cornell University researcher stated that Solidity was partially to blame: "this was actually not a flaw or exploit in the DAO contract itself: technically the EVM was operating as intended, but Solidity was introducing security flaws into contracts that were not only missed by the community, but missed by the designers of the language themselves."

This was not an isolated incident. July 19, 2017: Parity's multi-sig wallet exploited due to function visibility errors. November 6, 2017: a user accidentally locked away potentially $300 million in every Parity multi-sig wallet — root cause: uninitialized library dependency.

These are not catastrophic failures of key systems. They are almost always the result of multiple, small, innocuous failures that only manifest themselves in rare edge conditions — the kinds of things that grow geometrically as complexity grows linearly. The kinds of things humans will never see in code review.

### Why Existing Approaches Are Insufficient

> *"Programs must be written for people to read, and only incidentally for machines to execute."*
> — Hal Abelson, *Structure and Interpretation of Computer Programs*

> *"Debugging is twice as hard as writing the code in the first place. Therefore, if you write the code as cleverly as possible, you are, by definition, not smart enough to debug it."*
> — Brian Kernighan, *The Elements of Programming Style*

**Imperative languages** (C, Java, Solidity, Go, JavaScript): Shared mutable state, implicit side effects, and complex control flow hide bugs in the interactions between operations. The "von Neumann bottleneck" identified by Backus in 1977 — the fundamental mismatch between the sequential instruction-at-a-time model and the problem domains we actually need to address — remains unresolved.

**Functional languages** (Haskell, Cardano/Plutus): Backus correctly identified the problem but functional programming injects another level of indirection. Lambda calculus has practical limitations. "Provable programs" are not possible past a certain point of complexity — what's more important is simplicity itself. As De Millo argued in "Social Processes and Proofs of Theorems and Programs," software is made reliable primarily through social processes and experience, not formal verification of impenetrable proofs.

**Research languages**: Almost all look like variants of Lisp — because Lisp is easy to implement, easy to parse, and is the academic *lingua franca*. But starting from lambda calculus means starting on the wrong foot for concurrent, distributed systems.

**The actor model is based on physics rather than math.** It models independent entities communicating through message passing — which is how the real world actually works. This is the right computational model for distributed systems.

### The Vision: A Language to Build Better Languages

ActorForth exists to prove that the impedance mismatch isn't inevitable. The goal is a programming language that:

1. Enforces an architectural style that makes common defects actually impossible
2. Aligns the developer's mental model with the execution model
3. Is simple enough to be fully understood — not just used, but *understood*
4. Provides clear and enforceable abstractions inherent in the problem space
5. Can build domain-specific languages for any problem domain — **a language to build better languages**

The belief: no single crypto-language or cryptoledger will "win" — just as no single programming language won. What's needed is a meta-language that can express the right abstractions for each specific domain.

### The Cryptoledger Application Domain

Distributed, asynchronous computing platforms are fundamentally changing economic and social transactions. A cryptoledger is a non-refutable persistent store of immutable facts, typically time series based, managed in a completely decentralized manner. The implications:

- **Revolution of trust relationships**: Trust in correct algorithms and verifiable code, not institutions. Complete transparency without sacrificing privacy (zero-knowledge proofs). Radically increased optionality. All pricing and participation voluntary between all parties.
- **Disruption of rent-seeking intermediaries**: Information-theory economics. Radical reduction of transaction costs. Open call free trade exchanges replacing centralized platforms.
- **Skin in the game**: Integrity as the ultimate currency. Cryptoledger-managed smart contracts where parties secure deposits or performance insurance to guarantee their commitments. Reputation becomes something you prove cryptographically, not something a platform assigns you.
- **Extended type systems**: Cryptoledger ontologies require declarative, constraint-based resolution. Every good consumer cryptoledger application is based on a distributed open call bid system.

The contract complexity for real applications is 2-3+ orders of magnitude higher than simple crypto-coins. Human contracts are far larger and 90% of their logic consists of exception handling. Current tools cannot deploy contracts larger than ~500 lines of code. Incremental improvements (OpenZeppelin, Vyper) are bandaids that add more complexity without addressing the architectural root cause.

**Declarative and constraint-based systems** eliminate most errors caused by imperative programming styles. **Homoiconic languages** to build domain-specific languages have been around since Lisp and Forth — can't we get this right by now?

---

## Part II: Driving Principles

### Principle 1: Brutal Elimination of Unnecessary Complexity

> *"The art of programming is the art of organizing complexity, of mastering multitude and avoiding its bastard chaos as effectively as possible."*
> — Dijkstra (1970) "Notes On Structured Programming" (EWD249), Section 3

Every feature, every mechanism, every line of implementation must justify its existence. If a feature can be expressed in terms of simpler primitives, it should be. If a mechanism adds complexity without proportional capability, it should be removed.

This is not minimalism for aesthetic reasons. It is a correctness strategy. Small code is auditable code. Obvious code is correct code. When source is so compact that logic errors have nowhere to hide, formal verification becomes tractable and informal review becomes reliable.

> *"Program testing can be used to show the presence of bugs, but never to show their absence!"*
> — Dijkstra (1970) EWD249, corollary

The corollary: **never add a feature to work around a limitation when you can fix the limitation.** Don't add syntax to handle edge cases — fix the dispatch mechanism. Don't add special forms — make the general form work. As simple as possible but no simpler.

### Principle 2: No Syntax, Only Types

ActorForth has no grammar to violate. There are no syntax errors — only type errors. Every token either matches an operation (dispatched by name + stack signature) or becomes an Atom on the stack. The meaning of a token is determined entirely by the type currently on top of the stack.

This has profound consequences:

- **The parser is trivial.** Tokenize on whitespace and a few self-delimiting characters. Nothing else.
- **New syntax is just new types.** The compiler is four types with tiny dictionaries. A DSL is a type whose dictionary gives tokens domain-specific meaning. A BEAM assembler is types for emitting bytecode. All processed by the same interpreter.
- **The language is homoiconic.** Code and data are processed by the same mechanism. There is no distinction between "language features" and "user-defined abstractions" — they use the same dispatch.
- **Extensibility is unlimited.** Any construct that can be expressed as "push a marker type, intercept subsequent tokens via handler, finalize with a closing word" can be added without modifying the interpreter.

### Principle 3: The Stack Is the State Machine

The type on top of the stack determines the dictionary context. The interpreter always does the same thing:

1. Search TOS type's dictionary for the token
2. Check TOS type's handler
3. Search the Any (global) dictionary
4. Try literal handlers
5. Push as Atom

This never changes. What changes is what's on the stack. When the stack holds an Int, tokens mean arithmetic. When it holds CodeCompile, tokens mean "record this operation." When it holds ActorSend, tokens mean "dispatch to remote actor or execute locally."

The interpreter has no modes, no flags, no state variables. The stack IS the state.

### Principle 4: Strong Types Prevent Errors, Not Restrict Expression

Every value on the stack carries its type. Operations declare their input and output types. The dispatch mechanism enforces these contracts at every step. This catches errors immediately — not at the end of a long computation, but at the exact token where the type mismatch occurs.

But the type system is not restrictive. It's open:

- **Any type matches everything.** Generic stack operations work on all types.
- **New types can be defined at any time.** Product types, compiler types, actor types — all created through the same mechanisms.
- **Operations can be overloaded.** Same name, different signatures. The dispatch picks the most specific match.
- **Value constraints enable pattern matching.** `{Int, 0}` matches only zero. More specific than `Int` alone. The dispatch IS the branch.

### Principle 5: Actors Are Primitive, Not Bolted On

Concurrency is not a library feature or an afterthought. It is woven into the language from the beginning:

- Every ActorForth actor IS an Erlang process with its own continuation
- Message passing maps directly to Erlang message passing
- The `server` word turns any typed instance into an actor
- The `<< ... >>` syntax provides a clean send protocol with automatic cast/call classification
- Actor state is private by design — auto-generated accessors are excluded from the remote vocabulary

The actor model is based on physics rather than math. It models how the real world works — independent entities communicating through messages, not shared memory. This isn't about making concurrency "easy." It's about making it *natural* — something that emerges from the language's structure rather than fighting against it.

### Principle 6: Composition Over Branching

Small words that do one thing compose better than large words with complex branching. Pattern matching through overloaded word definitions replaces if/else:

```
: fib Int -> Int ;
    : 0 -> 0 ;
    : 1 -> 1 ;
    : Int -> Int ; dup 1 - fib swap 2 - fib +.
```

Each case is a separate declaration. No nesting. No else branches. The dispatch selects the right clause based on the actual value. This is:

- **Clearer** — each case reads independently
- **Extensible** — new cases are new declarations, no modification of existing code
- **BEAM-native** — maps directly to Erlang function clauses and BEAM pattern matching

### Principle 7: Self-Hosting Is the Goal

ActorForth should eventually compile itself. The BEAM assembler should be written in ActorForth — a set of types whose dictionaries give tokens meaning in the context of emitting bytecode. The same interpreter that runs programs compiles them.

This isn't an academic exercise. Self-hosting validates the language's expressive power. If ActorForth can't express its own compiler cleanly, its abstractions aren't powerful enough.

The path: Erlang prototype -> Core Erlang compilation -> BEAM bytecode emission -> self-hosting bootstrap.

---

## Part III: Fundamental Design Decisions

### Decision: TOS-Driven Dictionary Dispatch

**What:** The type on top of the stack determines which dictionary is searched first for any token.

**Why:** This eliminates the need for separate interpreter modes, method dispatch mechanisms, or context-switching logic. The interpreter is four lines of decision logic that never change.

**Consequence:** Every operation must be registered in a type's dictionary. The "right" dictionary is determined by the first type in the reversed sig_in (the TOS type). Words with no typed inputs go in the Any dictionary.

**Alternative considered:** Name-based dispatch without type context (traditional Forth). Rejected because it prevents overloading and makes the compiler/DSL mechanism impossible.

### Decision: Stack Items Are {Type, Value} Tuples

**What:** Every value on the stack is a two-element tuple: an atom naming the type and the value itself.

**Why:** This makes dispatch O(1) — just check the first element of TOS. No runtime type inference needed. Types are always explicit, never ambiguous.

**Consequence:** No "untyped" values. Even raw text is `{Atom, "text"}`. This is a feature, not overhead — it means type errors are caught immediately.

### Decision: Compilation IS Interpretation

**What:** The compiler is four types (WordDefinition, InputTypeSignature, OutputTypeSignature, CodeCompile) with small dictionaries. No separate compiler pass or mode switch.

**Why:** This keeps the interpreter simple and makes the compiler extensible. Any new syntax follows the same pattern: push a marker type, intercept tokens via handler, finalize with a closing word.

**Consequence:** The compiler's state is visible on the stack. You can inspect it, debug it, even manipulate it programmatically.

### Decision: Late Binding in Compiled Word Bodies

**What:** Each token in a compiled word body is stored as a thunk that dispatches through the interpreter at runtime.

**Why:** Enables recursion (word can call itself), forward references (word can call words defined later), and redefinition (changing a word affects all callers).

**Consequence:** Slightly slower than direct function pointers. Acceptable for the interpreter phase. When compiling to BEAM, this becomes direct function calls.

### Decision: sig_in Convention — Leftmost = Deepest, Rightmost = TOS

**What:** In type signatures, the rightmost type is TOS. `Counter Int -> Counter` means Int on top, Counter below.

**Why:** Matches standard Forth convention and how humans read stack notation. The compiler accumulates left-to-right during parsing, then reverses before registration so that element 0 of the stored sig_in corresponds to TOS for dispatch matching.

**Alternative considered:** TOS-first (leftmost = TOS). Initially implemented this way, causing confusion when reading signatures. Fixed to standard Forth convention based on analysis of sample .a4 files.

### Decision: Pattern Matching via Overloaded Word Definitions

**What:** Same word name can be defined multiple times with different signatures. Value-constrained signatures (`{Int, 0}`) are tried before general ones (`Int`).

**Why:** Eliminates if/else branching in favor of declarative pattern matching. Each case is independent and extensible. Maps directly to BEAM's pattern matching when compiling.

**Syntax:** Sub-clauses within a word definition use `:` to start each clause:

```
: word MasterSig ;
    : value-constrained-sig ;
    : value-constrained-sig ; body
    : general-sig ; body.
```

The master signature provides type context for resolving literal values in sub-clauses. Sub-clause bodies are terminated by the next `:` or the final `.`.

### Decision: Product Types with Auto-Generated Accessors

**What:** `type Point x Int y Int .` auto-generates constructor (`point`), getters (`x`, `y`), and setters (`x!`, `y!`).

**Why:** Eliminates boilerplate. The common case (define data structure, access its fields) should require zero manual operation definitions.

**Consequence:** Auto-generated ops are marked with `source = auto` so they can be distinguished from user-defined ops (important for actor vocabulary building).

### Decision: List Type — Built-In Wrapping Native BEAM Cons Cells

**What:** `List` is a built-in type, not a product type. Internally `{'List', [StackItems]}` — the value is a native Erlang list of `{Type, Value}` stack items. Operations: `nil` (empty list), `cons` (prepend), `length`, `head`, `tail`.

**Why:** BEAM lists are already singly-linked cons cells with O(1) prepend and O(n) length — exactly the semantics ActorForth needs. Wrapping native Erlang lists means zero overhead when compiling to Core Erlang: `cons` becomes the BEAM `cons` instruction, pattern matching on `[H|T]` maps directly to BEAM list decomposition. Building a list type from product types would create a linked-list-of-product-type-instances with unnecessary indirection and no path to efficient compilation.

**Consequence:** Lists are heterogeneous — any stack item can be consed onto any list. This matches Erlang's own list semantics. Future refinement may add typed lists (e.g., `List<Int>`) as a constraint on the existing mechanism, not a separate implementation.

**Operations:**
- `nil` — pushes `{'List', []}` (registered in Any dict, no inputs)
- `cons` — `Any List -> List` — prepends item to list (registered in Any dict so it's found when List is below TOS)
- `length` — `List -> Int` — returns element count (registered in List dict)
- `head` — `List -> Any` — returns first element, preserving its original type (registered in List dict)
- `tail` — `List -> List` — returns list without first element (registered in List dict)

### Decision: Literal Handlers — Types Claim Tokens

**What:** Types can register a `literal` word in their dictionary. Before the interpreter falls back to pushing an Atom, it iterates all types with literal handlers and gives each a chance to claim the token.

**Why:** Allows `42` to become `{Int, 42}` directly, without requiring the explicit constructor `42 int`. Maintains homoiconicity — the parser has no opinions about what constitutes a number or boolean; types make that determination.

**Consequence:** New types (Real, String, etc.) can add their own literal handlers without modifying the interpreter. The mechanism is open and extensible. Both explicit (`42 int`) and implicit (`42`) syntax remain valid.

### Decision: String Type — Wrapping Erlang Binaries

**What:** `{'String', Binary}` wraps native Erlang binaries. Quoted strings (`"hello"`) are automatically converted by the interpreter (via a `quoted` flag on the token record, checked before literal handlers and Atom fallback). Operations: `string` constructor, `concat`, `length`, `to-atom`, `to-int`, `to-string`.

**Why:** BEAM strings are binaries. Using binaries directly means zero-cost interop with Erlang/Elixir code and efficient memory representation. The `quoted` flag approach keeps the parser opinion-free — it doesn't know what a String is, it just marks tokens that came from `"..."` syntax.

**Consequence:** Quoted strings bypass literal handlers entirely. This is correct: `"42"` should always be a String, never an Int. The explicit `string` constructor handles Atom-to-String conversion for unquoted tokens.

### Decision: Map Type — Wrapping Erlang Maps

**What:** `{'Map', ErlangMap}` wraps native Erlang maps. Keys and values are full `{Type, Value}` stack items, preserving type information. Operations: `map-new`, `map-put`, `map-get`, `map-delete`, `map-has?`, `map-keys`, `map-values`, `map-size`.

**Why:** Erlang maps are the standard dynamic key-value structure on BEAM. Product types handle fixed schemas; Maps handle dynamic/open schemas (JSON payloads, configuration, session state).

**Consequence:** Maps are heterogeneous in both keys and values. `map-get` on a missing key raises a structured error rather than returning a sentinel — fail fast, consistent with the language's type-error philosophy.

### Decision: OTP Bridge via gen_server (af_server)

**What:** `af_server` is a gen_server that wraps an ActorForth interpreter. It loads a `.a4` script during init, then dispatches Erlang `call`/`cast` messages to ActorForth words with automatic term conversion at boundaries.

**Why:** This makes ActorForth actors supervisable OTP citizens without requiring BEAM compilation. An ActorForth actor can participate in supervision trees, be called via `:erpc` from Elixir, and be monitored/restarted — all standard OTP patterns. Compilation becomes a performance optimization, not a prerequisite for interop.

**Consequence:** The type registry (`af_type` ETS table) is shared across all `af_server` instances in the same VM. Type initialization happens once; each server only runs its script to build per-server vocabulary and state. Term conversion (`af_term`) handles the Erlang <-> ActorForth type mapping at every boundary.

### Decision: Structured Error Records (af_error)

**What:** Errors are `#af_error{}` records containing type, message, token (location), stack snapshot, and word trace (call chain). Compiled words push/pop trace frames automatically. `af_error:format/1` produces human-readable multi-line error messages.

**Why:** Ad-hoc error tuples made debugging difficult — no call chain, no stack context, inconsistent formats. Structured errors provide everything needed to diagnose problems: where it happened, what was on the stack, and how execution got there.

**Consequence:** The REPL pattern-matches on `#af_error{}` for pretty printing. `af_server` returns `{error, #af_error{}}` for structured error information to Erlang callers. Word trace adds minimal overhead (one cons/tl per word call).

---

## Part IV: Actor Model Decisions

### Decision: Actor Behavior Models — `server` as First Pattern

**What:** The `server` word takes any typed instance from the stack, spawns a stateful Erlang process to hold it, and pushes an Actor reference. This is one specific actor behavior pattern — the stateful, message-dispatching server (analogous to Erlang's gen_server).

**Why:** Actors aren't a separate concept — they're a capability of any type. A Counter can be a local value or an actor. The type's operations define the interface either way. The `server` pattern is the most common actor use case: hold state, accept messages, dispatch to vocabulary operations, thread state through calls.

**Other actor behavior patterns to follow:** `server` is not the only way to create an actor. Future patterns may include finite state machine actors, pub/sub actors, supervisors, and other behavior models — each as a separate word that spawns a process with different message-handling semantics. The `spawn` primitive already provides a simpler stateless pattern (runs a named word per message with no persistent state between messages).

**Consequence:** The actor's vocabulary is built automatically from user-defined words whose sig_in includes the state type. Auto-generated ops are excluded — state is private.

### Decision: `<< ... >>` Send Protocol

**What:** `<<` enters actor-send mode. Inside the block, tokens that match the actor's vocabulary are dispatched to the actor. Other tokens execute locally on a temporary stack. `>>` exits send mode, pushing any results back.

**Why:** Provides a clean syntax that separates "what to compute locally" from "what to send to the actor." Automatic cast/call classification based on whether the operation returns values beyond the state type.

**Consequence:** Cast (no return values) is async. Call (returns values) is sync with a timeout. The actor reference stays on the stack through the entire `<< ... >>` block.

### Decision: Actor State Privacy

**What:** Auto-generated getters and setters (source=auto) are NOT included in the actor's remote vocabulary. Only user-defined words are callable through `<<`.

**Why:** State encapsulation. The actor's internal structure is hidden. Only the operations the programmer explicitly defines are part of the remote interface. This prevents external code from directly accessing or modifying actor state.

**Consequence:** All field access inside actor word bodies works directly (the word executes inside the actor process with state on the stack). External access goes through user-defined words only.

### Decision: Actor Vocabulary Searches All Type Dictionaries

**What:** When building an actor's remote vocabulary, all registered type dictionaries are searched for user-defined ops whose sig_in includes the actor's state type.

**Why:** After sig_in reversal, multi-type words (e.g., `add Counter Int -> Counter`) register in the TOS type's dict (Int), not the state type's dict (Counter). The vocab builder must search everywhere to find all applicable words.

**Consequence:** Vocabulary building is O(all types x all ops). Acceptable for actor creation (happens once per actor). Could be optimized with an index if needed.

---

## Part V: Considered But Deferred

### Consideration: Implicit State Threading for Actors (Context Register)

**Problem:** Actor word definitions require stack manipulation (`dup`, `swap`, `rot`) to thread the state instance through getter/setter calls:

```
# Current — explicit threading:
: increment Counter -> Counter ; dup value 1 + value! .
: add Counter Int -> Counter ; swap dup value rot + value! .
```

**Options considered:**

**Option A: Context Register.** Add a `context` field to the continuation. When set, getters read from context instead of popping from stack. Setters write to context instead of expecting the instance on the stack. A new dispatch step checks the context type's dictionary when the token isn't found through normal dispatch.

- *Pros:* Eliminates all stack manipulation for state access in actor words. Actor loop simplified (no separate_reply needed). Clean separation of state and computation values.
- *Cons:* Adds an extra dispatch step to every token (overhead for all code, not just actors). Getters/setters need a `case` branch for context vs stack mode. Two mental models for working with product types. State becomes invisible to the `stack` inspection word.

**Option B: Compile-Time Transformation.** Automatically inject `dup` before getters and `swap` before setters at compile time.

- *Rejected:* Magic behavior. The source code wouldn't match what executes. This is the kind of implicit transformation that creates confusion and debugging nightmares. Violates the principle that the code says what it does.

**Option C: Non-Destructive Getters.** Change getter signature from `Type -> FieldType` to `Type -> Type FieldType`. The getter leaves the instance on the stack.

- *Pros:* Eliminates most `dup` calls. One-line change in getter generation. Works uniformly everywhere (not just actors). No new concepts.
- *Cons:* Multi-arg words (like `add`) still need stack shuffling. Changes getter semantics globally, which could affect existing code patterns.

**Decision: Defer.** All options represent tradeoffs. Option C is the lightest change and could be tried first. Option A is more powerful but adds complexity to the interpreter. Neither is urgently needed — the current actor word bodies work correctly, the stack manipulation is just verbose.

**Revisit when:** More real actor code has been written and the actual pain points are clear from experience, not theory.

### Consideration: Maximal-Consume Parser with Type Handler Literals

**Problem:** Currently `.` `:` `;` are always self-delimiting. This prevents tokens like `3.14` from being consumed as a single token for a future Real type.

**Proposed approach:** The parser maximal-consumes tokens (no self-delimiting characters break them). Types register literal handlers that can claim tokens. If no handler claims a token and it contains delimiter characters, the interpreter re-tokenizes it recursively, splitting on delimiters.

**Example flow for `3.14`:**
1. Parser produces single token `"3.14"`
2. Interpreter tries literal handlers — Real type's handler claims it -> `{Real, 3.14}`
3. If no Real type registered, re-tokenize: `"3"`, `"."`, `"14"` — each interpreted individually

**Why deferred:** The current parser with explicit constructors (`42 int`) works. The literal handler mechanism (implemented) already handles simple cases (`42` -> `{Int, 42}`). The maximal-consume change is only needed for types whose literal representation contains delimiter characters (Real, possibly others).

**Decision: Implement when Real type is added.** The design is solid but there's no immediate need.

### Consideration: Loop Primitives vs Recursive Patterns

**Problem:** The Python implementation had `countdown`, `lcount`, `loop` as built-in loop primitives. Should they be carried forward?

**Options:**

1. **Implement loop primitives.** Familiar to Forth users. Direct, imperative.
2. **Use tail-recursive patterns with pattern matching.** Loops become recursive words with value-constrained base cases:
   ```
   : countdown Int -> ;
       : 0 -> ; drop
       : Int -> ; dup print 1 - countdown.
   ```

**Decision: Recursive patterns preferred.** Loop primitives are special-purpose. Recursive patterns use the general-purpose pattern matching mechanism that already exists. This eliminates three built-in words (`countdown`, `lcount`, `loop`) in favor of zero new mechanisms. The BEAM VM handles tail recursion efficiently.

Loop primitives may still be added later if profiling shows recursive patterns are too slow for hot loops, but this is unlikely given BEAM's optimization of tail calls.

### Consideration: `debug on/off` Behavior

**Problem:** Need a way to trace interpreter dispatch decisions during development.

**Decision implemented:** `debug` is a word in the Any dictionary that pushes a `{'Debug', #{}}` marker. The Debug type has a handler that intercepts `on` and `off`, setting a flag on the continuation. When the flag is set, the interpreter prints per-token trace lines showing stack state, token, and dispatch decision.

This follows the language's own patterns — `debug` is a type whose handler changes the meaning of subsequent tokens. The interpreter doesn't know about "debug mode" — it just checks a flag on the continuation. The same mechanism could be used for other interpreter extensions.

### Consideration: File Loading Semantics

**Problem:** Need to execute `.a4` files from the command line and from the REPL.

**Decision:** `af_repl:run_file/1` executes a file and returns the final continuation. `af_repl:run_file_repl/1` executes a file then enters the REPL with the resulting state. The original Python implementation used `resume`/`cont` to stay in the REPL after file execution; this is replaced by explicit function choice or a future `--repl` command-line flag.

### Consideration: Word Definition Syntax — Colon Position

**Problem:** The Python-era samples used `name : Type -> Type ; body .` (name before colon). Current implementation uses `: name Type -> Type ; body .` (colon first).

**Decision: Colon first.** The colon must start the word definition because if the word name comes first, it would be dispatched as a token — potentially calling an existing word of the same name or pushing an Atom. `:` changes the TOS type to WordDefinition, which captures the next token as a name rather than executing it.

### Consideration: `==` vs `eq` for Equality

**Decision:** Use `==` as the primary equality operator. It's familiar, visually clear, and the parser handles it correctly as a single two-character token.

---

## Part VI: Type System Properties

### No Syntax Errors, Only Type Errors

This is not a slogan — it's a structural property. The parser never rejects input. The interpreter never rejects tokens. Every token either dispatches to an operation (because the types match) or becomes an Atom (because nothing matched). An Atom is valid data, not an error.

Type errors occur when an operation is invoked but the stack doesn't satisfy its signature. This happens at the point of dispatch, not at parse time. The error includes the exact token, its position, and the stack state — everything needed to diagnose the problem.

This property means **the language cannot have unexpected parse failures.** Any text is valid input. The question is always: does the resulting stack state match what the next operation expects?

### Provable Correctness Through Type-Driven Dispatch

Every operation declares its input and output types. The dispatch mechanism enforces these declarations at every step. This creates a continuous chain of type assertions through any program:

1. The stack starts empty (or with known types from prior computation)
2. Each operation consumes typed values and produces typed values
3. If any operation's input types don't match the actual stack, dispatch fails immediately
4. No operation can receive values of the wrong type

This is not a type checker that runs before execution. It IS the execution. The same mechanism that finds the right operation also verifies the types. There is no way to bypass it because there is no other execution mechanism.

A correctly designed programming language enforces an architectural style that makes as many common defects as possible actually impossible to occur in the first place.

### Code Size as a Correctness Tool

ActorForth words tend to be very small — typically one line. This is a design goal, not an accident:

- Operations are fine-grained (one word per stack transformation)
- Composition replaces branching (sequence of small words vs one large word)
- Pattern matching replaces conditionals (multiple small definitions vs if/else blocks)
- Product types auto-generate boilerplate (no manual getter/setter code)

Small words are auditable. A one-line word can be verified by inspection. A sequence of one-line words can be verified by reading left to right. Complex behavior emerges from composition, not from complexity within individual words.

---

## Appendix A: Key References

- Dijkstra (1970) "Notes On Structured Programming" (EWD249) — structured programming, reliability of mechanisms, testing shows presence not absence of bugs
- Backus (1977) Turing Award Lecture "Can Programming Be Liberated from the von Neumann Style?" — the von Neumann bottleneck
- De Millo, Lipton, Perlis "Social Processes and Proofs of Theorems and Programs" — reliability through social processes, not just formal verification
- Abelson & Sussman, *Structure and Interpretation of Computer Programs* — programs for people to read
- Kernighan, *The Elements of Programming Style* — debugging twice as hard as writing
- Bastiat (1850) "The Law" — "Law is the common force organized to act as an obstacle of injustice"
- Scherrey (2019) "ActorForth — Architectural Drivers" (docs/ActorForth.pdf)
- Scherrey (2019) "Why ActorForth" presentation (docs/WhyActorForth.pdf)
- Scherrey (2019) "State of the Block Chain" (docs/StateOfTheBlockChain.docx)

## Appendix B: Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| Type registry & dispatch | Done | ETS-backed, sig matching with value constraints |
| Parser | Done | Whitespace + self-delimiters, comments, strings |
| Interpreter (4-step + literals) | Done | TOS dict -> handler -> Any -> literals -> Atom |
| Int/Bool types | Done | Arithmetic, comparisons, literal handlers |
| Stack operations | Done | dup, drop, swap, rot, over, 2dup, print, stack |
| Word compiler | Done | `: name Sig -> Sig ; body .` with late binding |
| Pattern matching sub-clauses | Done | Value constraints on Int, Bool, String types; right-aligned partial matching |
| Product types | Done | Auto-generated constructor, non-destructive getters, setters |
| List type | Done | Built-in wrapping native BEAM cons cells: nil, cons, length, head, tail |
| String type | Done | Wraps Erlang binaries; quoted strings auto-convert; concat, length, to-atom, to-int, to-string |
| Map type | Done | Wraps Erlang maps; map-new, map-put, map-get, map-delete, map-has?, map-keys, map-values, map-size |
| Assertions (assert, assert-eq) | Done | Built-in test primitives with structured error reporting |
| Actor model (server, << >>, stop) | Done | Spawn, cast/call, vocab building, state privacy |
| Bridge module (af_server) | Done | gen_server wrapping ActorForth interpreter; OTP-supervisable; term conversion at boundaries |
| Term conversion (af_term) | Done | Bidirectional Erlang term <-> ActorForth stack item conversion |
| Structured errors (af_error) | Done | Consistent error records with location, word trace, stack snapshot |
| Literal handlers | Done | Types register `literal` word; Int and Bool implemented; quoted strings handled by interpreter |
| Debug on/off | Done | Trace output showing dispatch decisions |
| File loading (load word) | Done | `"file.a4" load` from ActorForth; `run_file/1` from Erlang; relative path resolution |
| Erlang FFI | Done | `erlang-apply` and `erlang-apply0` call any Erlang function with term conversion |
| Non-destructive getters | Done | Getters leave product instance on stack; eliminates dup-before-access pattern |
| Core Erlang compilation | Done | Closure-based word compilation (af_compile); BEAM module generation via compile:forms |
| OTP supervision for actors | Done | supervised-server word; simple_one_for_one supervisor; gen_server actor workers |
| Tail call optimization | Done | Detects self-call in tail position; pops trace before tail call for BEAM TCO |
| Compile-time type checking | Done | af_type_check: infers stack effects, resolves Any to concrete types, warns on sig mismatch |
| Context register for actors | Deferred | See Part V |
| Maximal-consume parser | Deferred | Needed for Real type |
| Self-hosting BEAM assembler | Future | Phase 3 |
