# Introduction to ActorForth

*A language where the stack is the state, types are the syntax, and actors are the concurrency.*

---

## Chapter 1: The Stack Is Everything

If you've used Forth before, you know the feeling: there are no variables cluttering up the place, no assignment statements, no declarations. There's just the stack, and the words that transform it. ActorForth inherits this directness, but adds something Forth never had — every value on the stack knows what it is.

In traditional Forth, a number is just bits. The programmer remembers what those bits mean. In ActorForth, there are no naked values. Everything on the stack is a typed pair: a type tag and a value, married together for life. When you type:

```
42 int
```

Two things happen. First, `42` lands on the stack as an Atom — it's just text that didn't match any operation. Then `int` sees that Atom, converts it to a proper integer, and puts `{Int, 42}` on the stack. The type stays with the value everywhere it goes.

This isn't just bookkeeping. The type of the value on top of the stack determines what every subsequent word *means*. This is the central idea of ActorForth, and everything else follows from it.

### Your First Session

Start the REPL:

```
$ rebar3 shell
1> af_repl:start().
ActorForth REPL. ^C to exit.
ok:
```

That `ok:` prompt is waiting. Let's put some numbers on the stack:

```
ok: 10 int 20 int
```

The stack now holds two integers. `10` and `20` each arrived as Atoms, then `int` converted each one. We can see the stack:

```
ok: stack
0) 20 : Int
1) 10 : Int
```

The top of stack (TOS) is shown first — `20` is on top, `10` is underneath. Now let's add them:

```
ok: +
ok: stack
0) 30 : Int
```

The `+` word consumed both integers and left their sum. Notice we didn't write `10 + 20`. There is no infix notation. The values go on the stack first, then the operation acts on them. This is postfix — also called Reverse Polish Notation — and it eliminates the need for parentheses, operator precedence, or parsing ambiguity.

### Stack Manipulation

Four words handle most stack shuffling:

```
ok: 5 int dup
```

`dup` copies the top item. The stack now has two copies of `{Int, 5}`.

```
ok: stack
0) 5 : Int
1) 5 : Int
```

```
ok: drop
```

`drop` discards the top item. Back to one `5`.

```
ok: 3 int swap
```

`swap` exchanges the top two items. Now `5` is on top and `3` is underneath. Or rather — `3` was on top after pushing it, then `swap` put `5` back on top:

```
ok: stack
0) 5 : Int
1) 3 : Int
```

There's also `2dup`, which copies the top *two* items:

```
ok: 2dup stack
0) 5 : Int
1) 3 : Int
2) 5 : Int
3) 3 : Int
```

These four words — `dup`, `drop`, `swap`, `2dup` — work on any type. They're in the global dictionary because they don't care what's on the stack. An Int, a Bool, a user-defined Point — all the same to `swap`.


## Chapter 2: Types Drive Everything

Here is the key insight that separates ActorForth from Forth, and indeed from most languages: **the type on top of the stack determines the dictionary that the next word is looked up in.**

When you type `+` and TOS is an Int, the interpreter finds `+` in the Int dictionary. If TOS were something else — a Bool, say — the interpreter wouldn't find `+` there, and would fall back to the global (Any) dictionary, and if it's not there either, `+` would just become an Atom on the stack. No crash. No syntax error. Just an Atom.

This means the *same word* can mean completely different things depending on what's on the stack. The stack is not just data storage — it's the program's state machine.

### The Dispatch Rules

When the interpreter encounters a token, it follows four steps:

1. **Search TOS type's dictionary.** If TOS is `{Int, 42}`, look in Int's dictionary.
2. **Check TOS type's handler.** Some types (like compiler states) intercept all tokens.
3. **Search the Any dictionary.** Global operations live here.
4. **Push as Atom.** Nothing matched — it's just text. Put `{Atom, "whatever"}` on the stack.

That's the entire interpreter. Four steps. It never changes. What changes is what's on the stack, and therefore which dictionary is consulted first.

### Constructors: The Gateway Between Types

Every type needs a way to create instances. The convention is: the constructor is a lowercase version of the type name, and it converts from some input type.

```
ok: 42 int         # Atom -> Int
ok: True bool      # Atom -> Bool
```

Constructors live in the Any dictionary because they need to be reachable regardless of what's currently on the stack. The input type constraint ensures they only fire when the right type is available:

```
ok: hello int      # "hello" is not a valid integer — this will error
```

### Arithmetic and Comparison

Integer operations live in the Int dictionary:

```
ok: 10 int 3 int -     # 10 - 3 = 7
ok: 7 int 6 int *      # 7 * 6 = 42
ok: 100 int 3 int /    # 100 div 3 = 33  (integer division)
```

Arithmetic follows the Forth convention: the *second* item on the stack is the left operand. So `10 int 3 int -` computes `10 - 3`, not `3 - 10`. Think of it as: "put 10 down, put 3 on top, subtract the top from what's underneath."

Comparisons are global — they work on any pair of values:

```
ok: 10 int 20 int <    # Is 10 < 20?  -> {Bool, true}
ok: 5 int 5 int ==     # Is 5 == 5?   -> {Bool, true}
ok: 3 int 7 int >=     # Is 3 >= 7?   -> {Bool, false}
```

Boolean operations live in the Bool dictionary:

```
ok: True bool not      # -> {Bool, false}
```

### No Syntax Errors, Only Type Errors

If you type something the interpreter doesn't recognize, it doesn't complain. It just puts it on the stack as an Atom:

```
ok: hello world foo
ok: stack
0) foo : Atom
1) world : Atom
2) hello : Atom
```

Three Atoms, sitting peacefully on the stack. They're not errors — they're data. Maybe some future word will know what to do with them. Maybe they'll be arguments to a constructor. Maybe they're names being captured by the compiler. The interpreter doesn't judge.

This is a fundamental philosophical point: **ActorForth has no syntax to violate.** There is only the question of whether the types on the stack match what an operation expects. Everything is a type error or a successful dispatch.


## Chapter 3: Defining New Words

Here's where ActorForth starts to feel powerful. You can define new words — named operations with typed signatures — and the system is smart enough to put them in the right dictionary automatically.

### The Colon Definition

The syntax mirrors traditional Forth, extended with type signatures:

```
: word-name  InputTypes -> OutputTypes ; body .
```

Let's define a word that doubles an integer:

```
ok: : double Int -> Int ; dup + .
```

Read it aloud: "Define `double`. It takes an Int, returns an Int. The body is: duplicate the top, then add." The `.` at the end closes the definition.

Now use it:

```
ok: 5 int double
ok: stack
0) 10 : Int
```

Because the first input type is `Int`, `double` was automatically registered in the Int dictionary. The interpreter finds it by the same mechanism it finds `+` — TOS is Int, search Int's dictionary.

### Building Words from Words

Words can call other words:

```
: double Int -> Int ; dup + .
: quadruple Int -> Int ; double double .
```

`quadruple` calls `double` twice. Each call does the full dispatch — TOS is Int, find `double` in Int's dictionary, execute it. No special calling convention.

```
ok: 3 int quadruple
ok: stack
0) 12 : Int
```

### Words with Multiple Inputs

A word can consume several stack items:

```
: add3 Int Int -> Int ; + .
```

This takes two Ints and produces one. The `+` inside consumes both inputs:

```
ok: 10 int 20 int add3
ok: stack
0) 30 : Int
```

### Words with No Inputs

A word with no input types goes into the Any dictionary — available everywhere:

```
: answer -> Int ; 42 int .
: pi-ish -> Int ; 355 int 113 int / .
```

```
ok: answer
ok: stack
0) 42 : Int
```

### How Compilation Works

Here's the beautiful part. When you type `:`, it pushes a `WordDefinition` value onto the stack. Now the TOS type is `WordDefinition`, so the interpreter's dictionary context changes. The next token — the word name — is captured by WordDefinition's handler instead of being executed or pushed as an Atom.

Then the TOS type becomes `InputTypeSignature`. Now every token is interpreted as a type name and accumulated into the input signature. When `->` appears, it's found in InputTypeSignature's dictionary. It transitions the TOS to `OutputTypeSignature`.

Same pattern continues: type names accumulate into the output signature. `;` transitions to `CodeCompile`. Now tokens are recorded as operation references — the body of the word. Finally `.` finishes compilation, saves the word, and pops all the compiler state off the stack.

**The interpreter never changed.** It's still doing the same four-step dispatch it always does. The compiler is just four types — `WordDefinition`, `InputTypeSignature`, `OutputTypeSignature`, `CodeCompile` — each with a tiny dictionary. The types on the stack redirected what the tokens meant.

This is why the compiler is so small. It's not a separate system. It's just more types.


## Chapter 4: Thinking in Patterns

Traditional Forth — and most languages — uses `if`/`else` for branching. ActorForth takes a different path. The preferred control flow mechanism is **pattern matching through overloaded word definitions.**

### Multiple Definitions, One Name

You can define the same word multiple times with different input signatures. The dispatcher walks the list of definitions and picks the first one whose signature matches the actual stack. Here's a word that behaves differently depending on what *type* is on the stack:

```
: describe Int -> ; drop print .
: describe Bool -> ; drop print .
```

Both are named `describe`, but one is registered in the Int dictionary, the other in Bool. When you call `describe`, the dispatcher checks TOS — if it's an Int, it finds the first definition; if it's a Bool, it finds the second. Same name, different behavior, selected by the types already on the stack.

But the real power comes from **value constraints**. The dispatch mechanism doesn't just match on type — it can match on specific values within a type. A signature entry like `{Int, 0}` matches only when TOS is an Int whose value is exactly zero. The general signature `Int` matches any integer. When both are registered under the same name, the more specific value-constrained version is tried first.

### Factorial: The Classic Example

The factorial function shows this at its best. In most languages you'd write `if n == 0 then 1 else n * factorial(n-1)`. In ActorForth, you write two separate definitions of the same word:

```
: fact 0 Int -> Int ; drop 1 int .          # base case: fact(0) = 1
: fact Int -> Int ; dup 1 int - fact * .     # recursive case: n * fact(n-1)
```

Both definitions are named `fact` and both live in the Int dictionary. When `fact` is called, the dispatcher tries each definition's input signature against the actual stack:

- If TOS is `{Int, 0}`, the first definition matches — its signature `0 Int` requires both the type *and* the value to match. It drops the zero and pushes 1.
- If TOS is any other Int, the first definition fails (wrong value) and the second matches — its signature `Int` requires only the type. It does the recursive computation.

The value constraint `0 Int` is more specific than `Int` alone, so it's tried first. No `if` statement needed. The dispatch *is* the branch.

Let's trace `3 int fact`:

```
Stack: [3]  → fact matches "Int" case → dup 1 int - fact *
Stack: [3, 3] → 1 int → [1, 3, 3] → - → [2, 3] → fact (recurse)
Stack: [2, 3] → fact matches "Int" case → dup 1 int - fact *
Stack: [2, 2, 3] → 1 int → [1, 2, 2, 3] → - → [1, 2, 3] → fact (recurse)
Stack: [1, 2, 3] → fact matches "Int" case → dup 1 int - fact *
Stack: [1, 1, 2, 3] → 1 int → [1, 1, 1, 2, 3] → - → [0, 1, 2, 3] → fact (recurse)
Stack: [0, 1, 2, 3] → fact matches "0 Int" case → drop 1 int
Stack: [1, 1, 2, 3] → * → [1, 2, 3] → * → [2, 3] → * → [6]
```

Result: `{Int, 6}`. Three recursive calls, three multiplications, no branching logic.

### Why Pattern Matching?

Three reasons:

**Clarity.** Each case is a separate definition. You can read them independently. There's no nested `if`/`else` to untangle.

**Extensibility.** Adding a new case means adding a new definition. You don't modify existing code. This is the Open/Closed Principle, enforced by the language.

**BEAM compatibility.** When ActorForth compiles to BEAM bytecode, pattern-matched word definitions map directly to Erlang's function clause mechanism. The BEAM VM is *built* for this — it compiles pattern matches into efficient jump tables. We get that optimization for free.

### Composition Over Branching

In ActorForth, we compose small words rather than writing long procedures with branches:

```
# Instead of one big word with if/else:
: process Int -> Int ; ... complex branching logic ... .

# Prefer small, focused words:
: double Int -> Int ; dup + .
: square Int -> Int ; dup * .
: bump Int -> Int ; 1 int + .
```

Each word does one thing. Combine them as needed:

```
ok: 5 int double bump square
ok: stack
0) 121 : Int
```

That's `((5 * 2) + 1)^2 = 121`. No temporary variables, no intermediate assignments. The data flows through the words.


## Chapter 5: Product Types — Building Data Structures

Numbers and booleans are fine, but real programs need structured data. ActorForth provides **product types** — named records with typed fields.

### Defining a Type

```
type Point x Int y Int .
```

This registers a new type called `Point` with two fields: `x` (an Int) and `y` (an Int). The system auto-generates three things:

1. **A constructor** — `point` (lowercase), registered in the Any dictionary
2. **Getters** — `x` and `y`, registered in the Point dictionary
3. **Setters** — `x!` and `y!`, for updating fields

### Construction

The constructor takes field values from the stack in definition order:

```
ok: 10 int 20 int point
ok: stack
0) {x: 10, y: 20} : Point
```

First the x value, then the y value, then `point` to assemble them.

### Field Access

When a Point is on top of the stack, getter words are available:

```
ok: 10 int 20 int point
ok: x
ok: stack
0) 10 : Int
```

The getter consumed the Point and pushed its `x` field value. If you need to keep the Point, `dup` first:

```
ok: 10 int 20 int point
ok: dup x swap y
ok: stack
0) 20 : Int
1) 10 : Int
```

### Updating Fields

Setters use the `!` suffix convention. Push the new value, then apply the setter:

```
ok: 10 int 20 int point     # create Point(10, 20)
ok: 99 int x!               # update x to 99
ok: dup x swap y
ok: stack
0) 20 : Int
1) 99 : Int
```

The setter consumes the new value and the Point, then pushes a new Point with the updated field. The original is not mutated — there is no mutation in ActorForth.

### Types with More Fields

Product types can have any number of fields:

```
type Color r Int g Int b Int .

255 int 128 int 0 int color
```

That creates `Color(r: 255, g: 128, b: 0)`.

### Types Compose with Words

Define operations that work with your types:

```
type Point x Int y Int .

: origin -> Point ; 0 int 0 int point .
: move-right Point -> Point ; dup y swap x 1 int + swap point .
```

`origin` takes no inputs and produces a Point at (0, 0). `move-right` takes a Point, increments its x by 1, and produces a new Point. These words go in the right dictionaries automatically — `origin` in Any (no inputs), `move-right` in Point (first input is Point).

```
ok: origin move-right move-right move-right x
ok: stack
0) 3 : Int
```

### Why Product Types Matter

Product types make ActorForth a viable language for real programs. You're not limited to loose values on the stack — you can bundle related data together, name the fields, and define operations that work on the bundle. And because the type goes on the stack, it participates in dispatch just like Int or Bool. When a Point is on top, Point-specific words become available. The stack is still the state machine.

Product types are also the foundation for more complex structures. A product type can contain fields of other product types. You can build trees, records, domain objects — whatever your program needs.


## Chapter 6: Actors — Concurrency as a Primitive

Most languages bolt concurrency on as an afterthought — threads, locks, mutexes, all the things that make parallel programming a nightmare. ActorForth takes a different approach: concurrency is built in from the beginning, using the actor model, and it maps directly to Erlang's battle-tested process system.

An actor is a process with its own stack, its own state, and a mailbox. Actors communicate by sending messages. There is no shared state. No locks. No races. If you need another actor to do something, you send it a message and move on.

### Spawning an Actor

First, define a word that will be the actor's behavior. This word receives a message (pushed onto its stack) and does something with it:

```
: printer Int -> ; print .
```

`printer` takes an Int and prints it. Now spawn an actor that runs this word:

```
ok: printer spawn
ok: stack
0) <0.123.0> : Actor
```

`spawn` consumed the Atom `printer` and produced an Actor. The actor is alive — it's an Erlang process, running in the background, waiting for messages.

### Sending Messages

Send a value to an actor with `send` (or its alias `!`):

```
ok: printer spawn       # spawn the actor
ok: 42 int swap send    # send it 42
```

The actor receives `{Int, 42}`, pushes it onto a fresh stack, and runs `printer` — which prints `42`. Then it goes back to waiting.

You can send as many messages as you want:

```
ok: printer spawn
ok: dup 10 int swap send
ok: dup 20 int swap send
ok: dup 30 int swap send
```

Each message is handled independently. The actor processes them one at a time, in order.

### Talking to Yourself

`self` pushes the current process's actor reference:

```
ok: self
ok: stack
0) <0.85.0> : Actor
```

Combined with `receive`, you can send yourself a message:

```
ok: 42 int self send    # send 42 to yourself
ok: receive             # block until it arrives
ok: stack
0) 42 : Int
```

This is more useful than it sounds — it's the basic pattern for request/reply between actors.

### The Actor Model in Practice

Think of actors as independent workers, each with their own desk (stack) and inbox (mailbox). They can't reach over to another worker's desk. They can only drop notes in each other's inboxes.

This maps naturally to ActorForth's stack-based computation:

- Each actor has its own continuation (stacks + execution state)
- Messages are typed stack items — they carry their type with them
- An actor's behavior is just a word — a sequence of stack operations
- Actors are Erlang processes — you get supervision, fault tolerance, and distribution for free

### Why Actors Belong in a Forth

The actor model and Forth share a philosophy: **keep things small and independent.** A Forth word does one thing. An actor handles one concern. Words compose by piping data through the stack. Actors compose by passing messages through mailboxes.

And because ActorForth runs on the BEAM, you inherit decades of engineering around reliable concurrent systems. Erlang processes are cheap — you can spawn millions of them. They're isolated — one crashing doesn't take down the others. They're distributable — they can run on different machines transparently.

ActorForth doesn't try to reinvent this. It exposes it directly, with stack-friendly syntax.


## Chapter 7: The Interpreter Is the Language

We've saved the deepest idea for near the end, because it requires everything before it to make sense.

Traditional language implementations have separate phases: parsing, analysis, compilation, execution. ActorForth has **one phase**: interpretation. Everything — including compilation — happens through the same four-step dispatch mechanism.

### The Four Steps, Always

Every token, every time:

1. Look in the TOS type's dictionary
2. Check the TOS type's handler
3. Look in the Any dictionary
4. Push as Atom

That's it. When you're "just running code," TOS is Int or Bool or whatever, and tokens resolve to arithmetic or stack operations. When you're "compiling a word," TOS is CodeCompile, and tokens resolve to "record this operation for later." The interpreter doesn't know the difference. It doesn't need to.

### Creating New Syntax

This is where the idea becomes truly powerful. The compiler isn't special — it's just four types with dictionaries. **You can create your own types with their own dictionaries.** Any type can intercept tokens and give them meaning.

The product type system works this way. When you type `type`, it pushes a `TypeDefinition` onto the stack. Now every subsequent token is interpreted in the context of defining a type — field names, field types — until `.` finishes the definition. Same mechanism. Same interpreter. Different types on the stack.

You could define a type whose dictionary contains words for describing state machines. Or regular expressions. Or SQL queries. Or BEAM bytecode instructions. Each domain gets its own syntax, built from the same primitive: types with dictionaries.

This is homoiconicity in a new form. The language doesn't distinguish between code and data because the mechanism for processing both is the same: look up the token in the current context's dictionary. Code is data that happens to be in a dictionary. Data is tokens that didn't match any dictionary.

### Late Binding and Recursion

When a word is compiled, its body isn't resolved to fixed function pointers. Each operation in the body is a thunk — a small piece of code that says "when executed, look up this token through the interpreter." This means:

- **Recursion works.** A word can call itself because the lookup happens at runtime, when the word already exists in the dictionary.
- **Forward references work.** A word can call a word that hasn't been defined yet. As long as it exists by the time the call happens, it's fine.
- **Redefinition works.** If you redefine a word, existing words that call it pick up the new definition automatically.

This is the same strategy that makes Lisp's `eval` powerful, adapted to a stack-based, type-driven world.


## Chapter 8: Putting It All Together

Let's build something that uses everything: types, words, product types, and actors.

### A Counter Service

```
# Define a counter type
type Counter name Atom value Int .

# Create a counter
: new-counter Atom -> Counter ; 0 int counter .

# Increment
: increment Counter -> Counter ; dup value 1 int + swap name swap counter .

# Get the current value
: count Counter -> Int ; value .
```

Use it interactively:

```
ok: hits new-counter
ok: increment increment increment
ok: count
ok: stack
0) 3 : Int
```

### A Temperature Converter

```
: f-to-c Int -> Int ; 32 int - 5 int * 9 int / .
: c-to-f Int -> Int ; 9 int * 5 int / 32 int + .
```

```
ok: 212 int f-to-c
ok: stack
0) 100 : Int

ok: drop 0 int c-to-f
ok: stack
0) 32 : Int
```

### Stack Juggling: Computing the Distance Between Two Points

```
type Point x Int y Int .

: diff-sq Int Int -> Int ; - dup * .

: dist-sq Point Point -> Int ;
    swap x swap x diff-sq
    swap y swap y diff-sq
    + .
```

This computes the squared distance between two points. (We don't have floating point or square roots yet — that's a future type to define.)

```
ok: 0 int 0 int point 3 int 4 int point dist-sq
ok: stack
0) 25 : Int
```

`3^2 + 4^2 = 25`. The classic 3-4-5 triangle.


## Chapter 9: The Road Ahead

What you've seen is the prototype interpreter — ActorForth running inside Erlang. It works, it's tested, and it demonstrates the core ideas. But the vision goes further.

### Phase 2: Compiling to BEAM

The interpreter is useful for exploration, but ActorForth should compile to BEAM bytecode. The first target is **Core Erlang** — a stable intermediate representation that the Erlang compiler can optimize. An ActorForth word like:

```
: double Int -> Int ; dup + .
```

would compile to something equivalent to:

```erlang
double(X) -> X + X.
```

This gets all of BEAM's optimizations for free — dead code elimination, constant folding, register allocation. ActorForth words become real BEAM functions.

### Phase 3: Self-Hosting

The ultimate goal: write the BEAM assembler in ActorForth itself. The assembler would be a set of types — `BeamModule`, `BeamFunction`, `BeamInstruction` — whose dictionaries give tokens meaning in the context of emitting bytecode. The same interpreter that runs your program would also compile it.

A `BeamModule` on the stack means tokens are module-level directives. A `BeamFunction` means tokens are function instructions. A `BeamInstruction` means tokens are opcodes and operands. The outer interpreter doesn't change — it still does the same four-step dispatch. The types on the stack determine whether you're writing a program or writing a compiler.

This is the promise of the TOS-driven dictionary mechanism: **any tool that processes text can be built as a set of types.** A compiler is just types for emitting code. A test framework is just types for asserting conditions. A network protocol handler is just types for parsing packets. The same interpreter drives them all.

---

## Quick Reference

### Stack Operations (Any dictionary)

| Word | Effect | Description |
|------|--------|-------------|
| `dup` | `( a -- a a )` | Duplicate top |
| `drop` | `( a -- )` | Discard top |
| `swap` | `( a b -- b a )` | Swap top two |
| `2dup` | `( a b -- a b a b )` | Duplicate top two |
| `print` | `( a -- )` | Print and consume top |
| `stack` | `( -- )` | Display entire stack |
| `words` | `( -- )` | List all operations |
| `types` | `( -- )` | List all types |

### Arithmetic (Int dictionary)

| Word | Effect | Description |
|------|--------|-------------|
| `+` | `( a b -- a+b )` | Addition |
| `-` | `( a b -- a-b )` | Subtraction (second minus top) |
| `*` | `( a b -- a*b )` | Multiplication |
| `/` | `( a b -- a/b )` | Integer division |

### Comparison (Any dictionary)

| Word | Effect | Description |
|------|--------|-------------|
| `==` | `( a b -- bool )` | Equal |
| `!=` | `( a b -- bool )` | Not equal |
| `<` | `( a b -- bool )` | Less than |
| `>` | `( a b -- bool )` | Greater than |
| `<=` | `( a b -- bool )` | Less or equal |
| `>=` | `( a b -- bool )` | Greater or equal |

### Boolean (Bool dictionary)

| Word | Effect | Description |
|------|--------|-------------|
| `not` | `( bool -- bool )` | Logical negation |

### Constructors (Any dictionary)

| Word | Effect | Description |
|------|--------|-------------|
| `int` | `( Atom -- Int )` | Parse text as integer |
| `bool` | `( Atom -- Bool )` | Parse "True"/"False" as boolean |

### Word Definition

```
: name  InputType1 InputType2 -> OutputType1 ; body words here .
```

### Product Type Definition

```
type TypeName field1 Type1 field2 Type2 .
```

Auto-generates: constructor (`typename`), getters (`field1`, `field2`), setters (`field1!`, `field2!`).

### Actor Primitives

| Word | Effect | Description |
|------|--------|-------------|
| `spawn` | `( Atom -- Actor )` | Spawn process running named word |
| `self` | `( -- Actor )` | Push current process reference |
| `send` | `( val Actor -- )` | Send value to actor |
| `!` | `( val Actor -- )` | Alias for send |
| `receive` | `( -- val )` | Block for next message |

### Special Tokens

| Token | Behavior |
|-------|----------|
| `.` `:` `;` | Self-delimiting (no whitespace needed) |
| `#` | Comment to end of line |
| `"..."` | String literal |
