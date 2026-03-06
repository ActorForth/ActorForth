# Introduction to ActorForth

*A language where the stack is the state, types are the syntax, and actors are the concurrency.*

*In the style of Leo Brodie's "Starting FORTH" and "Thinking FORTH"*

---

## Chapter 1: The Stack Is Everything

If you've used Forth before, you know the feeling: there are no variables cluttering up the place, no assignment statements, no declarations. There's just the stack, and the words that transform it. ActorForth inherits this directness, but adds something Forth never had — every value on the stack knows what it is.

In traditional Forth, a number is just bits. The programmer remembers what those bits mean. In ActorForth, there are no naked values. Everything on the stack is a typed pair: a type tag and a value, married together for life. When you type:

```
42
```

The interpreter tries to find an operation named `42`. No dictionary has one. Before giving up, it asks each type: "Do you want to claim this token?" The Int type's literal handler recognizes `42` as an integer and pushes `{Int, 42}` onto the stack. The type stays with the value everywhere it goes.

You can also be explicit:

```
42 int
```

Here `42` lands on the stack as an Atom — just text that didn't match any operation or literal handler. Then `int` sees that Atom, converts it to a proper integer, and puts `{Int, 42}` on the stack. Both forms produce the same result. The explicit form matters when literal handlers can't guess what you want, or when you want to be clear about your intent.

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
ok: 10 20
```

The stack now holds two integers — both claimed by the Int literal handler. We can see the stack:

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

A handful of words handle stack shuffling:

```
ok: 5 dup
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
ok: 3 swap
```

`swap` exchanges the top two items. Now `5` is on top and `3` is underneath. Or rather — `3` was on top after pushing it, then `swap` put `5` back on top:

```
ok: stack
0) 5 : Int
1) 3 : Int
```

`rot` rotates the third item to the top:

```
ok: 1 2 3
ok: stack
0) 3 : Int
1) 2 : Int
2) 1 : Int
ok: rot
ok: stack
0) 1 : Int
1) 3 : Int
2) 2 : Int
```

There's also `over`, which copies the second item to the top, and `2dup`, which copies the top *two* items.

These words — `dup`, `drop`, `swap`, `rot`, `over`, `2dup` — work on any type. They're in the global dictionary because they don't care what's on the stack. An Int, a Bool, a user-defined Point — all the same to `swap`.


## Chapter 2: Types Drive Everything

Here is the key insight that separates ActorForth from Forth, and indeed from most languages: **the type on top of the stack determines the dictionary that the next word is looked up in.**

When you type `+` and TOS is an Int, the interpreter finds `+` in the Int dictionary. If TOS were something else — a Bool, say — the interpreter wouldn't find `+` there, and would fall back to the global (Any) dictionary, and if it's not there either, `+` would just become an Atom on the stack. No crash. No syntax error. Just an Atom.

This means the *same word* can mean completely different things depending on what's on the stack. The stack is not just data storage — it's the program's state machine.

### The Dispatch Rules

When the interpreter encounters a token, it follows these steps:

1. **Search TOS type's dictionary.** If TOS is `{Int, 42}`, look in Int's dictionary.
2. **Check TOS type's handler.** Some types (like compiler states) intercept all tokens.
3. **Search the Any dictionary.** Global operations live here.
4. **Try literal handlers.** Each type gets a chance to claim the token.
5. **Push as Atom.** Nothing matched — it's just text. Put `{Atom, "whatever"}` on the stack.

That's the entire interpreter. Five steps. It never changes. What changes is what's on the stack, and therefore which dictionary is consulted first.

### Constructors: The Gateway Between Types

Every type needs a way to create instances. The convention is: the constructor is a lowercase version of the type name, and it converts from some input type.

```
ok: 42 int         # Atom -> Int (explicit constructor)
ok: True bool      # Atom -> Bool (explicit constructor)
```

Constructors live in the Any dictionary because they need to be reachable regardless of what's currently on the stack. The input type constraint ensures they only fire when the right type is available.

For common types, literal handlers provide a shorthand:

```
ok: 42             # Literal handler claims it -> {Int, 42}
ok: True           # Literal handler claims it -> {Bool, true}
ok: False          # Literal handler claims it -> {Bool, false}
```

Both forms are valid. The explicit constructor is still there when you need it — for example, when a token could be ambiguous, or when you're passing a string that happens to look like a number.

### Arithmetic and Comparison

Integer operations live in the Int dictionary:

```
ok: 10 3 -         # 10 - 3 = 7
ok: 7 6 *          # 7 * 6 = 42
ok: 100 3 /        # 100 div 3 = 33  (integer division)
```

Arithmetic follows the Forth convention: the *second* item on the stack is the left operand. So `10 3 -` computes `10 - 3`, not `3 - 10`. Think of it as: "put 10 down, put 3 on top, subtract the top from what's underneath."

Comparisons produce Booleans:

```
ok: 10 20 <        # Is 10 < 20?  -> {Bool, true}
ok: 5 5 ==         # Is 5 == 5?   -> {Bool, true}
ok: 3 7 >=         # Is 3 >= 7?   -> {Bool, false}
```

Boolean operations live in the Bool dictionary:

```
ok: True not       # -> {Bool, false}
```

### No Syntax Errors, Only Type Errors

If you type something the interpreter doesn't recognize — and no literal handler claims it — it simply goes on the stack as an Atom:

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

Let's define a word that squares an integer:

```
ok: : square Int -> Int ; dup * .
```

Read it aloud: "Define `square`. It takes an Int, returns an Int. The body is: duplicate the top, then multiply." The `.` at the end closes the definition.

Now use it:

```
ok: 7 square
ok: stack
0) 49 : Int
```

Because the input type is `Int`, `square` was automatically registered in the Int dictionary. The interpreter finds it by the same mechanism it finds `+` — TOS is Int, search Int's dictionary.

### Building Words from Words

Words can call other words:

```
: double Int -> Int ; dup + .
: quadruple Int -> Int ; double double .
```

`quadruple` calls `double` twice. Each call does the full dispatch — TOS is Int, find `double` in Int's dictionary, execute it. No special calling convention.

```
ok: 3 quadruple
ok: stack
0) 12 : Int
```

### Words with Multiple Inputs

A word can consume several stack items:

```
: add3 Int Int Int -> Int ; + + .
```

This takes three Ints from the stack and produces their sum:

```
ok: 10 20 30 add3
ok: stack
0) 60 : Int
```

### Where Words Live

A word's home dictionary is determined by the rightmost input type — the type that will be on top of the stack when the word is called. This happens automatically:

- `: square Int -> Int ;` — lives in Int dict (TOS will be Int)
- `: origin -> Point ;` — lives in Any dict (no inputs)
- `: add Counter Int -> Counter ;` — lives in Int dict (TOS will be Int)

You never specify which dictionary to use. The type signatures do it for you.

### How Compilation Works

Here's the beautiful part. When you type `:`, it pushes a `WordDefinition` value onto the stack. Now the TOS type is `WordDefinition`, so the interpreter's dictionary context changes. The next token — the word name — is captured by WordDefinition's handler instead of being executed or pushed as an Atom.

Then the TOS type becomes `InputTypeSignature`. Now every token is interpreted as a type name and accumulated into the input signature. When `->` appears, it's found in InputTypeSignature's dictionary. It transitions the TOS to `OutputTypeSignature`.

Same pattern continues: type names accumulate into the output signature. `;` transitions to `CodeCompile`. Now tokens are recorded as operation references — the body of the word. Finally `.` finishes compilation, saves the word, and pops all the compiler state off the stack.

**The interpreter never changed.** It's still doing the same dispatch it always does. The compiler is just four types — `WordDefinition`, `InputTypeSignature`, `OutputTypeSignature`, `CodeCompile` — each with a tiny dictionary. The types on the stack redirected what the tokens meant.

This is why the compiler is so small. It's not a separate system. It's just more types.


## Chapter 4: Thinking in Patterns

Traditional Forth — and most languages — uses `if`/`else` for branching. ActorForth takes a different path. The preferred control flow mechanism is **pattern matching through overloaded word definitions.**

### Value Constraints

The dispatch mechanism doesn't just match on type — it can match on specific values within a type. A word whose signature says `0` for an Int parameter matches only when that parameter is exactly zero. The general signature `Int` matches any integer. When both are registered under the same name, the more specific value-constrained version is tried first.

### Fibonacci: The Canonical Example

Here's the actual `fib.a4` from the samples directory:

```
: fib Int -> Int ;
    : 0 -> 0 ;
    : 1 -> 1 ;
    : Int -> Int ;
        dup
        1 -
        fib
        swap
        2 -
        fib
        +.
```

The master signature `: fib Int -> Int ;` establishes the word name and provides type context. Inside it, three sub-clauses handle different cases:

- `: 0 -> 0 ;` — when the input is exactly 0, return 0
- `: 1 -> 1 ;` — when the input is exactly 1, return 1
- `: Int -> Int ;` — for any other Int, compute `fib(n-1) + fib(n-2)`

No `if` statement. No `else`. The dispatch *is* the branch. Each case reads independently, and adding a new case means adding a new sub-clause — you don't modify existing code.

Test it:

```
ok: 0 fib 0 assert-eq
ok: 1 fib 1 assert-eq
ok: 6 fib 8 assert-eq
ok: 10 fib 55 assert-eq
```

### Recursive Countdown

Pattern matching naturally expresses recursion with a base case:

```
: countdown Int -> ;
    : 0 -> ; drop
    : Int -> ; 1 - countdown.
```

Two sub-clauses: when the value is 0, drop it and stop. For any other Int, subtract 1 and recurse. The BEAM VM handles tail recursion efficiently, so this is as fast as a loop.

### Why Pattern Matching?

Three reasons:

**Clarity.** Each case is a separate clause. You can read them independently. There's no nested `if`/`else` to untangle.

**Extensibility.** Adding a new case means adding a new clause. You don't modify existing code. This is the Open/Closed Principle, enforced by the language.

**BEAM compatibility.** When ActorForth compiles to BEAM bytecode, pattern-matched word definitions map directly to Erlang's function clause mechanism. The BEAM VM is *built* for this — it compiles pattern matches into efficient jump tables. We get that optimization for free.

### Pattern Matching on Booleans

Pattern matching isn't limited to integers. Booleans work the same way — `True` and `False` are value constraints:

```
: to-int Bool -> Int ;
    : True  -> Int ; drop 1
    : False -> Int ; drop 0 .
```

This is ActorForth's answer to `if`/`else`. The `drop` removes the matched boolean value from the stack. More complex branching is just more sub-clauses:

```
: choose Int Int Bool -> Int ;
    : True  -> Int ; drop drop
    : False -> Int ; drop swap drop .
```

This `choose` word takes two Ints and a Bool. If True, keep the deeper Int (the "then" value); if False, keep the TOS Int (the "else" value). Notice that sub-clauses with fewer elements than the master signature automatically right-align — `True` matches the Bool position (rightmost/TOS), and the Int positions default to type-only matching.

### Pattern Matching on Strings

Quoted strings can also be value constraints:

```
: greet String -> String ;
    : "hello"   -> String ; drop "Hello World!"
    : "goodbye" -> String ; drop "Farewell!"
    : String    -> String ; .

ok: "hello" greet
ok: stack
0) "Hello World!" : String
```

The general `String` clause acts as the fallback — unmatched strings pass through unchanged.

### Composition Over Branching

In ActorForth, we compose small words rather than writing long procedures with branches:

```
: double Int -> Int ; dup + .
: square Int -> Int ; dup * .
: bump Int -> Int ; 1 + .
```

Each word does one thing. Combine them as needed:

```
ok: 5 double bump square
ok: stack
0) 121 : Int
```

That's `((5 * 2) + 1)^2 = 121`. No temporary variables, no intermediate assignments. The data flows through the words.


## Chapter 5: Product Types — Building Data Structures

Numbers and booleans are fine, but real programs need structured data. ActorForth provides **product types** — named records with typed fields.

### Defining a Type

```
type Point
    x Int
    y Int .
```

This registers a new type called `Point` with two fields: `x` (an Int) and `y` (an Int). The system auto-generates three things:

1. **A constructor** — `point` (lowercase), registered in the Any dictionary
2. **Getters** — `x` and `y`, registered in the Point dictionary
3. **Setters** — `x!` and `y!`, for updating fields

Put each field name and its type on its own line. It makes it easy for the eye to parse what's a name and what's a type. The syntax is the same either way — `type Point x Int y Int .` works too — but the vertical layout is clearer.

### Construction

The constructor takes field values from the stack in definition order:

```
ok: 10 20 point
ok: stack
0) {x: 10, y: 20} : Point
```

First the x value, then the y value, then `point` to assemble them.

### Field Access

Getters are **non-destructive** — they push the field value while leaving the instance on the stack:

```
ok: 10 20 point
ok: x
ok: stack
0) 10 : Int
1) {x: 10, y: 20} : Point
```

This means you can access multiple fields without `dup`:

```
ok: 10 20 point
ok: x swap y
ok: stack
0) 20 : Int
1) {x: 10, y: 20} : Point
2) 10 : Int
```

When you're done with the instance, `swap drop` cleans it up:

```
ok: 10 20 point x swap drop
ok: stack
0) 10 : Int
```

### Updating Fields

Setters use the `!` suffix convention (read "store", as in Forth tradition). Push the new value, then apply the setter:

```
ok: 10 20 point     # create Point(10, 20)
ok: 99 x!           # update x to 99
ok: x swap y
ok: stack
0) 20 : Int
1) {x: 99, y: 20} : Point
2) 99 : Int
```

The setter consumes the new value and the Point, then pushes a new Point with the updated field. The original is not mutated — there is no mutation in ActorForth. Every "update" creates a new value.

### Types Compose with Words

Define operations that work with your types:

```
type Point
    x Int
    y Int .

: origin -> Point ; 0 0 point .
: move-right Point -> Point ; x 1 + x! .
```

`origin` takes no inputs and produces a Point at (0, 0). `move-right` takes a Point, reads `x` (non-destructive, leaves the Point), adds 1, then stores back with `x!`. No `dup` gymnastics needed — the getter leaves the instance for the setter.

### Types with More Fields

Product types can have any number of fields with mixed types:

```
type Account
    balance Int
    ledger List .
```

Fields can be of any registered type — Int, Bool, List, or even another product type.

### Why Product Types Matter

Product types make ActorForth a viable language for real programs. You're not limited to loose values on the stack — you can bundle related data together, name the fields, and define operations that work on the bundle. And because the type goes on the stack, it participates in dispatch just like Int or Bool. When a Point is on top, Point-specific words become available. The stack is still the state machine.

Product types are also the foundation for actors. Any product type instance can become an actor's state — and the operations defined on that type become the actor's interface.


## Chapter 6: Lists

ActorForth provides a built-in List type that wraps native Erlang cons cells. This isn't an accident — BEAM lists are already singly-linked cons cells with O(1) prepend, which is exactly what a stack-based language needs.

### Building Lists

`nil` pushes an empty list. `cons` prepends an item:

```
ok: nil
ok: stack
0) [] : List

ok: 1 cons 2 cons 3 cons
ok: stack
0) [3, 2, 1] : List
```

Each `cons` takes an item and a list, and produces a new list with the item on front. Building left-to-right with cons means the last item consed is the first in the list.

### Decomposing Lists

`head` returns the first element. `tail` returns everything after the first:

```
ok: nil 1 cons 2 cons 3 cons
ok: dup head
ok: stack
0) 3 : Int
1) [3, 2, 1] : List

ok: drop tail
ok: stack
0) [2, 1] : List
```

`length` tells you how many elements:

```
ok: nil 1 cons 2 cons 3 cons length
ok: stack
0) 3 : Int
```

### Lists Are Heterogeneous

Any stack item can be consed onto any list. The items keep their original types:

```
ok: nil 42 cons True cons
ok: stack
0) [True : Bool, 42 : Int] : List
```

This matches Erlang's own list semantics. Each element is a full `{Type, Value}` pair.

### Lists in Practice

Lists become powerful when combined with product types. Here's a ledger that tracks transactions:

```
type Transaction
    kind Atom
    amount Int .

# Build a list of transactions
nil
Deposit 500 transaction cons
Withdrawal 100 transaction cons
```

The list holds typed values — each element is a full Transaction, carrying its type information along.


## Chapter 7: Actors — Concurrency as a Primitive

Most languages bolt concurrency on as an afterthought — threads, locks, mutexes, all the things that make parallel programming a nightmare. ActorForth takes a different approach: concurrency is built in from the beginning, using the actor model, and it maps directly to Erlang's battle-tested process system.

An actor is a process with its own stack, its own state, and a mailbox. Actors communicate by sending messages. There is no shared state. No locks. No races. If you need another actor to do something, you send it a message.

### The `server` Pattern

The `server` word takes any typed instance from the stack and spawns an Erlang process to hold it. That process becomes a stateful actor — it holds the instance as its state, and accepts messages that correspond to user-defined words operating on that type.

Let's build a counter:

```
type Counter
    value Int .

: increment Counter -> Counter ;
    dup value 1 + value!.

: count Counter -> Counter Int ;
    dup value.

0 counter server
```

That last line does three things: pushes `{Int, 0}`, calls `counter` to build `{Counter, #{value => {Int, 0}}}`, then `server` spawns a process to hold it. The stack now has an Actor reference.

### Sending Messages with `<< >>`

The `<<` word enters send mode. Inside the block, tokens that match the actor's **vocabulary** are dispatched to the actor. Other tokens execute locally. `>>` exits send mode.

```
<< increment >>
<< increment >>
<< increment >>
<< count >> 3 assert-eq
```

Three increments, then a count. But notice something: `increment` and `count` behave differently:

- `increment` takes `Counter -> Counter` — same type in, same type out. No extra return values. This is a **cast** — fire-and-forget, asynchronous.
- `count` takes `Counter -> Counter Int` — it returns an extra Int beyond the state. This is a **call** — synchronous, waits for the reply.

The actor system classifies each word automatically based on its signature. You don't specify cast vs call — the types tell the story.

After the `<< count >>` call, the Int result (`3`) is pushed onto *your* stack, on top of the Actor reference. That's why `3 assert-eq` works — it pops the 3 and compares it.

### What's In the Vocabulary?

The actor's vocabulary is built automatically from user-defined words whose signature includes the actor's state type. But **auto-generated ops are excluded**. The getter `value` and setter `value!` that the type system created? Not in the vocabulary. The actor's internal structure is private.

This is state encapsulation, enforced by the language. Want to read the counter's value? Define a word for it (`count`). Want to modify it? Define a word for it (`increment`). The auto-generated accessors work *inside* those word bodies — they execute within the actor process, where the state is on the stack. But from outside, you go through the public interface.

### Local vs Remote Execution

Inside a `<< >>` block, not everything goes to the actor. Only vocabulary words are dispatched remotely. Everything else runs locally:

```
<< 5 add >>
```

Here `5` is a literal — it executes locally, pushing `{Int, 5}` onto a temporary local stack. Then `add` (if it's in the actor's vocabulary) dispatches to the actor with that `5` as an argument.

This is how you pass arguments to actor operations. Compute them locally, then the vocabulary word scoops them up and sends them along.

### Stopping an Actor

Every actor has a built-in `stop` word:

```
<< stop >> drop
```

`stop` terminates the actor process and pushes its final state onto your stack (the `drop` discards it). After `stop`, the Actor reference is no longer valid.

### A Real Example: Bank Account

Here's the full bank actor from `samples/bank_actor.a4`:

```
type Transaction
    kind Atom
    amount Int .

type Account
    balance Int
    ledger List .

# Helper: build a Transaction
: make-tx Atom Int -> Transaction ;
    transaction.

# Helper: prepend a Transaction onto Account's ledger
: add-tx Account Transaction -> Account ;
    swap dup ledger rot cons ledger!.

# Helper: add to Account balance
: credit Account Int -> Account ;
    swap dup balance rot + balance!.

# Helper: subtract from Account balance
: debit Account Int -> Account ;
    swap dup balance rot - balance!.

: deposit Account Int -> Account ;
    dup Deposit swap make-tx rot swap add-tx swap credit.

: withdraw Account Int -> Account ;
    dup Withdrawal swap make-tx rot swap add-tx swap debit.

: get-balance Account -> Account Int ;
    dup balance.

: get-ledger Account -> Account List ;
    dup ledger.

: tx-count Account -> Account Int ;
    dup ledger length.

# Open account with $1000 and empty ledger
1000 nil account server

# Deposits
<< 500 deposit >>
<< 250 deposit >>

# Withdrawal
<< 100 withdraw >>

# Check balance: 1000 + 500 + 250 - 100 = 1650
<< get-balance >> 1650 assert-eq

# Check transaction count
<< tx-count >> 3 assert-eq

# Clean up
<< stop >> drop
```

Notice the helper words: `make-tx`, `add-tx`, `credit`, `debit`. Each does one thing. The higher-level words (`deposit`, `withdraw`) compose them. This is Thinking Forth — decompose until each word is small and clear.

Also notice the stack manipulation. Words like `credit` start with `swap dup balance rot` — that's three stack shuffling words before you get to the business logic (`+`). This is the honest cost of explicit stack threading with destructive getters. It works, it's correct, but it pushes the developer's mind away from the problem domain and into the mechanics of stack ordering. Future language improvements (non-destructive getters, possibly a context register) aim to reduce this friction.

### Why Actors Belong in a Forth

The actor model and Forth share a philosophy: **keep things small and independent.** A Forth word does one thing. An actor handles one concern. Words compose by piping data through the stack. Actors compose by passing messages through mailboxes.

And because ActorForth runs on the BEAM, you inherit decades of engineering around reliable concurrent systems. Erlang processes are cheap — you can spawn millions of them. They're isolated — one crashing doesn't take down the others. They're distributable — they can run on different machines transparently.

ActorForth doesn't try to reinvent this. It exposes it directly, with stack-friendly syntax.

### `self`

The `self` word pushes the current process's actor reference:

```
ok: self
ok: stack
0) <0.85.0> : Actor
```

This is useful for actors that need to send messages to themselves, or pass their own reference to other actors.


## Chapter 8: Testing Your Code

ActorForth provides two assertion words that make it easy to test programs inline. Both are in the Any dictionary, available everywhere.

### `assert`

Pops a Bool from the stack. If it's true, nothing happens. If it's false, execution stops with an error that includes the file, line, and column where the assertion failed:

```
ok: 5 3 > assert         # passes — 5 > 3 is true
ok: 3 5 > assert         # FAILS — 3 > 5 is false
Error: assertion_failed at stdin:1:7
  Assertion failed: expected True on stack
  Stack(1): False:Bool
```

### `assert-eq`

Pops two values from the stack. If they're equal (same type and value), nothing happens. If not, the error shows you what was expected and what was actually there:

```
ok: 7 square 49 assert-eq     # passes — 49 == 49
ok: 7 square 50 assert-eq     # FAILS
Error: assert_eq_failed at stdin:1:14
  Expected 50 but got 49
  Stack(2): 50:Int 49:Int
```

### Testing in Sample Files

Every sample file uses assertions as self-tests. Run them and silence means success:

```
# From fib.a4
0 fib 0 assert-eq
1 fib 1 assert-eq
6 fib 8 assert-eq
10 fib 55 assert-eq
```

No output means all assertions passed. An error halts execution at the exact point of failure with the exact values involved. No test framework needed — the assertions are just words, like everything else.


## Chapter 9: Maps

Maps are dynamic key-value structures — the complement to product types. Where product types have fixed schemas defined at type-definition time, maps can hold any keys and values, added and removed at runtime.

### Building Maps

`map-new` pushes an empty map. `map-put` adds entries:

```
ok: map-new 100 "Alice" map-put 200 "Bob" map-put
ok: map-size
ok: stack
0) 2 : Int
```

The stack order for `map-put` is: map (deepest), value, key (TOS). Think of it as: "starting with this map, store this value at this key."

### Retrieving Values

```
ok: map-new 42 "answer" map-put
ok: "answer" map-get
ok: stack
0) 42 : Int
```

`map-get` takes a key and a map, and pushes the value. If the key doesn't exist, you get a clear error:

```
Error: map_key_not_found at stdin:1:10
  Key "missing" not found in map
  Stack(2): "missing":String <Map>:Map
```

### Other Map Operations

```
ok: map-new 1 "a" map-put 2 "b" map-put

ok: dup "a" map-has?       # -> True
ok: drop dup "c" map-has?  # -> False
ok: drop dup map-keys      # -> List of keys
ok: drop map-values        # -> List of values
```

`map-delete` removes a key. `map-size` returns the count of entries.

### Maps with Product Types

Maps and product types work well together. A product type can have a Map field:

```
type Config
    name String
    settings Map .

"myapp" map-new 8080 "port" map-put config
```

This creates a Config with a fixed `name` field and a flexible `settings` map.


## Chapter 10: The OTP Bridge

ActorForth actors can participate in Erlang/OTP applications today — without BEAM compilation. The `af_server` module is a gen_server that wraps an ActorForth interpreter, making ActorForth actors supervisable, callable from Erlang or Elixir, and fully integrated with OTP.

### How It Works

Write your actor logic in a `.a4` file:

```
# bridge_counter.a4
type Counter
    value Int .

: increment Counter -> Counter ;
    dup value 1 + value!.

: count Counter -> Counter Int ;
    dup value.

: add Counter Int -> Counter ;
    swap dup value rot + value!.

0 counter
```

The last line leaves a Counter instance on the stack. `af_server` loads this file and uses that instance as the server's state.

### Calling from Erlang

```erlang
{ok, Pid} = af_server:start_link("samples/bridge_counter.a4"),

%% Cast (async, no return):
af_server:cast(Pid, "increment", []),

%% Call (sync, returns values):
{ok, [1]} = af_server:call(Pid, "count", []),

%% Call with arguments:
af_server:call(Pid, "add", [10]),
{ok, [11]} = af_server:call(Pid, "count", []),

%% Stop:
af_server:stop(Pid).
```

### Term Conversion

At the bridge boundary, Erlang terms are automatically converted to ActorForth stack items and back:

| Erlang | ActorForth |
|--------|-----------|
| `42` | `{Int, 42}` |
| `<<"hello">>` | `{String, <<"hello">>}` |
| `true` / `false` | `{Bool, true/false}` |
| `foo` (atom) | `{Atom, "foo"}` |
| `[1, 2, 3]` | `{List, [{Int,1}, {Int,2}, {Int,3}]}` |
| `#{<<"k">> => 1}` | `{Map, ...}` |

Product type instances convert to Erlang maps with a `type` key: `#{type => 'Counter', value => 0}`.

### Supervision

Because `af_server` is a gen_server, it slots into supervision trees:

```erlang
ChildSpec = #{
    id => my_counter,
    start => {af_server, start_link, ["counter.a4"]},
    restart => permanent,
    type => worker
},
```

If the ActorForth actor crashes, the supervisor restarts it — loading the `.a4` file fresh and starting from the initial state. This is standard OTP behavior, no special handling needed.

### Raw Evaluation

For more flexible interaction, `af_server:eval/2` interprets a raw ActorForth line against the server's current state:

```erlang
af_server:eval(Pid, "dup value 5 + value!").
```

This is useful for debugging, REPL-like interaction, or dynamic behavior.


## Chapter 11: The Interpreter Is the Language

We've saved the deepest idea for near the end, because it requires everything before it to make sense.

Traditional language implementations have separate phases: parsing, analysis, compilation, execution. ActorForth has **one phase**: interpretation. Everything — including compilation — happens through the same dispatch mechanism.

### The Five Steps, Always

Every token, every time:

1. Look in the TOS type's dictionary
2. Check the TOS type's handler
3. Look in the Any dictionary
4. Try literal handlers
5. Push as Atom

That's it. When you're "just running code," TOS is Int or Bool or whatever, and tokens resolve to arithmetic or stack operations. When you're "compiling a word," TOS is CodeCompile, and tokens resolve to "record this operation for later." The interpreter doesn't know the difference. It doesn't need to.

### Creating New Syntax

This is where the idea becomes truly powerful. The compiler isn't special — it's just four types with dictionaries. **You can create your own types with their own dictionaries.** Any type can intercept tokens and give them meaning.

The product type system works this way. When you type `type`, it pushes a `TypeDefinition` onto the stack. Now every subsequent token is interpreted in the context of defining a type — the type name, then alternating field names and field types — until `.` finishes the definition. Same mechanism. Same interpreter. Different types on the stack.

The actor send protocol works the same way. `<<` pushes an `ActorSend` onto the stack. Now tokens are classified as local or remote and dispatched accordingly. `>>` finishes the send block. No special syntax — just types redirecting what tokens mean.

You could define a type whose dictionary contains words for describing state machines. Or regular expressions. Or SQL queries. Or BEAM bytecode instructions. Each domain gets its own syntax, built from the same primitive: types with dictionaries.

This is homoiconicity in a new form. The language doesn't distinguish between code and data because the mechanism for processing both is the same: look up the token in the current context's dictionary. Code is data that happens to be in a dictionary. Data is tokens that didn't match any dictionary.

### Late Binding and Recursion

When a word is compiled, its body isn't resolved to fixed function pointers. Each operation in the body is a thunk — a small piece of code that says "when executed, look up this token through the interpreter." This means:

- **Recursion works.** A word can call itself because the lookup happens at runtime, when the word already exists in the dictionary.
- **Forward references work.** A word can call a word that hasn't been defined yet. As long as it exists by the time the call happens, it's fine.
- **Redefinition works.** If you redefine a word, existing words that call it pick up the new definition automatically.


## Chapter 12: Loading Files

ActorForth programs can be split across multiple files using the `load` word.

### Libraries

Create a library file with reusable definitions:

```
# lib_math.a4
: square Int -> Int ; dup * .
: double Int -> Int ; 2 * .
: cube   Int -> Int ; dup dup * * .
```

Load it from another file or the REPL:

```
ok: "lib_math.a4" load
ok: 5 square
ok: stack
0) 25 : Int
```

The `load` word takes a String path, reads the file, parses it, and interprets it into the current continuation. All word definitions from the loaded file become available immediately. Relative paths are resolved from the loading file's directory.

### Building Programs from Parts

```
# main.a4
"lib_math.a4" load
"counter.a4" load

5 square double print    # uses both libraries
```

The `load` word preserves the current stack state — it only adds definitions, it doesn't clear anything.


## Chapter 13: Erlang FFI

ActorForth runs on the BEAM VM, and sometimes you need to call existing Erlang functions directly. The FFI (Foreign Function Interface) provides this bridge.

### Zero-Argument Calls

```
ok: node erlang erlang-apply0
ok: stack
0) nonode@nohost : Atom
```

Push the function name, then the module name, then `erlang-apply0`. The result is automatically converted to an ActorForth stack item.

### Calls with Arguments

For functions that take arguments, build an argument list first:

```
ok: nil -42 cons abs erlang erlang-apply
ok: stack
0) 42 : Int
```

Arguments go in a List (built with `nil` and `cons`), then function name, module name, then `erlang-apply`.

```
ok: nil 10 cons 20 cons max erlang erlang-apply
ok: stack
0) 20 : Int
```

### Type Conversion

The FFI automatically converts between Erlang and ActorForth types:
- Erlang integers ↔ `{Int, N}`
- Erlang binaries ↔ `{String, B}`
- Erlang atoms ↔ `{Atom, S}`
- Erlang booleans ↔ `{Bool, V}`
- Erlang lists ↔ `{List, Items}`
- Erlang maps ↔ `{Map, M}`

### When to Use FFI

The FFI is for calling existing Erlang/OTP libraries — file I/O, networking, crypto, date/time, etc. For new logic, write it in ActorForth. The FFI is a bridge, not a crutch.


## Chapter 14: Putting It All Together

Let's trace through a complete example that uses types, words, lists, pattern matching, and actors.

### A Stateful Counter Actor

From `samples/counter_actor.a4`:

```
type Counter
    value Int .

: increment Counter -> Counter ;
    value 1 + value!.

: decrement Counter -> Counter ;
    value 1 - value!.

: count Counter -> Counter Int ;
    value.

# Create a counter starting at 0 and make it an actor
0 counter server

# Increment three times (cast - async)
<< increment >>
<< increment >>
<< increment >>

# Decrement once
<< decrement >>

# Get the count (call - sync) and verify
<< count >> 2 assert-eq

# Increment two more
<< increment >>
<< increment >>
<< count >> 4 assert-eq

# Clean up
<< stop >> drop
```

Every line is either a definition or a test. The assertions at the bottom prove the actor behaves correctly. Run the file and silence means it works.

### A Temperature Converter

```
: f-to-c Int -> Int ; 32 - 5 * 9 / .
: c-to-f Int -> Int ; 9 * 5 / 32 + .

212 f-to-c 100 assert-eq
0 c-to-f 32 assert-eq
```

Small words, clear intent, self-testing.

### Fibonacci with Pattern Matching

From `samples/fib.a4`:

```
: fib Int -> Int ;
    : 0 -> 0 ;
    : 1 -> 1 ;
    : Int -> Int ;
        dup
        1 -
        fib
        swap
        2 -
        fib
        +.

0 fib 0 assert-eq
1 fib 1 assert-eq
6 fib 8 assert-eq
10 fib 55 assert-eq
```

No branching. No if/else. The dispatch mechanism selects the right clause based on the value on the stack.

### An Event Statistics Actor

```
type Stats
    hits Int
    errors Int .

: hit Stats -> Stats ;
    hits 1 + hits!.

: err Stats -> Stats ;
    errors 1 + errors!.

: report Stats -> Stats Int Int ;
    hits swap errors rot.

0 0 stats server

<< hit >>
<< hit >>
<< hit >>
<< err >>
<< report >>
# Stack now has: 3 (hits), 1 (errors), Actor
```

Two counters in one actor. Each operation touches only the field it cares about. The `report` call returns both values synchronously.


## Chapter 15: The Road Ahead

What you've seen is the prototype interpreter — ActorForth running inside Erlang. It works, it's tested, and it demonstrates the core ideas. But the vision goes further.

### Compiling to BEAM

The interpreter is useful for exploration, but ActorForth should compile to BEAM bytecode. The first target is **Core Erlang** — a stable intermediate representation that the Erlang compiler can optimize. An ActorForth word like:

```
: double Int -> Int ; dup + .
```

would compile to something equivalent to:

```erlang
double(X) -> X + X.
```

This gets all of BEAM's optimizations for free — dead code elimination, constant folding, register allocation. ActorForth words become real BEAM functions. Pattern matching sub-clauses map directly to Erlang function clauses.

### Self-Hosting

The ultimate goal: write the BEAM assembler in ActorForth itself. The assembler would be a set of types — `BeamModule`, `BeamFunction`, `BeamInstruction` — whose dictionaries give tokens meaning in the context of emitting bytecode. The same interpreter that runs your program would also compile it.

A `BeamModule` on the stack means tokens are module-level directives. A `BeamFunction` means tokens are function instructions. A `BeamInstruction` means tokens are opcodes and operands. The outer interpreter doesn't change — it still does the same five-step dispatch. The types on the stack determine whether you're writing a program or writing a compiler.

This is the promise of the TOS-driven dictionary mechanism: **any tool that processes text can be built as a set of types.** A compiler is just types for emitting code. A test framework is just types for asserting conditions. A network protocol handler is just types for parsing packets. The same interpreter drives them all.

---

## Quick Reference

### Stack Operations (Any dictionary)

| Word | Effect | Description |
|------|--------|-------------|
| `dup` | `( a -- a a )` | Duplicate top |
| `drop` | `( a -- )` | Discard top |
| `swap` | `( a b -- b a )` | Swap top two |
| `rot` | `( a b c -- b c a )` | Rotate third to top |
| `over` | `( a b -- a b a )` | Copy second to top |
| `2dup` | `( a b -- a b a b )` | Duplicate top two |
| `print` | `( a -- )` | Print and consume top |
| `stack` | `( -- )` | Display entire stack |
| `words` | `( -- )` | List all operations |
| `types` | `( -- )` | List all types |
| `load` | `( String -- )` | Load and interpret a .a4 file |

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

### Assertions (Any dictionary)

| Word | Effect | Description |
|------|--------|-------------|
| `assert` | `( Bool -- )` | Error with location if false |
| `assert-eq` | `( a b -- )` | Error with expected/actual if not equal |

### List Operations

| Word | Dict | Effect | Description |
|------|------|--------|-------------|
| `nil` | Any | `( -- List )` | Push empty list |
| `cons` | Any | `( a List -- List )` | Prepend item to list |
| `length` | List | `( List -- Int )` | Count elements |
| `head` | List | `( List -- a )` | First element |
| `tail` | List | `( List -- List )` | All but first |

### String Operations

| Word | Dict | Effect | Description |
|------|------|--------|-------------|
| `"hello"` | — | `( -- String )` | Quoted string literal |
| `string` | Any | `( Atom -- String )` | Explicit constructor |
| `concat` | String | `( Str Str -- Str )` | Concatenate (second + top) |
| `length` | String | `( Str -- Int )` | Byte length |
| `to-atom` | String | `( Str -- Atom )` | Convert to Atom |
| `to-int` | String | `( Str -- Int )` | Parse as integer |
| `to-string` | Int | `( Int -- Str )` | Convert to String |

### Map Operations

| Word | Dict | Effect | Description |
|------|------|--------|-------------|
| `map-new` | Any | `( -- Map )` | Push empty map |
| `map-put` | Any | `( Map val key -- Map )` | Add/update entry |
| `map-get` | Any | `( Map key -- val )` | Get value (errors if missing) |
| `map-delete` | Any | `( Map key -- Map )` | Remove entry |
| `map-has?` | Any | `( Map key -- Bool )` | Check key exists |
| `map-keys` | Map | `( Map -- List )` | All keys as list |
| `map-values` | Map | `( Map -- List )` | All values as list |
| `map-size` | Map | `( Map -- Int )` | Count of entries |

### Constructors (Any dictionary)

| Word | Effect | Description |
|------|--------|-------------|
| `int` | `( Atom -- Int )` | Parse text as integer |
| `bool` | `( Atom -- Bool )` | Parse "True"/"False" as boolean |
| `42` | `( -- Int )` | Literal handler (no explicit constructor needed) |
| `True` / `False` | `( -- Bool )` | Literal handler (no explicit constructor needed) |

### Word Definition

```
: name  InputTypes -> OutputTypes ; body .
```

### Pattern Matching Sub-Clauses

```
: name MasterSig ;
    : value-constrained-sig ;
    : value-constrained-sig ; body
    : general-sig ; body.
```

### Product Type Definition

```
type TypeName
    field1 Type1
    field2 Type2 .
```

Auto-generates: constructor (`typename`), non-destructive getters (`field1`, `field2`), setters (`field1!`, `field2!`). Getters push the field value while leaving the instance on the stack.

### Actor Operations

| Word | Effect | Description |
|------|--------|-------------|
| `server` | `( instance -- Actor )` | Spawn stateful actor from any typed instance |
| `self` | `( -- Actor )` | Push current process reference |
| `<<` | Enter send mode | Start actor message block |
| `>>` | Exit send mode | End actor message block, push results |
| `stop` | (inside `<< >>`) | Terminate actor, push final state |

### OTP Bridge (Erlang API)

| Function | Description |
|----------|-------------|
| `af_server:start_link(Script)` | Start bridge server from .a4 file |
| `af_server:call(Pid, Word, Args)` | Sync call, returns `{ok, Results}` |
| `af_server:cast(Pid, Word, Args)` | Async cast, returns `ok` |
| `af_server:eval(Pid, Line)` | Evaluate raw ActorForth line |
| `af_server:stop(Pid)` | Stop the server |

### Erlang FFI (Any dictionary)

| Word | Effect | Description |
|------|--------|-------------|
| `erlang-apply0` | `( Atom Atom -- Any )` | Call Module:Function() |
| `erlang-apply` | `( Atom Atom List -- Any )` | Call Module:Function(Args...) |

### Special Tokens

| Token | Behavior |
|-------|----------|
| `.` `:` `;` | Self-delimiting (no whitespace needed) |
| `#` | Comment to end of line |
| `"..."` | String literal (auto-converts to String type) |
