# ActorForth Definition

## Overview

ActorForth is an environment, compiler, interpreter, and program combined into one.

Outside of a small number of primitives, two stacks, and its interpreter,
there is no inherent syntax for ActorForth.

The execution environment is a concatenative, stack based, strongly typed language
that can be safely extended to build domain specific languages (DSLs). It targets
the Erlang BEAM VM, leveraging BEAM's native process model for actor concurrency.

All human/computer interaction starts via the interpreter/compiler REPL. From
entries initially appearing as **Atoms** in the input stream, global operators and
type-specific operators are applied to these Atoms to manipulate them into type
instance data, new operators, and create new syntax upon which to build more
powerful programs and systems.

ActorForth's development environment may be invoked as an interpreter by running
`af_repl:run_file/1` and passing a file name to it, or it may be run as a REPL
taking input from stdin via `af_repl:start/0`.

**Running an ActorForth script:**
```
$ rebar3 shell
1> af_repl:run_file("samples/fundamentals01.a4").
```

**Invoking ActorForth as a REPL:**
```
$ rebar3 shell
1> af_repl:start().
ActorForth REPL. ^C to exit.
ok: 10 20
ok: stack
Stack(2):
  0) 20 : Int
  1) 10 : Int
ok: *
ok: stack
Stack(1):
  0) 200 : Int
ok: print
200
```


## Primitives

All operations in ActorForth have a name and a stack signature. The stack signature
specifies what are the legal inputs present on the stack, in order for the operation
to be matched and invoked as well as what outputs will be left on the stack as a
result of the operation's invocation. To invoke an operation, both its name and input
stack signature must be matched.

### Tokens

Tokens are any combination of printable characters separated by whitespace except for
a few special punctuation characters which don't need whitespace to be tokenized.
White space consists of **space: ' '**, **tab: '\t'**, or **newline: '\n'**. The
special punctuation characters are **period: '.'**, **colon: ':'**, and **semi-colon:
';'**. Any token will be treated as a lookup request for an operation or, if not
found, will be converted to an **Atom** on the stack. See `src/af_parser.erl`
for details of how the parser is implemented.

### Atoms

Text input coming into the interpreter is first tokenized and tested against existing
operations. Any token that does not match an operation name is automatically converted
into an instance of the **Atom** type and placed on the stack. This means that there
aren't any actual syntax errors in ActorForth, only type errors. A TypeError occurs when
an operation is invoked without a valid input stack signature match or there is some
kind of invariant violation when trying to construct a type instance or as a result of
applying an operation onto a type instance. **Atoms** can be compared by applying the
**Bool** type operators as if they were strings but otherwise have no operations of
their own. They are intended to be the building blocks by which they get converted to
new operations, types, or data items.

**Parsing tokens as Atoms:**
```
ok: This is 5 atoms.
ok: stack
Stack(5):
  0) . : Atom
  1) atoms : Atom
  2) 5 : Atom
  3) is : Atom
  4) This : Atom
```

Note: The `.` is self-delimiting and becomes its own token. In this context (no
`CodeCompile` on the stack), it is pushed as an Atom.

### Literal Handlers

Before falling back to creating an Atom, the interpreter checks registered literal
handlers. Types can register a `literal` word that attempts to claim tokens. For
example, the Int type's literal handler recognizes numeric tokens like `42` and
pushes `{Int, 42}` directly:

```
ok: 42
ok: stack
Stack(1):
  0) 42 : Int
```

Both explicit (`42 int`) and implicit (`42`) forms produce the same result. Float
and Bool types also have literal handlers (`3.14` → `{Float, 3.14}`,
`True`/`False` → `{Bool, true/false}`).

### The 'Any' Type

There is also one more special type, ***Any***. It is used for generic matching in stack
type signatures and will match any type. This is critical for operations like those that
manipulate generic stack items like `dup` and `swap`.

The following generic stack manipulation operators are presently available:

`dup` : Any -> Any, Any

    Takes one input and copies it twice onto the stack.

    ok: hello dup
    ok: stack
    Stack(2):
      0) hello : Atom
      1) hello : Atom

`swap` : Any, Any -> Any, Any

    Takes two inputs and switches their order on the stack.

    ok: 10 20 swap
    ok: stack
    Stack(2):
      0) 10 : Int
      1) 20 : Int

`drop` : Any ->

    Removes the top object from the stack.

    ok: hello drop
    ok: stack
    Stack empty

`rot` : Any, Any, Any -> Any, Any, Any

    Brings the third item to the top.

    ok: 1 2 3 rot
    ok: stack
    Stack(3):
      0) 1 : Int
      1) 3 : Int
      2) 2 : Int

`over` : Any, Any -> Any, Any, Any

    Copies the second item to the top.

    ok: 1 2 over
    ok: stack
    Stack(3):
      0) 1 : Int
      1) 2 : Int
      2) 1 : Int

`2dup` : Any, Any -> Any, Any, Any, Any

    Duplicates the top two items.

    ok: 10 20 2dup
    ok: stack
    Stack(4):
      0) 20 : Int
      1) 10 : Int
      2) 20 : Int
      3) 10 : Int

`print` : Any ->

    Removes whatever is on top of the stack and prints it to stdout.

    ok: 42 print
    42


### Constructors

Types have a special form of operator called a constructor (**ctor**). A **ctor** is an
operation that takes one or more input type signatures and only leaves an item of
its type on the stack. It's generally considered good form to have a constructor that is
the same name as the type (in lower case) that takes an **Atom** as a stack input. But any
operation that converts from one type to a new type can be a **ctor**.

**Constructing an integer explicitly:**
```
ok: 42 int
ok: stack
Stack(1):
  0) 42 : Int
```

**Implicit construction via literal handler:**
```
ok: 42
ok: stack
Stack(1):
  0) 42 : Int
```

### Types and Type-Driven Dispatch

The central dispatch mechanism works as follows for each token:

1. Search TOS type's dictionary for the token
2. If not found, check TOS type's **handler**
3. If no handler, search **Any** (global) dictionary
4. Try literal handlers (Int, Float, Bool)
5. If still not found, push as `{Atom, Value}`

This means the TOS type IS the interpreter's state. The compiler is just four
types (`WordDefinition`, `InputTypeSignature`, `OutputTypeSignature`, `CodeCompile`)
whose handlers change what tokens mean — no mode switch needed.

### Word Definition

Words (user-defined operations) are created with the following syntax:

```
: name InputTypes -> OutputTypes ; body .
```

Example:
```
ok: : double Int -> Int ; dup + .
ok: 5 double
ok: stack
Stack(1):
  0) 10 : Int
```

### Pattern Matching via Overloaded Signatures

Multiple definitions of the same word with different signatures:

```
: factorial Int -> Int ;
    : 0 -> 0 ;
    : 1 -> 1 ;
    : Int -> Int ; dup 1 - factorial * .

6 factorial 720 assert-eq
```

Value-constrained signatures (`{Int, 0}`) are tried before general ones (`Int`).
This is the preferred control flow mechanism over if/else branching.

### Product Types

User-defined composite types with named fields:

```
type Point x Int y Int .
10 20 point
```

Auto-generates:
- Constructor `point` (takes values for each field)
- Non-destructive getters (`x`, `y`) — push field value, leave instance on stack
- Setters (`x!`, `y!`) — update field value

### Actor Primitives

ActorForth leverages BEAM's native process model. Each actor is an Erlang process
with its own continuation (stacks + execution state).

```
type Counter value Int .
: increment Counter -> Counter ; value 1 + value! .
: count Counter -> Counter Int ; value .

0 counter server

<< increment >>
<< increment >>
<< count >> 2 assert-eq
<< stop >> drop
```

- `server` spawns an actor from any typed instance
- `<<` enters send mode, `>>` exits
- Cast (no return values) is async, Call (returns values) is sync
- Auto-generated ops are excluded from the remote vocabulary (state is private)

### Python Interop

ActorForth can call Python code via the `erlang_python` library, embedding
Python in the BEAM VM:

```
py-start
"samples/python" py-import
"2 + 2" py-eval                       # -> {Int, 4}
16 math sqrt 1 py-call                 # -> {Float, 4.0}
"import sys" py-exec
```

Words: `py-start`, `py-call`, `py-call0`, `py-eval`, `py-exec`, `py-import`,
`py-venv`, `py-register`.

### Inter-Module Imports

The `import` word compiles a `.a4` file to a native BEAM module and loads its
words into the type registry:

```
"lib_math.a4" import
# Returns the module name as an Atom
# All words from lib_math.a4 are now available as native BEAM functions
```

The `load` word interprets a `.a4` file into the current continuation (adding
word definitions without native compilation):

```
"lib_math.a4" load
```

### Compile-Time Type Checking

Word definitions are type-checked at compile time. The compiler infers the stack
effect of each body token and compares against the declared output signature:

```
: double Int -> Int ; dup + .     # OK: inferred [Int] matches declared [Int]
: bad Int -> Bool ; dup + .       # ERROR: inferred [Int] doesn't match [Bool]
```

When inference is incomplete (unknown operations, product type accessors), the
compiler emits a warning instead of an error.

### Tail Call Optimization

Words whose body ends with a call to another compiled word (including self-calls)
are tail-call optimized. The word trace frame is popped before the tail call,
putting it in true Erlang tail position:

```
: countdown Int -> ;
    : 0 -> ; drop
    : Int -> ; 1 - countdown .

1000000 countdown    # Uses O(1) stack space
```

### Environment Configuration

The REPL automatically loads a `.env` file on startup, setting environment
variables (useful for API keys and configuration):

```
# .env
OPENAI_API_KEY=sk-xxx
MY_CONFIG=value
```

### Native BEAM Compilation

Words can be compiled to native BEAM modules:

```
: double Int -> Int ; dup + .
double af_math compile-to-beam
# af_math:double(5) => 10 from Erlang
```

### OTP Bridge

`af_server` wraps an ActorForth interpreter as a gen_server, making actors
supervisable OTP citizens:

```erlang
{ok, Pid} = af_server:start_link("counter.a4"),
af_server:cast(Pid, "increment", []),
{ok, [1]} = af_server:call(Pid, "count", []).
```
