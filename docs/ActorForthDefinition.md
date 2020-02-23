# ActorForth Definition

## Overview

ActorForth is an environment, compiler, interpreter, and program combined into one.

Outside of a small number of primitives, two (presently one) stacks, and its interpreter, 
there is no inherent syntax for ActorForth.

The execution environment is a concatenative, stack based, strongly typed language 
that can be safely extended to build domain specific languages (DSLs).

All human/computer interaction starts via the interpreter/compiler REPL. From 
entries initially appearing as **Atoms** in the input stream, global operators and 
type-specific operators are applied to these Atoms to manipulate them into type
instance data, new operators, and create new syntax upon which to build more 
powerful programs and systems.

ActorForth's development environment may be invoked as an interpreter by running
interpret and passing a file name to it, or it may be run as a simple repl taking
input from stdin if no file name is passed to it.

**Running an ActorForth script:**
```
./interpret samples/fundamentals01.a4 
ActorForth demo interpreter. ^C to exit.
Global Dictionary : ['>=', '<=', '>', '<', '!=', '==', 'bool', 'bool', 'int', 'int', '2dup', 'drop', 'swap', 'dup', 'print']
Int Dictionary : ['/', '*', '-', '+']
Bool Dictionary : ['not']
Interpreting file: 'samples/fundamentals01.a4'.
junk
Stack(1) = [StackObject(value='junk', type=Atom)] 
40
Stack(2) = [StackObject(value='junk', type=Atom), StackObject(value='40', type=Atom)] 
int
match_in: in_types = [Atom]
match_in: stack_types = [Atom]
Stack(2) = [StackObject(value='junk', type=Atom), StackObject(value=40, type=Int)] 
2
Stack(3) = [StackObject(value='junk', type=Atom), StackObject(value=40, type=Int), StackObject(value='2', type=Atom)] 
int
match_in: in_types = [Atom]
match_in: stack_types = [Atom]
Stack(3) = [StackObject(value='junk', type=Atom), StackObject(value=40, type=Int), StackObject(value=2, type=Int)] 
+
match_in: in_types = [Int, Int]
match_in: stack_types = [Int, Int]
Stack(2) = [StackObject(value='junk', type=Atom), StackObject(value=42, type=Int)] 
print
match_in: in_types = [Any]
match_in: stack_types = [Int]
'42'
Stack(1) = [StackObject(value='junk', type=Atom)] 
True
Stack(2) = [StackObject(value='junk', type=Atom), StackObject(value='True', type=Atom)] 
bool
match_in: in_types = [Bool]
match_in: stack_types = [Atom]
match_in: Stack type <class 'type'> doesn't match input arg type Bool.
match_in: in_types = [Atom]
match_in: stack_types = [Atom]
Stack(2) = [StackObject(value='junk', type=Atom), StackObject(value=True, type=Bool)] 
False
Stack(3) = [StackObject(value='junk', type=Atom), StackObject(value=True, type=Bool), StackObject(value='False', type=Atom)] 
bool
match_in: in_types = [Bool]
match_in: stack_types = [Atom]
match_in: Stack type <class 'type'> doesn't match input arg type Bool.
match_in: in_types = [Atom]
match_in: stack_types = [Atom]
Stack(3) = [StackObject(value='junk', type=Atom), StackObject(value=True, type=Bool), StackObject(value=False, type=Bool)] 
[StackObject(value='junk', type=Atom), StackObject(value=True, type=Bool), StackObject(value=False, type=Bool)]
Stack max_depth = 3
Stack depth_history = [1, 2, 1, 2, 3, 2, 3, 2, 1, 2, 1, 2, 1, 2, 1, 2, 3, 2, 3]
Stack total operations = 19

end of line...

```

**Invoking ActorForth as a REPL**
```
./interpret 
ActorForth demo interpreter. ^C to exit.
Global Dictionary : ['>=', '<=', '>', '<', '!=', '==', 'bool', 'bool', 'int', 'int', '2dup', 'drop', 'swap', 'dup', 'print']
Int Dictionary : ['/', '*', '-', '+']
Bool Dictionary : ['not']
ok: 10
Stack(1) = [StackObject(value='10', type=Atom)] 
ok: int
match_in: in_types = [Atom]
match_in: stack_types = [Atom]
Stack(1) = [StackObject(value=10, type=Int)] 
ok: 20
Stack(2) = [StackObject(value=10, type=Int), StackObject(value='20', type=Atom)] 
ok: int
match_in: in_types = [Atom]
match_in: stack_types = [Atom]
Stack(2) = [StackObject(value=10, type=Int), StackObject(value=20, type=Int)] 
ok: *
match_in: in_types = [Int, Int]
match_in: stack_types = [Int, Int]
Stack(1) = [StackObject(value=200, type=Int)] 
ok: print
match_in: in_types = [Any]
match_in: stack_types = [Int]
'200'
Stack(0) = [] 
ok: ^C key interrupt.
[]
Stack max_depth = 2
Stack depth_history = [1, 0, 1, 2, 1, 2, 1, 0, 1, 0, 1, 0]
Stack total operations = 12

end of line...

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
White space consists of **space: ' '** , **tab: '\t'**, or **newline: '\n'**. The 
special punctuation characters are **period: '.'**, **colon: ':'**, and **semi-colon:
 ';'**. Any token will be treated as a lookup request for an operation or, if not 
found, will be converted to an **Atom** on the stack. See [parser.py](../src/parser.py)
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
Stack(1) = [StackObject(value='This', type=Atom)] 
ok: Stack(2) = [StackObject(value='This', type=Atom), StackObject(value='is', type=Atom)] 
ok: Stack(3) = [StackObject(value='This', type=Atom), StackObject(value='is', type=Atom), StackObject(value='5', type=Atom)] 
ok: Stack(4) = [StackObject(value='This', type=Atom), StackObject(value='is', type=Atom), StackObject(value='5', type=Atom), StackObject(value='atoms', type=Atom)] 
ok: Stack(5) = [StackObject(value='This', type=Atom), StackObject(value='is', type=Atom), StackObject(value='5', type=Atom), StackObject(value='atoms', type=Atom), StackObject(value='.', type=Atom)] 
```

### The 'Any' Type

There is also one more special type, ***Any***. It is used for generic matching in stack
type signatures and will match any type. This is critical for operations like those that 
manipulate generic stack items like dup and swap. 

**Generic type-idependent stack manipulations:**
```
ok: 17 42
Stack(1) = [StackObject(value='17', type=Atom)] 
ok: Stack(2) = [StackObject(value='17', type=Atom), StackObject(value='42', type=Atom)] 
ok: swap
match_in: in_types = [Any, Any]
match_in: stack_types = [Atom, Atom]
Stack(2) = [StackObject(value='42', type=Atom), StackObject(value='17', type=Atom)] 
ok: dup
match_in: in_types = [Any]
match_in: stack_types = [Atom]
Stack(3) = [StackObject(value='42', type=Atom), StackObject(value='17', type=Atom), StackObject(value='17', type=Atom)] 
```

### Constructors
Types have a special form of operator called a constructor (**ctor**). A **ctor** is an
operation that takes one or more input type signatures and only leaves an item of
its type on the stack. It's generally considered good form to have a constructor that is
the same name as the type (in lower case) that takes an **Atom** as a stack input. But any
operation that converts from one type to a new type can be a **ctor**. 

There is even a form of type inference for operations that take multiple inputs from the
stack. If the top input is an **Atom** but the next item on the stack is some other type,
ActorForth will attempt to apply a **ctor** against the **Atom** on the top of the stack 
before matching and invoking the operator. *Right now this is only implemented for Bool
operators that take two inputs.*

**Constructing an integer, an Atom, then performing implicit type conversion for the > operator:**
```
ok: 42
Stack(1) = [StackObject(value='42', type=Atom)] 
ok: int
match_in: in_types = [Atom]
match_in: stack_types = [Atom]
Stack(1) = [StackObject(value=42, type=Int)] 
ok: 17
Stack(2) = [StackObject(value=42, type=Int), StackObject(value='17', type=Atom)] 
ok: >
match_in: in_types = [Any, Any]
match_in: stack_types = [Int, Atom]
Stack(1) = [StackObject(value=True, type=Bool)]
```

