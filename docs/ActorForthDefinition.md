# ActorForth Definition

### NOTE - TODO this document needs to be updated to remove the references to the python execution environment.

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
Global Dictionary : ['>=', '==', 'swap', 'debug', 'dup', ':', 'words', 'bool', 'print', 'drop', '>', '2dup', 'int', 'nop', 'stack', '<=', '<', '!=', 'types']
Int Dictionary : ['/', '*', '-', '+']
Bool Dictionary : ['not']
Debug Dictionary : ['off', 'on']
WordDefinition Dictionary : ['->', ';']
InputTypeSignature Dictionary : ['->']
OutputTypeSignature Dictionary : [';']
CodeCompile Dictionary : [';', '.']
Interpreting file: 'samples/fundamentals01.a4'.
ok: junk
Cont:
        sym=Symbol(s_id='junk', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=1))
         op=Op{'make_atom' [] -> [ Atom,] :(make_atom) []}
ok: 40
Cont:
        sym=Symbol(s_id='40', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=6))
         op=Op{'40' [] -> [ Atom,] :(make_atom) []}
ok: int
Cont:
        sym=Symbol(s_id='int', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=9))
         op=Op{'int' [ Atom,] -> [ Any,] :(op_int) []}
ok: 2
Cont:
        sym=Symbol(s_id='2', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=13))
         op=Op{'2' [] -> [ Atom,] :(make_atom) []}
ok: int
Cont:
        sym=Symbol(s_id='int', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=15))
         op=Op{'int' [ Atom,] -> [ Any,] :(op_int) []}
ok: +
Cont:
        sym=Symbol(s_id='+', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=19))
         op=Op{'+' [ Int, Int,] -> [ Int,] :(op_plus) []}
ok: print
'42'
Cont:
        sym=Symbol(s_id='print', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=21))
         op=Op{'print' [ Any,] -> [] :(op_print) []}
ok: True
Cont:
        sym=Symbol(s_id='True', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=27))
         op=Op{'True' [] -> [ Atom,] :(make_atom) []}
ok: bool
Cont:
        sym=Symbol(s_id='bool', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=32))
         op=Op{'bool' [ Atom,] -> [ Any,] :(op_bool) []}
ok: False
Cont:
        sym=Symbol(s_id='False', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=37))
         op=Op{'False' [] -> [ Atom,] :(make_atom) []}
ok: bool
Cont:
        sym=Symbol(s_id='bool', location=Location(filename='samples/fundamentals01.a4', linenum=1, column=43))
         op=Op{'bool' [ Atom,] -> [ Any,] :(op_bool) []}
ok: 
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
Global Dictionary : ['debug', 'nop', '==', 'words', 'types', '>=', '<=', '2dup', 'swap', 'bool', 'dup', '<', ':', 'stack', '!=', '>', 'drop', 'print', 'int']
Int Dictionary : ['/', '-', '*', '+']
Bool Dictionary : ['not']
Debug Dictionary : ['off', 'on']
WordDefinition Dictionary : [';', '->']
InputTypeSignature Dictionary : ['->']
OutputTypeSignature Dictionary : [';']
CodeCompile Dictionary : ['.', ';']
ok: debug on
Cont:
        sym=Symbol(s_id='debug', location=Location(filename='stdin', linenum=1, column=1))
         op=Op{'debug' [] -> [ Any,] :(op_debug) []}
ok: Cont:
        sym=Symbol(s_id='on', location=Location(filename='stdin', linenum=1, column=7))
         op=Op{'on' [ Debug,] -> [] :(op_on) []}
        Debug : On (Call Depth:0)
        Stack = empty
 
ok: 10
Cont:
        sym=Symbol(s_id='10', location=Location(filename='stdin', linenum=2, column=1))
         op=Op{'make_atom' [] -> [ Atom,] :(make_atom) []}
        Debug : On (Call Depth:0)
        Stack = 0) val=10,  type=Atom

 
ok: int
Cont:
        sym=Symbol(s_id='int', location=Location(filename='stdin', linenum=3, column=1))
         op=Op{'int' [ Atom,] -> [ Any,] :(op_int) []}
        Debug : On (Call Depth:0)
        Stack = 0) val=10,  type=Int

 
ok: 20
Cont:
        sym=Symbol(s_id='20', location=Location(filename='stdin', linenum=4, column=1))
         op=Op{'20' [] -> [ Atom,] :(make_atom) []}
        Debug : On (Call Depth:0)
        Stack = 0) val=20,  type=Atom
                1) val=10,  type=Int

 
ok: int
Cont:
        sym=Symbol(s_id='int', location=Location(filename='stdin', linenum=5, column=1))
         op=Op{'int' [ Atom,] -> [ Any,] :(op_int) []}
        Debug : On (Call Depth:0)
        Stack = 0) val=20,  type=Int
                1) val=10,  type=Int

 
ok: *
Cont:
        sym=Symbol(s_id='*', location=Location(filename='stdin', linenum=6, column=1))
         op=Op{'*' [ Int, Int,] -> [ Int,] :(op_multiply) []}
        Debug : On (Call Depth:0)
        Stack = 0) val=200,  type=Int

 
ok: print
'200'
Cont:
        sym=Symbol(s_id='print', location=Location(filename='stdin', linenum=7, column=1))
         op=Op{'print' [ Any,] -> [] :(op_print) []}
        Debug : On (Call Depth:0)
        Stack = empty
 
ok: ^C key interrupt.

[]
Stack max_depth = 2
Stack depth_history = [1, 0, 1, 0, 1, 2, 1, 2, 1, 0, 1, 0, 1, 0]
Stack total operations = 14

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

The following generic stack manipulation operators are presently available:

dup : Any -> Any, Any

    Takes one input and copies it twice onto the stack.
    
    ok: 1    
    Stack(1) = [StackObject(value='1', type=Atom)] 
    ok: dup
    match_in: in_types = [Any]
    match_in: stack_types = [Atom]
    Stack(2) = [StackObject(value='1', type=Atom), StackObject(value='1', type=Atom)] 
    

swap : Any1, Any2 -> Any2, Any1

    Takes two inputs and switches their order on the stack.

    ok: 1 2
    Stack(1) = [StackObject(value='1', type=Atom)] 
    ok: Stack(2) = [StackObject(value='1', type=Atom), StackObject(value='2', type=Atom)] 
    ok: swap
    match_in: in_types = [Any, Any]
    match_in: stack_types = [Atom, Atom]
    Stack(2) = [StackObject(value='2', type=Atom), StackObject(value='1', type=Atom)]

drop : Any -> 

    Removes the top object from the stack.

    ok: 1
    Stack(1) = [StackObject(value='1', type=Atom)] 
    ok: drop
    match_in: in_types = [Any]
    match_in: stack_types = [Atom]
    Stack(0) = []

2dup : Any1, Any2 -> Any1, Any2, Any1, Any2

    Takes the two top objects and duplicates them onto the stack.

    ok: 1 2
    Stack(1) = [StackObject(value='1', type=Atom)] 
    ok: Stack(2) = [StackObject(value='1', type=Atom), StackObject(value='2', type=Atom)] 
    ok: 2dup
    match_in: in_types = [Any, Any]
    match_in: stack_types = [Atom, Atom]
    Stack(4) = [StackObject(value='1', type=Atom), StackObject(value='2', type=Atom), StackObject(value='1', type=Atom), StackObject(value='2', type=Atom)] 

print : Any1 -> 

    Removes whatever is on top of the stack and prints it to stdout.

    ok: 1
    Stack(1) = [StackObject(value='1', type=Atom)] 
    ok: print
    match_in: in_types = [Any]
    match_in: stack_types = [Atom]
    '1'
    Stack(0) = []

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

