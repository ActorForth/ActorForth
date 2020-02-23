# ActorForth Definition

## Overview

ActorForth is an environment, compiler, interpreter, and program combined into one.

Outside of a small number of primitives, two (presently one) stacks, and its interpreter, 
there is no inherent syntax for ActorForth.

The execution environment is a concatenative, stack based, strongly typed language 
that can be safely extended to build domain specific languages (DSLs).

All human/computer interaction starts via the interpreter/compiler REPL. From 
entries initially appearing as Atoms in the input stream, global operators and 
type-specific operators are applied to these Atoms to manipulate them into type
instance data, new operators, and create new syntax upon which to build more 
powerful programs and systems.

## Primitives

All operations in ActorForth have a name and a stack signature. The stack signature
specifies what are the legal inputs present on the stack, in order for the operation
to be matched and invoked as well as what outputs will be left on the stack as a
result of the operation's invocation. To invoke an operation, both its name and input
stack signature must be matched. 

Types have a special form of operator called a constructor (ctor). A ctor is an
operation that takes one or more input type signatures and only leaves an item of
its type on the stack.

