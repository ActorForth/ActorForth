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