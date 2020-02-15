#
#   af_types.py     - Types for our language.
#

import types
from typing import Dict, List, Tuple, Callable, Any
from enum import Enum
from dataclasses import dataclass

from stack import Stack

# An operation takes a stack instance and returns nothing.
Op_name = str
Operation = Callable[[Stack],None]
Type_name = str
Op_list = List[Tuple[Op_name, Operation]]

class Type:

    # Types is a dictionary of Type names to their respective
    # custom dictionaries.   

    types : Dict[Type_name, Op_list] = {}

    types["Any"] = [] # Global dictionary. Should it be "Any"/TAny? Probably.

    def __init__(self, typename: Type_name):
        self.name = typename
        if not Type.types.get(self.name):
            Type.types[self.name] = []

    # Inserts a new operations for the given type name (or global for None).
    @staticmethod
    def add_op(name: Op_name, op: Operation, type: Type_name = "Any") -> None:
        assert Type.types.get(type) is not None, "No type '%s' found. We have: %s" % (type,Type.types.keys()) 
        type_list = Type.types.get(type,[])        
        type_list.insert(0,(name, op))

    # Returns the first operation for this named type.
    @staticmethod
    def op(name: Op_name, type: Type_name = "Any") -> Tuple[Operation, bool]:
        assert Type.types.get(type) is not None, "No type '%s' found. We have: %s" % (type,Type.types.keys()) 
        type_list = Type.types.get(type,[])  
        for atom in type_list:
            if atom[0] == type: return atom[1], True
        # Not found.
        return op_atom, False

    def __eq__(self, type: object) -> bool:
        if isinstance(type, Type):
            return self.name == type.name
        return False

    def __str__(self) -> Type_name:
        return self.name

    def __repr__(self) -> str:
        return self.__str__()


@dataclass
class StackObject:
    value: Any
    type: Type 

@dataclass
class TypeSignature:
    stack_in : List[Type]
    stack_out : List[Type]

    def match_in(self, types: List[Type]) -> bool:
        return True

    def match_out(self, types: List[Type]) -> bool:
        return True

TAtom = Type("Atom")

TAny = Type("Any")

#
#   Generic operations
#
# Atom needs to take the symbol name to push on the stack.
def op_atom(s: Stack, s_id: Op_name = "Unknown") -> None:
    s.push(StackObject(s_id,TAtom))
op_atom.sig=TypeSignature([],[TAtom])


def op_print(s: Stack) -> None:
    op1 = s.pop().value
    print("'%s'" % op1)
op_print.sig=TypeSignature([TAny],[])

#
#   Should dup, swap, drop and any other generic stack operators 
#   dynamically determine the actual stack types on the stack and
#   create dynamic type signatures based on what are found?
#
def op_dup(s: Stack) -> None:
    op1 = s.tos()
    s.push(op1)
    print("'%s'" % op1)
op_dup.sig=TypeSignature([TAny],[TAny, TAny])

def op_swap(s: Stack) -> None:
    op1 = s.pop()
    op2 = s.pop()
    s.push(op1)
    s.push(op2)
    print("'%s','%s'" % (op1,op2))
op_swap.sig=TypeSignature([TAny, TAny],[TAny, TAny])

def op_drop(s: Stack) -> None:
    op1 = s.pop()
    print("'%s'" % op1)
op_drop.sig=TypeSignature([TAny],[])


#
#   Forth dictionary of primitive operations is created here.
#

Type.add_op('print', op_print)
Type.add_op('dup', op_dup)
Type.add_op('swap', op_swap)
Type.add_op('drop', op_drop)
