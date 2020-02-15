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

class Type:

    # Types is a dictionary of Type names to their respective
    # custom dictionaries.  
    types : Dict[Type_name, List[Tuple[Op_name,Operation]]] = {}

    def __init__(self, typename: Type_name = "Unknown"):
        self.name = typename
        if not Type.types.get(self.name):
            Type.types[self.name] = []

    @property
    def forth_dict(self) -> List[Tuple[Op_name,Operation]]:
        return Type.types[self.name]

    def __eq__(self, type: object) -> bool:
        #print("equality check for %s against %s" % (self.name,type))
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

#   Global dictionary
forth_dict : List[Tuple[Op_name,Operation]] = []

# NOTE that atom is not a dictionary word but is the default behavior of
# our find atom functions.

forth_dict.insert(0,('print',op_print))

forth_dict.insert(0,('dup',op_dup))
forth_dict.insert(0,('swap',op_swap))
forth_dict.insert(0,('drop',op_drop))


def find_atom(s: Op_name) -> Tuple[Operation, bool]:
    for atom in forth_dict:
        if atom[0] == s: return atom[1], True
    # Not found.
    return op_atom, False

def find_type_atom(type: Type, s: Op_name) -> Tuple[Operation, bool]:
    for atom in type.forth_dict:
        if atom[0] == s: return atom[1], True
    # Not found.
    return op_atom, False 