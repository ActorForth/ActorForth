#
#   af_types.py     - Types for our language.
#

import types
from typing import Dict, List, Tuple, Callable, Any
from enum import Enum
from dataclasses import dataclass

from stack import Stack



class Type:

    # Types is a dictionary of Type names to their respective
    # custom dictionaries.
    types : Dict[str, List[Tuple[str,Callable[[Stack, str],None]]]] = {}

    def __init__(self, typename: str = "Unknown"):
        self.name = typename
        if not Type.types.get(self.name):
            Type.types[self.name] = []

    @property
    def forth_dict(self) -> List[Tuple[str,Callable[[Stack, str],None]]]:
        return Type.types[self.name]

    def __eq__(self, type: object):
        print("equality check for %s against %s" % (self.name,type))
        if isinstance(type, Type):
            return self.name == type.name
        return False

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return self.__str__()

TAtom = Type("Atom")
TInt = Type("Int")
TAny = Type("Any")
#def TAny_eq(self, type: object):
#    print("Special equality check for Any")
#    return True
#TAny.__eq__ = types.MethodType(TAny_eq,TAny)
#TAny.__class__.__eq__ = types.MethodType(TAny_eq,TAny)


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


#
#   Generic operations
#

def op_atom(s: Stack, s_id: str) -> None:
    print("op_atom(s_id = '%s')\n" % s_id) 
    s.push(StackObject(s_id,TAtom))
op_atom.sig=TypeSignature([],[TAtom])

def op_print(s: Stack, s_id: str) -> None:
    print("op_print(s_id = '%s')\n" % s_id) 
    op1 = s.pop().value
    print("'%s'" % op1)
op_print.sig=TypeSignature([TAny],[])

#
#   Should dup, swap, drop and any other generic stack operators 
#   dynamically determine the actual stack types on the stack and
#   create dynamic type signatures based on what are found?
#
def op_dup(s: Stack, s_id: str) -> None:
    print("op_dup(s_id = '%s')\n" % s_id) 
    op1 = s.tos()
    s.push(op1)
    print("'%s'" % op1)
op_dup.sig=TypeSignature([TAny],[TAny, TAny])

def op_swap(s: Stack, s_id: str) -> None:
    print("op_swap(s_id = '%s')\n" % s_id) 
    op1 = s.pop()
    op2 = s.pop()
    s.push(op1)
    s.push(op2)
    print("'%s','%s'" % (op1,op2))
op_swap.sig=TypeSignature([TAny, TAny],[TAny, TAny])

def op_drop(s: Stack, s_id: str) -> None:
    print("op_drop(s_id = '%s')\n" % s_id) 
    op1 = s.pop()
    print("'%s'" % op1)
op_drop.sig=TypeSignature([TAny],[])

#
#   Integer handling
#

def op_int(s: Stack, s_id: str) -> None:
    print("op_int(s_id = '%s')\n" % s_id )
    i = int(s.pop().value)
    assert i <  999999999999, "int overflow > 999999999999"
    assert i > -999999999999, "int underflow < -999999999999"
    s.push(StackObject(i,TInt))
op_int.sig=TypeSignature([TAtom],[TInt])

def op_plus(s: Stack, s_id: str) -> None:
    print("op_plus(s_id = '%s')\n" % s_id) 
    op1 = s.pop().value
    op2 = s.pop().value
    result = op1+op2
    # Guarantee output is valid and not overflow.
    assert int(result) - op2 == op1, "python math error"
    s.push(StackObject(result,TInt))
    op_int(s,s_id) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
op_plus.sig=TypeSignature([TInt,TInt],[TInt])   

def op_minus(s: Stack, s_id: str) -> None:
    print("op_minus(s_id = '%s')\n" % s_id) 
    op1 = s.pop().value
    op2 = s.pop().value
    result = op2-op1
    # Guarantee output is valid and not overflow.
    assert int(result) + op1 == op2, "python math error"
    s.push(StackObject(result,TInt))
    op_int(s,s_id) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
op_minus.sig=TypeSignature([TInt,TInt],[TInt])  

#
#   Forth dictionary of primitive operations is created here.
#

#   Global dictionary
forth_dict : List[Tuple[str,Callable[[Stack, str],None]]] = []

forth_dict.insert(0,('print',op_print))

forth_dict.insert(0,('dup',op_dup))
forth_dict.insert(0,('swap',op_swap))
forth_dict.insert(0,('drop',op_drop))

#   Int dictionary
forth_dict.insert(0,('int',op_int))
TInt.forth_dict.insert(0,('+',op_plus))
TInt.forth_dict.insert(0,('-',op_minus))
