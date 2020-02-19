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


@dataclass
class TypeSignature:
    stack_in : List["Type"]
    stack_out : List["Type"]

    def match_in(self, stack: Stack) -> bool:
        if not len(self.stack_in): return True
        stack_types = [s.type for s in stack.contents()[len(self.stack_in)*-1:] ]

        print("in_types = %s" % (self.stack_in))
        print("stack_types = %s" % stack_types)
        for in_type in reversed(self.stack_in):
            if in_type is TAny: continue
            """
            Should probably have TAny types transform to the discovered type
            so that manipulations across generics are still completely type safe.
            """
            stack_type = stack_types.pop()
            if in_type != stack_type:
                print("Stack type %s doesn't match input arg type %s." % (type,in_type))
                return False
        return True

    def match_out(self, on_stack_types: List["Type"]) -> bool:
        return True

@dataclass
class WordFlags:
    immediate : bool = False

Op_list = List[Tuple[Op_name, Operation, TypeSignature, WordFlags]]

Op_map = List[Tuple[List["Type"],Operation]]




class Type:

    # Types is a dictionary of Type names to their respective
    # custom dictionaries.   

    types : Dict[Type_name, Op_list] = {}

    types["Any"] = [] # Global dictionary. Should it be "Any"/TAny? Probably.


    def __init__(self, typename: Type_name):
        self.name = typename
        self.ctors : Op_map = []
        if not Type.types.get(self.name):
            Type.types[self.name] = []

    def register_ctor(self, name: Op_name, op: Operation, sig: List["Type"]) -> None:
        # Ctors only have TypeSignatures that return their own Type.
        # Register the ctor in the Global dictionary.
        Type.add_op(name, op, TypeSignature(sig,[self]))

        # Append this ctor to our list of valid ctors.
        self.ctors.append((sig,op))

    def find_ctor(self, inputs : List["Type"]) -> Operation:
        # Given a stack of input types, find the first matching ctor.
        for type_sig in self.ctors:
            matching = False
            types = inputs.copy()
            try:
                for ctor_type in type_sig[0]:
                    in_type = types.pop(0)
                    if in_type.name == "Any" or ctor_type == "Any":
                        matching = True
                        continue
                    if in_type == ctor_type:
                        matching = True
                    else:
                        matching = False
                        break
            except IndexError:
                # wasn't enough on the stack to match
                matching = False
                break

            if matching == True:
                return type_sig[1]
        return None
                


    # Inserts a new operations for the given type name (or global for None).
    @staticmethod
    def add_op(name: Op_name, op: Operation, sig: TypeSignature, flags: WordFlags = None, type: Type_name = "Any") -> None:
        assert Type.types.get(type) is not None, "No type '%s' found. We have: %s" % (type,Type.types.keys()) 
        if not flags:
            flags = WordFlags()
        type_list = Type.types.get(type,[])        
        type_list.insert(0,(name, op, sig, flags))

    # Returns the first operation for this named type.
    @staticmethod
    def op(name: Op_name, type: Type_name = "Any") -> Tuple[Operation, TypeSignature, WordFlags, bool]:
        #print("Searching for op:'%s' in type: '%s'." % (name,type))
        assert Type.types.get(type) is not None, "No type '%s' found. We have: %s" % (type,Type.types.keys()) 
        type_list = Type.types.get(type,[])  
        #print("\ttype_list = %s" % type_list)
        for atom in type_list:
            if atom[0] == name:
                #print("Found! Returning %s, %s, %s" % (atom[1],atom[2],True))
                return atom[1], atom[2], atom[3], True
        # Not found.
        #print ("Not found!")
        return make_atom, TypeSignature([],[TAtom]), WordFlags(), False

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



TAtom = Type("Atom")

TAny = Type("Any")

#
#   Generic operations
#
# Atom needs to take the symbol name to push on the stack.
def make_atom(s: Stack, s_id: Op_name = "Unknown") -> None:
    s.push(StackObject(s_id,TAtom))

def op_print(s: Stack) -> None:
    op1 = s.pop().value
    print("'%s'" % op1)

#
#   Should dup, swap, drop and any other generic stack operators 
#   dynamically determine the actual stack types on the stack and
#   create dynamic type signatures based on what are found?
#
def op_dup(s: Stack) -> None:
    op1 = s.tos()
    s.push(op1)

def op_swap(s: Stack) -> None:
    op1 = s.pop()
    op2 = s.pop()
    s.push(op1)
    s.push(op2)

def op_drop(s: Stack) -> None:
    op1 = s.pop()


#
#   Forth dictionary of primitive operations is created here.
#

Type.add_op('print', op_print, TypeSignature([TAny],[]))
Type.add_op('dup', op_dup, TypeSignature([TAny],[TAny, TAny]))
Type.add_op('swap', op_swap, TypeSignature([TAny, TAny],[TAny, TAny]))
Type.add_op('drop', op_drop, TypeSignature([TAny],[]))
