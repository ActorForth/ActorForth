#
#   af_types.py     - Types for our language.
#

from typing import Dict, List, Tuple, Callable, Any, Optional
from dataclasses import dataclass

from stack import Stack

# An operation takes a stack instance and returns nothing.
Op_name = str
Operation = Callable[[Stack, Op_name],None]
Type_name = str


@dataclass
class TypeSignature:
    stack_in : List["Type"]
    stack_out : List["Type"]

    def match_in(self, stack: Stack) -> bool:
        if not len(self.stack_in): return True
        stack_types = [s.type for s in stack.contents()[len(self.stack_in)*-1:] ]

        print("match_in: in_types = %s" % (self.stack_in))
        print("match_in: stack_types = %s" % stack_types)
        for in_type in reversed(self.stack_in):
            if in_type == TAny: continue
            """
            Should probably have TAny types transform to the discovered type
            so that manipulations across generics are still completely type safe.
            """
            stack_type = stack_types.pop()
            if in_type != stack_type:
                print("match_in: Stack type %s doesn't match input arg type %s." % (type,in_type))
                return False
        #print("match_in: Found matching type for stack_in: %s" % self.stack_in)
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

    ctors : Dict[Type_name, Op_map] = {}


    def __init__(self, typename: Type_name):
        self.name = typename
        if not Type.ctors.get(self.name, False):
            Type.ctors[self.name] = []
        if not Type.types.get(self.name, False):
            Type.types[self.name] = []

    @staticmethod
    def register_ctor(name: Type_name, op_name: Op_name, op: Operation, sig: List["Type"]) -> None:
        # Ctors only have TypeSignatures that return their own Type.
        # Register the ctor in the Global dictionary.
        Type.add_op(op_name, op, TypeSignature(sig,[Type("Any")]))

        # Append this ctor to our list of valid ctors.
        op_map = Type.ctors.get(name, None)
        assert op_map is not None, ("No ctor map for type %s found.\n\tCtors exist for the following types: %s." % (name, Type.ctors.keys()))
        op_map.append((sig,op))

    @staticmethod
    def find_ctor(name: Type_name, inputs : List["Type"]) -> Optional[Operation]:
        # Given a stack of input types, find the first matching ctor.
        #print("Attempting to find a ctor for Type '%s' using the following input types: %s." % (self.name, inputs))
        #print("Type '%s' has the following ctors: %s." % (self.name, self.ctors))
        for type_sig in Type.ctors.get(name,[]):

            matching = False
            types = inputs.copy()
            try:
                for ctor_type in type_sig[0]:
                    in_type = types.pop(0)
                    if in_type.name == "Any" or ctor_type == "Any":
                        #print("Matching ctor for Any type.")
                        matching = True
                        continue
                    if in_type == ctor_type:
                        #print("Matching ctor for specific %s type." % in_type)
                        matching = True
                    else:
                        #print("Failed match for %s and %s types." % (in_type, ctor_type))
                        matching = False
                        break
            except IndexError:
                # wasn't enough on the stack to match
                #print("Ran out of inputs to match a ctor for %s type." % self.name)
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

    # Returns the first matching operation for this named type.
    @staticmethod
    def op(name: Op_name, stack: Stack, type: Type_name = "Any") -> Tuple[Operation, TypeSignature, WordFlags, bool]:
        #print("Searching for op:'%s' in type: '%s'." % (name,type))
        assert Type.types.get(type) is not None, "No type '%s' found. We have: %s" % (type,Type.types.keys()) 
        name_found = False
        op_list = Type.types.get(type,[])  
        #print("\top_list = %s" % [(name,sig.stack_in) for (name, op, sig, flags) in op_list])
        for op_name, op, sig, flags in op_list:
            if op_name == name:
                name_found = True
                # Now try to match the input stack...
                # Should it be an exception to match the name but not the 
                # stack input signature? Probably so.
                if sig.match_in(stack):

                    #print("Found! Returning %s, %s, %s, %s" % (op, sig, flags, True))
                    return op, sig, flags, True
        # Not found.
        if name_found:
            # Is this what we want to do?
            raise Exception("Stack content doesn't match Op %s." % sig.stack_in)

        #print ("Not found!")
        # This is redundant for what interpret already does by default.
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

def op_print(s: Stack, s_id: Op_name = None) -> None:
    op1 = s.pop().value
    print("'%s'" % op1)

#
#   Should dup, swap, drop and any other generic stack operators 
#   dynamically determine the actual stack types on the stack and
#   create dynamic type signatures based on what are found?
#
def op_dup(s: Stack, s_id: Op_name) -> None:
    op1 = s.tos()
    s.push(op1)

def op_swap(s: Stack, s_id: Op_name) -> None:
    op1 = s.pop()
    op2 = s.pop()
    s.push(op1)
    s.push(op2)

def op_drop(s: Stack, s_id: Op_name) -> None:
    op1 = s.pop()

def op_2dup(s: Stack, s_id: Op_name) -> None:
    op1 = s.tos()
    op_swap(s, s_id)
    op2 = s.tos()
    op_swap(s, s_id)
    s.push(op2)
    s.push(op1)


#
#   Forth dictionary of primitive operations is created here.
#

Type.add_op('print', op_print, TypeSignature([TAny],[]))
Type.add_op('dup', op_dup, TypeSignature([TAny],[TAny, TAny]))
Type.add_op('swap', op_swap, TypeSignature([TAny, TAny],[TAny, TAny]))
Type.add_op('drop', op_drop, TypeSignature([TAny],[]))
Type.add_op('2dup', op_2dup, TypeSignature([TAny, TAny],[TAny, TAny]))

