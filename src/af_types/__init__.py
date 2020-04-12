#
#   af_types.py     - Types for our language.
#

from typing import Dict, List, Tuple, Callable, Any, Optional
from dataclasses import dataclass


from stack import Stack

from aftype import AF_Type, AF_Continuation, Symbol, Location

from operation import Op_list, Op_map, Op_name, Operation, TypeSignature, op_nop

Type_name = str


#
# NOTE : had to remove Python typing notation from this function
#        to get things to compile clean.
def default_op_handler(cont):
    cont.op, sig, found = Type.op(cont.symbol.s_id, cont)
    cont.op(cont)

@dataclass
class TypeDefinition:
    ops_list: Op_list 
    op_handler : Callable[["AF_Continuation"],None] = default_op_handler

class Type(AF_Type):

    # Types is a dictionary of Type names to their respective
    # custom dictionaries.   

    types : Dict[Type_name, TypeDefinition] = {}

    types["Any"] = TypeDefinition(ops_list = []) # Global dictionary. 
    types["CodeCompile"] = TypeDefinition(ops_list = [])

    ctors : Dict[Type_name, Op_map] = {}


    def __init__(self, typename: Type_name):
        self.name = typename
        if not Type.ctors.get(self.name, False):
            Type.ctors[self.name] = []
        if not Type.types.get(self.name, False):
            Type.types[self.name] = TypeDefinition(ops_list = [])

        ## Do we need this? super().__init__(self)            

    @staticmethod
    def register_ctor(name: Type_name, op: Operation, sig: List["Type"]) -> None:
        # Ctors only have TypeSignatures that return their own Type.
        # Register the ctor in the Global dictionary.
        Type.add_op(op, TypeSignature(sig,[Type("Any")]))

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
                


    # Inserts a new operations for the given type name (or global for Any).
    @staticmethod
    def add_op(op: Operation, sig: TypeSignature, type_name: Type_name = "Any") -> None:
        type_def = Type.types.get(type_name, None)
        assert type_def is not None, "No type '%s' found. We have: %s" % (type,Type.types.keys()) 
        if type_def:
            type_def.ops_list.insert(0,(op, sig))            
        
        #print("\n\nADD_OP type_def type(%s) = %s." % (type(type_def), str(type_def)))

        #print("Added Op:'%s' to %s context : %s." % (op,type,type_list))

    # Returns the first matching operation for this named type.
    @staticmethod
    def find_op(name: Op_name, cont: AF_Continuation, type_name: Type_name = "Any") -> Tuple[Operation, TypeSignature, bool]:
        type_def = Type.types.get(type_name, None)
        print("Searching for op:'%s' in type: '%s'." % (name,type_name))
        assert type_def is not None, "No type '%s' found. We have: %s" % (type,Type.types.keys()) 
        name_found = False
        sigs_found : List[TypeSignature] = []
        if type_def:
            op_list = type_def.ops_list  
            print("\top_list = %s" % [(name,sig.stack_in) for (name, sig) in op_list])
            for op, sig, in op_list:
                if op.name == name:
                    name_found = True
                    sigs_found.append(sig)
                    # Now try to match the input stack...
                    # Should it be an exception to match the name but not the 
                    # stack input signature? Probably so.
                    if sig.match_in(cont.stack):

                        print("Found! Returning %s, %s, %s" % (op, sig, True))
                        return op, sig, True
        # Not found.
        if name_found:
            # Is this what we want to do?
            raise Exception("Continuation doesn't match Op '%s' with available signatures: %s." % (name, [s.stack_in for s in sigs_found]))

        print ("Not found!")
        # Default operation is to treat the symbol as an Atom and put it on the stack.
        return Operation("make_atom", make_atom), TypeSignature([],[TAtom]), False

    @staticmethod    
    def op(name: Op_name, cont: AF_Continuation, type_name: Type_name = "Any") -> Tuple[Operation, TypeSignature, bool]:
        tos = cont.stack.tos()        
        op : Operation = Operation("invalid_result!", make_atom)
        sig : TypeSignature = TypeSignature([],[])
        found : bool = False

        if tos is not Stack.Empty:
            # We first look for an atom specialized for the type/value on TOS.
            op, sig, found = Type.find_op(name, cont, tos.type.name)

        if not found:
            # If Stack is empty or no specialized atom exists then search the global dictionary.
            op, sig, found = Type.find_op(name, cont)

        if tos is not Stack.Empty and not found:
            # There's no such operation by that 'name' in existence 
            # so let's find the default op for this type or else from the global dict
            # (as that's the make_atom op returned by default for Type.find_op.)
            #print("Searching for default specialized for Type: %s." % tos.type.name)
            op, sig, found = Type.find_op('_', cont, tos.type.name)            
            op.name = name


        #print("Type.op(name:'%s',cont.symbol:'%s' returning op=%s, sig=%s, found=%s." % (name,cont.symbol,op,sig,found))
        return op, sig, found            

    def __eq__(self, type: object) -> bool:
        if self.name == "Any":
            return True
        if isinstance(type, Type):
            return self.name == type.name
        return False

    def __ne__(self, type: object) -> bool:
        if self.name == "Any":
            return False
        if isinstance(type, Type):
            return self.name != type.name
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
def make_atom(c: AF_Continuation) -> None:
    #print("make_atom c.symbol = %s" % c.symbol)
    if c.symbol is None:
        c.symbol = Symbol("Unknown", Location())
    c.stack.push(StackObject(c.symbol.s_id,TAtom))
#Type.add_op(Operation('_', make_atom), TypeSignature([],[TAtom]))

