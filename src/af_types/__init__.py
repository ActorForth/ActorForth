"""
af_types/__init__.py - Types for our language. What everything is built on.

INTRO 5 : Types drive all ActorForth behavior and construction. ActorForth
          execution context is based on the Continuation (state) plus the
          Type behavior (words in the type dictionary) for whatever is on
          the top of the Stack in the Continuation.
"""

import logging
from typing import Dict, List, Tuple, Callable, Any, Optional
from dataclasses import dataclass


from stack import Stack
from aftype import AF_Type, AF_Continuation, StackObject, Symbol, Location
from operation import Op_list, Op_map, Op_name, Operation, TypeSignature, op_nop


Type_name = str


"""
INTRO 5.1 : The default execution handler (kind of an inter interpreter in
            FORTH parlance) simply takes a Continutation, looks up the
            Symbol in its list of Operations (ops_list) or the core (Any)
            dictionary if it can't be found there, and then calls the
            discovered Operation by passing the Continuation to it.

            Most types only ever want to execute the words looked up in
            their dictionaries. Those types should just use this handler.

            But sometimes you want to manipulate those words rather than
            execute them - such as when you're writing a compiler. Then
            you can create a specialized op handler that does something
            completely different.
"""
def default_op_handler(cont): # Had to remove Python typing notation to get it to compile.
    cont.op, found = Type.op(cont.symbol.s_id, cont)
    cont.op(cont)


"""
INTRO 5.2 : A TypeDefinition is defined as a list of named Operations, ops_list,
            and its handler, op_handler, which defines what you want to
            do with these named Operations when they're referenced.
"""
@dataclass
class TypeDefinition:
    ops_list: Op_list
    op_handler : Callable[["AF_Continuation"],None] = default_op_handler 


"""
INTRO 5.3 : The Type class holds all the TypeDefinitions in global
            dictionaries along with their special Constructors (ctors).
"""
class Type(AF_Type):

    """
    INTRO 5.4 : An individual Type is simply a named TypeDefinition.
    """
    types : Dict[Type_name, TypeDefinition] = {}

    """
    INTRO 5.5 : The core words in ActorForth are stored in the special
                "Any" Type. This is the global dictionary for words.
    """
    types["Any"] = TypeDefinition(ops_list = []) # Global dictionary.

    """
    INTRO 5.6 : Constructors (ctors) are special words that take one or
                more objects from the Stack and create an instance of the
                ctor's Type with them and then places the result on the
                Stack.

                It is through ctors that Atoms can become other useful
                types.

                Ctors are named for their Type and are not regular
                words in the Dictionaries. They can only return a single
                instance of their own Type.
    """
    ctors : Dict[Type_name, Op_map] = {}


    def __init__(self, typename: Type_name, handler = None):
        if handler is None:
            handler = default_op_handler
        self.name = typename
        if not Type.ctors.get(self.name, False):
            Type.ctors[self.name] = []
        if not Type.types.get(self.name, False):
            t_def = TypeDefinition(ops_list=[], op_handler=handler)
            Type.types[self.name] = t_def
        ## Do we need this? super().__init__(self)


    def ops(self) -> Op_list:
        t_def = Type.types.get(self.name,TypeDefinition(ops_list=[]))
        return t_def.ops_list


    # Typing doesn't like me having a return type specification here so dropped it.
    def handler(self):
        t_def = Type.types.get(self.name,TypeDefinition(ops_list=[]))
        return t_def.op_handler


    @staticmethod
    def register_ctor(name: Type_name, op: Operation, input_sig: List["StackObject"]) -> None:
        # Ctors only have TypeSignatures that return their own Type.
        # Register the ctor in the Global dictionary.
        op.sig = TypeSignature(input_sig,[StackObject(stype=Type(name))]) 
        # Type.add_op(op)

        # Append this ctor to our list of valid ctors.
        op_map = Type.ctors.get(name, None)
        assert op_map is not None, ("No ctor map for type %s found.\n\tCtors exist for the following types: %s." % (name, Type.ctors.keys()))
        op_map.append((input_sig,op))


    @staticmethod
    def find_ctor(name: Type_name, inputs : List["StackObject"]) -> Optional[Operation]:
        # Given a stack of input types, find the first matching ctor.
        logging.debug("Attempting to find a ctor for Type '%s' using the following input types: %s." % (name, inputs))
        logging.debug("Type '%s' has the following ctors: %s." % (name, Type.ctors))
        for type_sig in Type.ctors.get(name,[]):

            matching = False
            inputs = inputs.copy()
            try:
                for ctor_type in type_sig[0]:
                    in_type = inputs.pop(0).stype
                    if in_type.name == "Any" or ctor_type == "Any":
                        logging.debug("Matching ctor for 'Any' type.")
                        matching = True
                        continue
                    if in_type == ctor_type:
                        logging.debug("Matching ctor for specific '%s' type." % in_type)
                        matching = True
                    else:
                        logging.debug("Failed match for '%s' and '%s' types." % (in_type, ctor_type))
                        matching = False
                        break
            except IndexError:
                # wasn't enough on the stack to match
                logging.debug("Ran out of inputs to match a ctor for '%s' type." % name)
                matching = False
                break

            if matching == True:
                return type_sig[1]
        return None


    # Inserts a new operations for the given type name (or global for Any).
    @staticmethod
    def add_op(op: Operation, type_name: Type_name = None) -> None:
        if type_name is None:
            type_name = "Any"
        type_def = Type.types.get(type_name, None)
        assert type_def is not None, "No type '%s' found. We have: %s" % (type,Type.types.keys())
        if type_def:
            type_def.ops_list.insert(0,op)

        #print("\n\nADD_OP type_def type(%s) = %s." % (type(type_def), str(type_def)))

        logging.debug("Added Op:'%s' to %s context : %s." % (op,type_name,type_def))


    # Returns the first matching operation for this named type, defaulting to "make_atom"
    # and a boolean indicating whether a named operation was found.
    @staticmethod
    def find_op(name: Op_name, cont: AF_Continuation, type_name: Type_name) -> Tuple[Operation, bool]:

        type_def : TypeDefinition = Type.types[type_name] # (type_name,Type.types["Any"])
        cont.log.debug("Searching for op:'%s' in type: '%s'." % (name,type_name))
        assert type_def is not None, "No type '%s' found. We have: %s" % (type,Type.types.keys())
        name_found = False
        sigs_found : List[TypeSignature] = []
        if type_def:
            op_list = type_def.ops_list
            cont.log.debug("\top_list = %s" % ["name:'%s', sig:%s" % (op.name,op.sig.stack_in) for op in op_list])
            for op in op_list:
                if op.name == name:
                    name_found = True
                    sigs_found.append(op.sig)
                    # Now try to match the input stack...
                    # Should it be an exception to match the name but not the
                    # stack input signature? Probably so.

                    ### TODO : start using operation.check_stack_effect!!!
                    #if op.sig.match_in(cont.stack):
                    if op.check_stack_effect(cont.stack):

                        cont.log.debug("Found! Returning %s, %s, %s" % (op, op.sig, True))
                        return op, True
        # Not found.
        if name_found:
            # Is this what we want to do?
            # This will happen if names match but stacks don't.
            cont.log.debug("Continuation (stack = %s) doesn't match Op '%s' with available signatures: %s." % (cont.stack, name, [s.stack_in for s in sigs_found]))
            raise Exception("Continuation (stack = %s) doesn't match Op '%s' with available signatures: %s." % (cont.stack, name, [s.stack_in for s in sigs_found]))

        cont.log.debug("Not found!")
        # Default operation is to treat the symbol as an Atom and put it on the stack.
        return Operation("make_atom", make_atom, sig=TypeSignature([],[StackObject(stype=TAtom)])), False


    @staticmethod
    def op(name: Op_name, cont: AF_Continuation, type_name: Type_name = "Any") -> Tuple[Operation, bool]:
        tos = cont.stack.tos()
        op : Operation = Operation("invalid_result!", make_atom)
        sig : TypeSignature = TypeSignature([],[])
        found : bool = False

        if tos is not Stack.Empty:
            # We first look for an atom specialized for the type/value on TOS.
            op, found = Type.find_op(name, cont, tos.stype.name)

        if not found:
            # If Stack is empty or no specialized atom exists then search the global dictionary.
            op, found = Type.find_op(name, cont, "Any")

        if tos is not Stack.Empty and not found:
            # There's no such operation by that 'name' in existence
            # so let's find the default op for this type or else from the global dict
            # (as that's the make_atom op returned by default for Type.find_op.)
            cont.log.debug("Searching for default specialized for Type: %s." % tos.stype.name)
            op, found = Type.find_op('_', cont, tos.stype.name)
            op.name = name

        cont.log.debug("Type.op(name:'%s',cont.symbol:'%s' returning op=%s, sig=%s, found=%s." % (name,cont.symbol,op,sig,found))
        return op, found


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


"""
INTRO 5.7 : The Atom Type is what gets created any time a new Symbol
            shows up that the Interpreter does not recognize. Ultimately
            ALL objects initially are introduced into an ActorForth
            environment as Atoms.
"""
TAtom = Type("Atom")


"""
INTRO 5.8 : The Any Type is a special Type that will match ALL other types.
            It is also where the global word dictionary is contained.
"""
TAny = Type("Any")


#
#   Generic operations
#
"""
INTRO 5.9 : make_atom is the primitive that takes a token and converts
             it into an Atom on the Stack.

             Continue to operation.py for INTRO stage 6.
"""
# Atom needs to take the symbol name to push on the stack.
def make_atom(c: AF_Continuation) -> None:
    c.log.debug("make_atom c.symbol = '%s'" % c.symbol)
    if c.symbol is None:
        c.symbol = Symbol("Unknown", Location())
    c.stack.push(StackObject(value=c.symbol.s_id,stype=TAtom))
#Type.add_op(Operation('_', make_atom), TypeSignature([],[TAtom]))

