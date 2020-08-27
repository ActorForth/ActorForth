"""
af_types/__init__.py - Types for our language. What everything is built on.

INTRO 5 : Types drive all ActorForth behavior and construction. ActorForth
          execution context is based on the Continuation (state) plus the
          Type behavior (words in the type dictionary) for whatever is on
          the top of the Stack in the Continuation.
"""

import logging
from typing import Dict, List, Tuple, Callable, Any, Optional, Generator, Sequence
from dataclasses import dataclass
from itertools import chain


from stack import Stack
from aftype import AF_Type, AF_Continuation, StackObject, Symbol, Location
from operation import Op_list, Op_map, Op_name, Operation, Operation_def, TypeSignature, op_nop, SigValueTypeMismatchException


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
    #cont.op, found = Type.op(cont.symbol.s_id, cont)
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

    INTRO 5.5 : The core words in ActorForth are stored in the special generic
                "Any" Type. This is the global dictionary for words.
    """
    types : Dict[Type_name, TypeDefinition] = {"Any" : TypeDefinition(ops_list=[])}

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
    ctors : Dict[Type_name, Op_map] = {"Any":[]}

    def __init__(self, typename: Type_name, handler = None):
        assert Type.types["Any"]
        if handler is None:
            handler = default_op_handler
        self.name = typename
        if not self.is_generic():
            if not Type.ctors.get(self.name, False):
                Type.ctors[self.name] = []
            if not Type.types.get(self.name, False):
                t_def = TypeDefinition(ops_list=[], op_handler=handler)
                Type.types[self.name] = t_def    

    @staticmethod
    def is_generic_name(name: Type_name) -> bool:
        # Any types names "Any" or that start with underscore, '_', refer to 
        # generic types and will share the same word lookup.
        return name == "Any" or name.startswith('_')


    def is_generic(self) -> bool:        
        return Type.is_generic_name(self.name)
        #print("is_generic for %s is: %s." % (self.name, result))
        


    def words(self) -> Op_list:
        if self.is_generic():
            return Type.types["Any"].ops_list
        return Type.types[self.name].ops_list


    def handler(self):
        if self.is_generic():
            return Type.types["Any"].op_handler
        return Type.types[self.name].op_handler


    @staticmethod
    def get_type(name: Type_name) -> Optional["Type"]:
        if Type.types.get(name, None) is not None:
            return Type(name)
        return None


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
                ctor_obj : StackObject
                for ctor_obj in type_sig[0]:
                    in_obj = inputs.pop(0)
                    # Match against value first.
                    if ctor_obj.value is not None:
                        if ctor_obj.value != in_obj.value:
                            logging.debug("Failed value match for %s against ctor value %s." % (ctor_obj.value, in_obj.value))
                            matching = False
                            break
                    if in_obj.stype.is_generic() or ctor_obj.stype.is_generic():
                        logging.debug("Matching ctor for Generic 'Any' type.")
                        matching = True
                        continue
                    if in_obj.stype == ctor_obj.stype:
                        logging.debug("Matching ctor for specific '%s' type." % in_obj.stype)
                        matching = True
                    else:
                        logging.debug("Failed match for '%s' against ctor '%s' types." % (in_obj, ctor_obj))
                        matching = False
                        break
            except IndexError:
                # wasn't enough on the stack to match
                logging.debug("Ran out of inputs to match a ctor for '%s' type." % name)
                matching = False
                break

            if matching == True:
                result : Operation = type_sig[1]
                return result
        return None


    # Inserts a new operations for the given type name (or global for Any).
    @staticmethod
    def add_op(op: Operation, stack_in: Stack) -> None:
        type_def = Type.get_type("Any")
        if stack_in.depth() > 0:        
            type_def = stack_in.tos().stype
        assert type_def is not None

        # Once a word has been created for a Type (or global "Any"), 
        # we're going to enforce that the input signature length's be identical 
        # for now on.      
        all_named_words = chain(Type.find_named_ops_for_scope(op.name, type_def), 
                             Type.find_named_ops_for_scope(op.name, TAny))
        if type_def.is_generic():
            all_named_words = chain(Type.find_named_ops_for_scope(op.name, TAny))
        existing_words = [o for o in all_named_words if o.sig.stack_in.depth()!=op.sig.stack_in.depth()]
        if existing_words:
            assert existing_words, "ERROR - there are existing words of lengths other than %s : %s." \
                % (op.sig.stack_in.depth(), [(x,x.sig.stack_in.depth()) for x in existing_words])
        type_def.words().append(op)
        logging.debug("Added Op:'%s' to %s." % (op,type_def))


    @staticmethod
    def find_named_ops_for_scope(name: Op_name, type_context: "Type", recurse_option: Optional[Operation] = None) -> Generator[Operation, None, None]:
        logging.debug("find_named_ops_for_scope name:'%s', type_context:'%s', recurse_option:%s." % (name, type_context, recurse_option))
        for op in type_context.words():
            #logging.debug("Checking against: %s. op.name = '%s'." % (op, op.name))
            if op.name == name: 
                logging.debug("\tyielding op:%s" % op)
                yield(op)  # Return any matching Ops with this name.
        # If there's a possible recursive call for an unregistered method with an input type sig...
        if recurse_option is not None:
            if recurse_option.sig.stack_in.depth():
                # If the last type for the potential recursive call matches our scope...
                if recurse_option.sig.stack_in.tos().stype == type_context:
                    logging.debug("\tyielding recurse_option:%s" % recurse_option)
                    yield(recurse_option)
            # If the potential recursive call has no input sig and we're in global scope...
            elif type_context.is_generic(): 
                logging.debug("\tyielding 'Any' recurse_option:%s" % recurse_option)
                yield(recurse_option)        
        

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
                    try:
                        if op.check_stack_effect(cont.stack): # TODO - are we doing the right thing with this return types?
                            cont.log.debug("Found! Returning %s, %s, %s" % (op, op.sig, True))
                            return op, True
                    except SigValueTypeMismatchException:
                        # We found the name but not the right value/type sig. Keep looking.
                        pass
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
        # TODO : Word lookup is not matching based on values. need to fix this to proceed.
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

    # NOTE - we support comparisons between Type and str.
    def __eq__(self, t: object) -> bool:    
        if self.is_generic(): return True
        if isinstance(t, Type): return self.name == t.name
        return self.name == t


    def __ne__(self, t: object) -> bool:      
        if self.is_generic(): return False
        if isinstance(t, Type): return self.name != t.name
        return self.name != t


    def __lt__(self, t: object) -> bool:
        # Any Types come last in sorting line. Otherwise lexical sort by name.
        if self.is_generic(): return False
        if isinstance(t, Type): return self.name < t.name
        return self.name < str(t) # Typing requires that string cast. Odd.


    def __str__(self) -> Type_name:
        return self.name


    def __repr__(self) -> str:
        return self.__str__()


    def __hash__(self) -> int:
        return hash(self.name)


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
            All types that start with an underscore, '_', will also match
            the Any type in terms of dictionary access.
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


def make_word_context(word_name: Op_name, op_def: Operation_def, in_seq: Sequence["Type"] = [], out_seq: Sequence["Type"] = [])  -> None: 
    sig = TypeSignature(in_seq = [StackObject(stype=x) for x in in_seq], out_seq = [StackObject(stype=x) for x in out_seq])
    Type.add_op(Operation(word_name, op_def, sig=sig), sig.stack_in)
    
def t(name: str) -> Type:
    return Type(name)