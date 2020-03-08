#
#   compiler.py     - Building new words/types for our language.
#

from typing import Dict, List, Tuple, Callable, Any, Optional
from dataclasses import dataclass

from af_types import *

TWordDefinition = Type("WordDefinition")
TInputTypeSignature = Type("InputTypeSignature")
TOutputTypeSignature = Type("OutputTypeSignature")

# Declared in af_types/__init__.py TCodeCompile = Type("CodeCompile")
global op_context
def nop(s: Stack) -> None:
    pass

# Just a place holder. op_context is updated by interpret.interpret.
op_context = Operation("nop",nop)

def op_new_word(s: Stack) -> None:
    """
    Atom -> WordDefinition(Op_name).

    Take an Atom, confirm that it's not already an active op,
    and turn it into a new WordDefinition.
    """
    itype = s.tos().type
    assert (itype == TAtom) or (itype == TWordDefinition), \
        "New words must be atoms or new word definitions. %s is a %s." % (s.tos().value, itype)

    op, sig, flags, found = Type.op(s.tos().value, s)
    assert not found, "Can't redefine an existing op." 

    if itype == TAtom:
        s.tos().type = TWordDefinition
Type.add_op(Operation(':',op_new_word), TypeSignature([TAtom],[TWordDefinition]))        

#
# Next three ops are for taking type names for type signatures.
#
def op_start_input_sig(s: Stack) -> None:
    """
    WordDefinition(Op_name) -> WordDefinition(Op_name), InputTypeSignature(TypeSignature).

    Works only there's a WordDefinition followed by an Atom on tos.
    Creates a new TypeSignature, adds the first item and pushes it to the stack.
    Does NOT consume the WordDefinition.
    """
    global op_context
    assert Type.types.get(op_context.name, False) is not False, \
        "%s is not a valid type name.\nValid types are: %s." % (op_context.name, [n for n in Type.types.keys()])
    #print("Got valid type: %s" % op_context.name)
    sig = TypeSignature([Type(op_context.name)],[])
    s.push(StackObject(sig,TInputTypeSignature))

def op_continue_input_sig(s: Stack) -> None:
    """
     WordDefinition(Op_name), InputTypeSignature(TypeSignature) 
        ->  WordDefinition(Op_name), InputTypeSignature(TypeSignature).

    Adds another Type to the InputTypeSignature list.
    """
    s.tos().value.stack_in.append(Type(op_context.name))

def op_output_sig(s: Stack) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature -> WordDefinition(Op_name), OutputTypeSignature.

    Should be invoked only when there's an InputTypeSignaure followed by the -> operator.
    Takes the existing TypeSignature, adds the first item to the output signature. 
    Does NOT consume the TypeSignature.
    """
    s.tos().value.stack_out.append(Type(op_context.name))

# Register every type name in order to be able to express Type Signatures at compile time.
for type_name in Type.types.keys():
    Type.add_op(Operation(type_name, op_start_input_sig), 
                TypeSignature([TWordDefinition],[TWordDefinition,TInputTypeSignature]), 
                WordFlags(), "WordDefinition")
    Type.add_op(Operation(type_name, op_continue_input_sig), 
                TypeSignature([TWordDefinition, TInputTypeSignature],[TWordDefinition, TInputTypeSignature]), 
                WordFlags(), "InputTypeSignature")  
    Type.add_op(Operation(type_name, op_output_sig), 
                TypeSignature([TWordDefinition, TOutputTypeSignature],[TWordDefinition, TOutputTypeSignature]), 
                WordFlags(), "OutputTypeSignature")          

def op_switch_to_output_sig(s: Stack) -> None:
    """
    WordDefinition(Op_name), InputTypeSignature(TypeSignature) 
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature).

    During a TypeSignature declaration, -> switches the compiler from building the
    input types to building the output types.
    """
    s.tos().type = TOutputTypeSignature
Type.add_op(Operation('->',op_switch_to_output_sig), 
            TypeSignature([TWordDefinition, TInputTypeSignature],[TWordDefinition, TOutputTypeSignature]), 
            WordFlags(), "InputTypeSignature")


def op_skip_to_output_sig(s: Stack) -> None:
    """
    WordDefinition(Op_name) -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature).

    Used when a WordDefition is encountered that has no InputTypeSignature.
    """
    sig = TypeSignature([],[])
    s.push(StackObject(sig,TOutputTypeSignature))
Type.add_op(Operation('->',op_skip_to_output_sig), 
            TypeSignature([TWordDefinition],[TWordDefinition, TOutputTypeSignature]), 
            WordFlags(), "WordDefinition")


def op_start_code_compile(s: Stack) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature) 
        -> WordDefinition(Op_name), CodeCompile(Operation).

    Signifies the completion of the TypeSignature for the new word.
    Switches to start the definition of the word's behavior.

    Constructs a new Operation declaration from STUFF
    """
    sig = s.pop().value # Later may need to copy and leave on the stack to support pattern matching.
    op = Operation(s.tos().value, op_execute_compiled_word)
    print("I'M COMPILING Op=%s!!!" % op)
    s.push( StackObject(op, TCodeCompile) )
Type.add_op(Operation(';',op_start_code_compile), 
            TypeSignature([TWordDefinition, TOutputTypeSignature],[TWordDefinition, TCodeCompile]), 
            WordFlags(), "OutputTypeSignature")


def op_skip_to_code_compile(s: Stack) -> None:
    """
    WordDefinition(Op_name) -> WordDefinition(Op_name), CodeCompile(Operation).

    Used if a new word definition is created but has no TypeSignature. 
    Creates the new empty TypeSignature, new Operation, and switches
    to start the definition of the word's behavior.
    """
    sig = TypeSignature([],[])
    s.push(StackObject(sig,TCodeCompile))  
    op_start_code_compile(s)
# Does this make sense yet? Type.add_op(':', op_new_word, TypeSignature([TWordDefinition],[TWordDefinition]))
Type.add_op(Operation(';',op_skip_to_code_compile), 
            TypeSignature([TWordDefinition],[TWordDefinition, TCodeCompile]), 
            WordFlags(), "WordDefinition")


def op_execute_compiled_word(s: Stack, words: Optional[List[Operation]] = None):
    print("Executing words: %s" % words)



