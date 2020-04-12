#
#   compiler.py     - Building new words/types for our language.
#

from typing import Dict, List, Tuple, Callable, Any, Optional
from dataclasses import dataclass

from af_types import *
from af_types.af_any import op_swap


def compilation_word_handler(c: AF_Continuation) -> bool:
    # Lookup ONLY words for my specific type.
    assert c.symbol
    name = c.symbol.s_id
    op, sig, found = Type.find_op(name, c, c.stack.tos().type.name)

    # Is this a word specialized for my type matches my stack/type specification?
    if found and sig.match_in(c.stack):
        # Yes - so execute it.
        c.op = op
        c.op(c)
        return True
    return False

def type_sig_handler(c: AF_Continuation, type_name: str) -> None:
    handled = compilation_word_handler(c)
    if handled: return 

    #
    # NOTE - HERE'S WHERE WE'D DEAL WITH LITERALS/VALUES BY MAPPING TYPE SPECS & CTORS
    #

    # Is this word actually a type?
    assert c.symbol
    _type = Type.types.get(c.symbol.s_id,None)
    assert _type
    sig = TypeSignature([_type],[])
    c.stack.push(StackObject(sig,Type(type_name)))

def word_def_handler(c: AF_Continuation) -> None:
    type_sig_handler(c, "InputTypeSignature")

def input_type_handler(c: AF_Continuation) -> None:
    type_sig_handler(c, "InputTypeSignature")

def output_type_handler(c: AF_Continuation) -> None:
    type_sig_handler(c, "OutputTypeSignature")    


TWordDefinition = Type("WordDefinition", handler = word_def_handler)
TInputTypeSignature = Type("InputTypeSignature", handler = input_type_handler)
TOutputTypeSignature = Type("OutputTypeSignature", handler = output_type_handler)


#### THIS CATCHES ATOMS DURING COMPILATION
def op_compile_atom(c: AF_Continuation) -> None:
    if c.symbol is None:
        c.symbol = Symbol("Unknown", Location())
    #print("\n\nCompiling Atom: %s\n\n" % c.symbol.s_id)
    new_op = Operation(c.symbol.s_id, make_atom)
    c.stack.tos().value.add_word(new_op)

new_op = Operation('_', op_compile_atom)
new_sig = TypeSignature([Type("WordDefinition"),Type("OutputTypeSignature"),Type("CodeCompile")],
                        [Type("WordDefinition"),Type("OutputTypeSignature"),Type("CodeCompile")])
Type.types["CodeCompile"].ops_list.insert(0, (new_op,new_sig))


#
#
#
def compile_word_handler(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation) 
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')

    Given an Op_name, place it in the list of our Operation to be executed at runtime later.
    TODO: Confirm Type Signatures in & out of found words to enforce type safety.
    """
    handled = compilation_word_handler(c)
    if handled: return

    assert c.symbol
    op, sig, found = Type.op(c.symbol.s_id, c)

    ##
    ## THIS IS WHERE WE SHOULD DO TYPE CHECKING DURING COMPILATION
    ##

    if found:
        c.stack.tos().value.add_word(op)
    else:
        assert False        

TCodeCompile = Type("CodeCompile", handler = compile_word_handler)


def op_new_word(c: AF_Continuation) -> None:
    """
    Atom -> WordDefinition(Op_name).

    Take an Atom, confirm that it's not already an active op,
    and turn it into a new WordDefinition.
    """
    itype = c.stack.tos().type
    # Why is TWordDefinition ok here? For pattern matching/word overloading? 
    # Need to update the TypeSignature for it if we still want this.
    assert (itype == TAtom) or (itype == TWordDefinition), \
        "New words must be atoms or new word definitions. %s is a %s." % (c.stack.tos().value, itype)

    op, sig, found = Type.op(c.stack.tos().value, c)
    assert not found, "Can't redefine an existing op." 

    if itype == TAtom:
        c.stack.tos().type = TWordDefinition
Type.add_op(Operation(':',op_new_word), TypeSignature([TAtom],[TWordDefinition]))        
        

def op_switch_to_output_sig(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), InputTypeSignature(TypeSignature) 
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature).

    During a TypeSignature declaration, -> switches the compiler from building the
    input types to building the output types.
    """
    c.stack.tos().type = TOutputTypeSignature
Type.add_op(Operation('->',op_switch_to_output_sig), 
            TypeSignature([TWordDefinition, TInputTypeSignature],[TWordDefinition, TOutputTypeSignature]), 
            "InputTypeSignature")


def op_skip_to_output_sig(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name) -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature).

    Used when a WordDefition is encountered that has no InputTypeSignature.
    """
    sig = TypeSignature([],[])
    c.stack.push(StackObject(sig,TOutputTypeSignature))
Type.add_op(Operation('->',op_skip_to_output_sig), 
            TypeSignature([TWordDefinition],[TWordDefinition, TOutputTypeSignature]), 
            "WordDefinition")


def op_start_code_compile(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature) 
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation).

    Signifies the completion of the TypeSignature for the new word.
    Switches to start the definition of the word's behavior.

    Constructs a new Operation declaration from STUFF
    """
    #sig_s = c.stack.pop() # Later need to copy and leave on the stack to support pattern matching.
    #sig = sig.s.value

    # Grab the name of the new word from the WordDefinition
    op_swap(c)
    op = Operation(c.stack.tos().value, op_execute_compiled_word)
    op_swap(c)
    #c.stack.push(sig_s)
    #print("I'M COMPILING Op=%s!!!" % op)
    c.stack.push( StackObject(op, TCodeCompile) )
Type.add_op(Operation(';',op_start_code_compile), 
            TypeSignature([TWordDefinition, TOutputTypeSignature],[TWordDefinition, TOutputTypeSignature, TCodeCompile]), 
            "OutputTypeSignature")


def op_skip_to_code_compile(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name) -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation).

    Used if a new word definition is created but has no TypeSignature. 
    Creates the new empty TypeSignature, new Operation, and switches
    to start the definition of the word's behavior.
    """
    sig = TypeSignature([],[])
    c.stack.push(StackObject(sig,TOutputTypeSignature))  
    op_start_code_compile(c)
# Does this make sense yet? Type.add_op(':', op_new_word, TypeSignature([TWordDefinition],[TWordDefinition]))
Type.add_op(Operation(';',op_skip_to_code_compile), 
            TypeSignature([TWordDefinition],[TWordDefinition, TOutputTypeSignature, TCodeCompile]), 
            "WordDefinition")


def op_finish_word_compilation(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')
        -> WordDefinition
    """
    #print("finishing word compilation!")
    op = c.stack.pop().value
    sig = c.stack.pop().value
    Type.add_op(op,sig)
    new_op = Operation(op.name, op_compile_word, [op])
    new_sig = TypeSignature([Type("WordDefinition"),Type("OutputTypeSignature"),Type("CodeCompile")],
                    [Type("WordDefinition"),Type("OutputTypeSignature"),Type("CodeCompile")])
    Type.types["CodeCompile"].ops_list.insert(0, (new_op, new_sig))
Type.add_op(Operation(';',op_finish_word_compilation), 
            TypeSignature([TWordDefinition, TOutputTypeSignature, TCodeCompile],[TWordDefinition]), 
            "CodeCompile")


def op_finish_word_definition(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')
        -> (empty)
    """
    op_finish_word_compilation(c)
    c.stack.pop()
Type.add_op(Operation('.',op_finish_word_definition), 
            TypeSignature([TWordDefinition, TOutputTypeSignature, TCodeCompile],[]), 
            "CodeCompile")    


def _indent(c: AF_Continuation) -> str:
    return ''.join(['\t' for n in range(c.cdepth)])  


def op_execute_compiled_word(c: AF_Continuation) -> None:
    #print("\nop_execute_compiled_word c.stack.contents = %s." % c.stack.contents())
    op = c.op
    symbol = c.symbol
    words = op.words
    doutput = _indent(c) + op.name + " : "
    #print("\tExecuting %s words for %s : %s." % (len(words), op.name, words))
    for word in words:
        c.cdepth += 1
        doutput += "\n%s%s" % (_indent(c),word)
        #print("\n\t\tword: %s" % word)
        c.op = word
        c.symbol = Symbol(word.name, Location())
        word(c)
        c.cdepth -= 1
        #print("\n\t\t%s c.stack.contents = %s." % (word,c.stack.contents()))
    
    if c.debug:
        print(doutput)

    c.op = op
    c.symbol = symbol
        







