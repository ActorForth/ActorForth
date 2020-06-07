#
#   compiler.py     - Building new words/types for our language.
#

from typing import Dict, List, Tuple, Callable, Any, Optional, Sequence
from dataclasses import dataclass

from af_types import *
from af_types.af_any import op_swap, op_stack


def input_type_handler(c: AF_Continuation) -> None:
    type_sig_handler(c, "InputTypeSignature")

def output_type_handler(c: AF_Continuation) -> None:
    type_sig_handler(c, "OutputTypeSignature")    

def code_compile_handler(c: AF_Continuation ) -> None:
    return compile_word_handler(c)

TWordDefinition = Type("WordDefinition")
TInputTypeSignature = Type("InputTypeSignature", handler = input_type_handler)
TOutputTypeSignature = Type("OutputTypeSignature", handler = output_type_handler)
TCodeCompile = Type("CodeCompile", handler = code_compile_handler)


def op_new_word(c: AF_Continuation) -> None:
    """
    Atom -> WordDefinition(Op_name), InputTypeSignature(TypeSignature).

    Take an Atom, confirm that it's not already an active op,
    and turn it into a new WordDefinition.
    """
    op_name = c.stack.tos().value
    op, found = Type.op(op_name,c)  # Do we need to check that it's also not a ctor/type? Probably so.
    assert not found, "Compile error: '%s' already defined." % op_name
    c.stack.tos().type = TWordDefinition

    sig = TypeSignature([],[])
    c.stack.push(StackObject(sig,TInputTypeSignature))

Type.add_op(Operation(':',op_new_word, sig=TypeSignature([TAtom],[TWordDefinition, TInputTypeSignature])) )       
        

def op_switch_to_output_sig(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), InputTypeSignature(TypeSignature) 
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature).

    During a TypeSignature declaration, -> switches the compiler from building the
    input types to building the output types.
    """
    c.stack.tos().type = TOutputTypeSignature
Type.add_op(Operation('->',op_switch_to_output_sig, 
            sig=TypeSignature([TWordDefinition, TInputTypeSignature],[TWordDefinition, TOutputTypeSignature]) ), 
            "InputTypeSignature") 


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
Type.add_op(Operation(';',op_start_code_compile, 
            sig = TypeSignature([TWordDefinition, TOutputTypeSignature],[TWordDefinition, TOutputTypeSignature, TCodeCompile]) ), 
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
Type.add_op(Operation(';',op_skip_to_code_compile, 
            sig=TypeSignature([TWordDefinition],[TWordDefinition, TOutputTypeSignature, TCodeCompile]) ), 
            "WordDefinition")


def op_finish_word_compilation(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')
        -> WordDefinition
    """
    #print("finishing word compilation!")
    op = c.stack.pop().value
    op.sig = c.stack.pop().value
    Type.add_op(op)
    # new_op = Operation(op.name, op_compile_word, [op])
    # new_sig = TypeSignature([Type("WordDefinition"),Type("OutputTypeSignature"),Type("CodeCompile")],
    #                 [Type("WordDefinition"),Type("OutputTypeSignature"),Type("CodeCompile")])
    # Type.types["CodeCompile"].ops_list.insert(0, (new_op, new_sig))
Type.add_op(Operation(';',op_finish_word_compilation, 
            sig=TypeSignature([TWordDefinition, TOutputTypeSignature, TCodeCompile],[TWordDefinition]) ), 
            "CodeCompile")


def op_finish_word_definition(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')
        -> (empty)
    """
    op_finish_word_compilation(c)
    c.stack.pop()
Type.add_op(Operation('.',op_finish_word_definition, 
            sig=TypeSignature([TWordDefinition, TOutputTypeSignature, TCodeCompile],[]) ), 
            "CodeCompile")    


def _indent(c: AF_Continuation) -> str:
    return ''.join(['\t' for n in range(c.cdepth)])  


# For executing COMPILE TIME words only! 
def compilation_word_handler(c: AF_Continuation) -> bool:
    #print("compilation_word_handler")
    # Lookup ONLY words for my specific type.
    assert c.symbol
    name = c.symbol.s_id
    op, found = Type.find_op(name, c, c.stack.tos().type.name)

    # Is this a word specialized for my type matches my stack/type specification?
    if found and op.sig.match_in(c.stack):
        # Yes - so execute it.
        c.op = op
        c.op(c)
        return True
    return False


def type_sig_handler(c: AF_Continuation, type_name: str) -> None:
    #print("\n\nstarting type_sig_handler")  
    handled = compilation_word_handler(c)
    out = "type_sig_handler for type_name='%s' : received for symbol: %s "
    if handled: out += "HANDLED by compilation_word_handler."
    #print(out % (type_name, c.symbol))
    if handled: return 

    #
    # NOTE - HERE'S WHERE WE'D DEAL WITH LITERALS/VALUES BY MAPPING TYPE SPECS & CTORS
    #

    # Is this word actually a type?
    assert c.symbol
    _type = Type.types.get(c.symbol.s_id,None)
    assert _type
    if type_name == "InputTypeSignature":
        c.stack.tos().value.stack_in.append(Type(c.symbol.s_id))
    else:
        c.stack.tos().value.stack_out.append(Type(c.symbol.s_id))


def compile_word_handler(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation) 
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')

    Given an Op_name, place it in the list of our Operation to be executed at runtime later.
    TODO: Confirm Type Signatures in & out of found words to enforce type safety.
    """
    #print("compile_word_handler starting")
    handled = compilation_word_handler(c)
    if handled: return

    assert c.symbol
    #print("looking up symbol.s_id = %s" % c.symbol.s_id)
    op_name = c.symbol.s_id
    found = False

    ##
    ## THIS IS WHERE WE SHOULD DO TYPE CHECKING DURING COMPILATION
    ##
    tos_output_sig : Sequence["AF_Type"] = []

    if c.stack.tos().value.words:
        # Match to the output stack of our last word in this definition.
        words = c.stack.tos().value.words 
        tos_output_sig = words[-1].sig.stack_out
        #print("Match to prior word's output sig: %s" % tos_output_sig)

    else:
        # Match to the input stack of the input defintion of our word.
        op_swap(c)
        tos_output_sig = c.stack.tos().value.stack_in
        op_swap(c)
        #print("Match to current word's input sig: %s" % tos_output_sig)

    if len(tos_output_sig):
        # First try to match up with an op specialized for this type.

        # Have to create a fake continuation for type matching.
        fake_c = AF_Continuation(stack = Stack())
        for t in tos_output_sig:
            fake_c.stack.push(StackObject(None, t))

        output_type_name = tos_output_sig[-1].name
        #print("fake Continuation stack for find_op: %s" % fake_c.stack.contents())
        op, found = Type.find_op(op_name, fake_c, output_type_name)

        ### HACK HACK
        ### Because TypeSignatures may output an "Any" type, we really need to replace
        ### them with the concrete output type for proper type checking.
        ### For now just jump through all the types and see if we find one and hope
        ### there are no word collisions.
        if not found and output_type_name == "Any":
            for output_type_name in Type.types.keys():
                if output_type_name == "Any": continue
                if found: break
                op, found = Type.find_op(op_name, fake_c, output_type_name)

    if not found:
        # Next try to match up with an op for Any type.
        op, found = Type.find_op(op_name, c)


    if not found:
        # See if there's a ctor for this name?
        ctor = Type.find_ctor(op_name, [TAny,])
        if ctor is not None:
            c.stack.tos().value.add_word(ctor)
            found = True


    if found:
        c.stack.tos().value.add_word(op)
    else:
        print("FAILED TO FIND WORD TO COMPILE %s" % c.symbol.s_id )

        print("Compile as literal")
        #assert False   
        def curry_make_atom(s, func = make_atom ):
            def compiled_make_atom( c: AF_Continuation ):
                c.symbol = s
                return func(c)
        new_op = Operation(op_name, curry_make_atom(op_name), sig=TypeSignature([],[TAtom]))
        print("New anonymous function: %s" % new_op)
        c.stack.tos().value.add_word( new_op )

        #print("compile_word_handler ending")     

        ### MERGE CONFLICT HERE c.stack.tos().value.add_word(Operation(c.symbol.s_id, make_atom))
        # assert False   


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