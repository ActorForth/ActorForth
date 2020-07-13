#
#   compiler.py     - Building new words/types for our language.
#

import logging
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
    c.stack.tos().stype = TWordDefinition

    sig = TypeSignature([],[])
    c.stack.push(StackObject(value=sig,stype=TInputTypeSignature))

Type.add_op(Operation(':',op_new_word, sig=TypeSignature([StackObject(stype=TAtom)],
            [StackObject(stype=TWordDefinition), StackObject(stype=TInputTypeSignature)])) )


def op_switch_to_output_sig(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), InputTypeSignature(TypeSignature)
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature).

    During a TypeSignature declaration, -> switches the compiler from building the
    input types to building the output types.
    """
    c.stack.tos().stype = TOutputTypeSignature
Type.add_op(Operation('->',op_switch_to_output_sig,
            sig=TypeSignature([StackObject(stype=TWordDefinition), StackObject(stype=TInputTypeSignature)],
                        [StackObject(stype=TWordDefinition), StackObject(stype=TOutputTypeSignature)]) ),
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
    c.stack.push( StackObject(value=op, stype=TCodeCompile) )
Type.add_op(Operation(';',op_start_code_compile,
            sig = TypeSignature([StackObject(stype=TWordDefinition), StackObject(stype=TOutputTypeSignature)],
                    [StackObject(stype=TWordDefinition), StackObject(stype=TOutputTypeSignature), StackObject(stype=TCodeCompile)]) ),
                    "OutputTypeSignature")


def op_skip_to_code_compile(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name) -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation).

    Used if a new word definition is created but has no TypeSignature.
    Creates the new empty TypeSignature, new Operation, and switches
    to start the definition of the word's behavior.
    """
    sig = TypeSignature([],[])
    c.stack.push(StackObject(value=sig, stype=TOutputTypeSignature))
    op_start_code_compile(c)
# Does this make sense yet? Type.add_op(':', op_new_word, TypeSignature([TWordDefinition],[TWordDefinition]))
Type.add_op(Operation(';',op_skip_to_code_compile,
            sig=TypeSignature([StackObject(stype=TWordDefinition)],
                [StackObject(stype=TWordDefinition), StackObject(stype=TOutputTypeSignature), StackObject(stype=TCodeCompile)]) ),
                "WordDefinition")


def op_finish_word_compilation(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')
        -> WordDefinition
    """
    c.log.debug("finishing word compilation!")
    op = c.stack.pop().value
    pop_value = c.stack.pop().value
    op.sig = pop_value
    s_in = pop_value.stack_in

    if s_in.is_empty() :
        c.log.debug("'%s' operation being added to global dictionary." % op.name)
        Type.add_op(op)
    else:
        s_in_tos : StackObject = s_in.tos()
        c.log.debug("'%s' operation being added to '%s' dictionary." % (op.name, s_in_tos.value))
        Type.add_op(op, s_in_tos.value)

    # new_op = Operation(op.name, op_compile_word, [op])
    # new_sig = TypeSignature([Type("WordDefinition"),Type("OutputTypeSignature"),Type("CodeCompile")],
    #                 [Type("WordDefinition"),Type("OutputTypeSignature"),Type("CodeCompile")])
    # Type.types["CodeCompile"].ops_list.insert(0, (new_op, new_sig))
Type.add_op(Operation(';',op_finish_word_compilation,
            sig=TypeSignature([StackObject(stype=TWordDefinition), StackObject(stype=TOutputTypeSignature), StackObject(stype=TCodeCompile)],
                    [StackObject(stype=TWordDefinition)]) ),
                    "CodeCompile")


def op_finish_word_definition(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')
        -> (empty)
    """
    op_finish_word_compilation(c)
    c.stack.pop()
Type.add_op(Operation('.',op_finish_word_definition,
            sig=TypeSignature([StackObject(stype=TWordDefinition), StackObject(stype=TOutputTypeSignature), StackObject(stype=TCodeCompile)],
                []) ),
                "CodeCompile")


def _indent(c: AF_Continuation) -> str:
    return ''.join(['\t' for n in range(c.cdepth)])


# For executing COMPILE TIME words only!
def compilation_word_handler(c: AF_Continuation) -> bool:
    c.log.debug("compilation_word_handler")
    # Lookup ONLY words for my specific type.
    assert c.symbol
    name = c.symbol.s_id
    op, found = Type.find_op(name, c, c.stack.tos().stype.name)

    # Is this a word specialized for my type matches my stack/type specification?
    if found and op.sig.match_in(c.stack):
        # Yes - so execute it.
        c.op = op
        c.op(c)
        return True
    return False


def type_sig_handler(c: AF_Continuation, type_name: str) -> None:
    c.log.debug("\n\nstarting type_sig_handler")
    handled = compilation_word_handler(c)
    out = "type_sig_handler for type_name='%s' : received for symbol: %s "
    if handled: out += "HANDLED by compilation_word_handler."
    c.log.debug(out % (type_name, c.symbol))
    if handled: return

    #
    # NOTE - HERE'S WHERE WE'D DEAL WITH LITERALS/VALUES BY MAPPING TYPE SPECS & CTORS
    #

    # Is this word actually a type?
    assert c.symbol
    c.log.debug("Looking up a type called '%s'." % c.symbol.s_id)
    _type = Type.types.get(c.symbol.s_id,None)
    assert _type, "%s isn't an existing type : %s" % (_type, Type.types.keys())
    if type_name == "InputTypeSignature":
        #c.stack.tos().value.stack_in.append(Type(c.symbol.s_id))
        c.stack.tos().value.stack_in.push(StackObject(stype=Type(c.symbol.s_id)) )
    else:
        #c.stack.tos().value.stack_out.append(Type(c.symbol.s_id))
        c.stack.tos().value.stack_out.push(StackObject(stype=Type(c.symbol.s_id)) )


def compile_word_handler(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation)
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')

    Given an Op_name, place it in the list of our Operation to be executed at runtime later.
    TODO: Confirm Type Signatures in & out of found words to enforce type safety.
    """
    c.log.debug("compile_word_handler starting")
    handled = compilation_word_handler(c)
    if handled: return

    assert c.symbol
    c.log.debug("looking up symbol.s_id = %s" % c.symbol.s_id)
    op_name : Op_name = c.symbol.s_id
    found : bool = False

    ##
    ## THIS IS WHERE WE SHOULD DO TYPE CHECKING DURING COMPILATION
    ##
    tos_output_sig : Stack = Stack()

    op : Operation = c.stack.tos().value
    op_words : List[Operation] = op.words
    if op_words:
        # Match to the output stack of our last word in this definition.        
        tos_output_sig = op_words[-1].sig.stack_out
        c.log.debug("Match to prior word's output sig: %s" % tos_output_sig)

    else:
        # Match to the input stack of the input defintion of our word.
        op_swap(c)
        tos_output_sig = op.sig.stack_in
        op_swap(c)
        c.log.debug("Match to current word's input sig: %s" % tos_output_sig)

    if len(tos_output_sig):
        # First try to match up with an op specialized for this type.

        # Have to create a fake continuation for type matching.
        fake_c = AF_Continuation(stack = Stack())
        for t in tos_output_sig.contents():
            fake_c.stack.push(StackObject(stype=t, value=None))

        output_type_name = tos_output_sig.contents()[-1].name
        c.log.debug("fake Continuation stack for find_op: %s" % fake_c.stack.contents())
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
        op, found = Type.find_op(op_name, c, "Any")


    if not found:
        # See if there's a ctor for this name?
        ctor = Type.find_ctor(op_name, [StackObject(stype=TAny),])
        if ctor is not None:

            ### TODO:   See if the prior word is a literal/atom (how to tell?)
            ###         and, if so, go ahead and apply the ctor and therefore
            ###         set it's value/type at compile time.

            c.stack.tos().value.add_word(ctor)
            found = True




    if found:
        c.stack.tos().value.add_word(op)
    else:
        c.log.debug("FAILED TO FIND WORD TO COMPILE '%s'" % c.symbol.s_id )

        c.log.debug("Compile as literal")
        #assert False
        def curry_make_atom(s, func = make_atom ):
            def compiled_make_atom( c: AF_Continuation ):
                c.symbol = c.symbol
                return func(c)
            return compiled_make_atom
        op_implementation = curry_make_atom(op_name, make_atom)                
        new_op = Operation(op_name, op_implementation, sig=TypeSignature([],[StackObject(stype=TAtom)]))
        c.log.debug("New anonymous function: %s" % new_op)
        c.stack.tos().value.add_word( new_op )

        c.log.debug("compile_word_handler ending")


def op_execute_compiled_word(c: AF_Continuation) -> None:
    c.log.debug("\nop_execute_compiled_word c.stack.contents = %s." % c.stack.contents())
    op = c.op
    symbol = c.symbol
    words = op.words
    doutput = _indent(c) + op.name + " : "

    c.log.debug("\tExecuting %s words for %s : %s." % (len(words), op.name, words))
    for word in words:
        c.cdepth += 1
        doutput += "\n%s%s" % (_indent(c),word)
        c.log.debug("\n\t\tword: %s" % word)
        c.op = word
        c.symbol = Symbol(word.name, Location())
        word(c)
        c.cdepth -= 1
        c.log.debug("\n\t\t%s c.stack.contents = %s." % (word,c.stack.contents()))

    if c.debug:
        c.log.debug(doutput)

    c.op = op
    c.symbol = symbol
