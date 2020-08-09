#
#   compiler.py     - Building new words/types for our language.
#

import logging
from typing import Dict, List, Tuple, Callable, Any, Optional, Sequence
from dataclasses import dataclass
from itertools import zip_longest

from af_types import *
from af_types.af_any import op_swap, op_stack
from operation import Operation_def, TypeSignature

def make_word_context(word_name: Op_name, op_def: Operation_def, in_seq: Sequence["Type"], out_seq: Sequence["Type"]) -> None:
    sig = TypeSignature(in_seq = [StackObject(stype=x) for x in in_seq], out_seq = [StackObject(stype=x) for x in out_seq])
    Type.add_op(Operation(word_name, op_def, sig=sig), type_name=in_seq[-1].name)

def input_type_handler(c: AF_Continuation) -> None:
    type_sig_handler(c, "InputTypeSignature")

def output_type_handler(c: AF_Continuation) -> None:
    type_sig_handler(c, "OutputTypeSignature")

def code_compile_handler(c: AF_Continuation ) -> None:
    return compile_word_handler(c)

def pattern_handler(c: AF_Continuation) -> None:
    return compile_pattern_handler(c)


TWordDefinition = Type("WordDefinition")
TInputTypeSignature = Type("InputTypeSignature", handler = input_type_handler)
TOutputTypeSignature = Type("OutputTypeSignature", handler = output_type_handler)
TCodeCompile = Type("CodeCompile", handler = code_compile_handler)
TInputPatternMatch = Type("InputPatternMatch", handler = pattern_handler)
TOutputPatternMatch = Type("OutputPatternMatch", handler = pattern_handler)


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
make_word_context(':', op_new_word, [TAtom],[TWordDefinition, TInputTypeSignature])


def op_switch_to_output_sig(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), InputTypeSignature(TypeSignature)
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature).

    During a TypeSignature declaration, -> switches the compiler from building the
    input types to building the output types.
    """
    c.stack.tos().stype = TOutputTypeSignature
make_word_context('->',op_switch_to_output_sig,[TWordDefinition,TInputTypeSignature],
                        [TWordDefinition,TOutputTypeSignature])


def op_start_code_compile(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature)
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation).

    Signifies the completion of the TypeSignature for the new word.
    Switches to start the definition of the word's behavior.

    Constructs a new Operation declaration from STUFF
    """
    # Grab the name of the new word from the WordDefinition
    sig = c.stack.tos().value
    op_swap(c)
    op = Operation(c.stack.tos().value, op_execute_compiled_word, sig=sig)
    op_swap(c)
    c.stack.push( StackObject(value=op, stype=TCodeCompile) )
make_word_context(';',op_start_code_compile, [TWordDefinition,TOutputTypeSignature], 
                    [TWordDefinition, TOutputTypeSignature, TCodeCompile])


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
make_word_context(';',op_skip_to_code_compile, [TWordDefinition], [TWordDefinition, TOutputTypeSignature, TCodeCompile])


def op_switch_to_pattern_matching(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature)
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), InputPatternMatch(TypeSignature).

    Start specializing for an implementation of the new word that matches
    the TypeSignature from the OutputTypeSignature.
    """    
    sig = TypeSignature([],[])
    c.stack.push(StackObject(value=sig, stype=TInputPatternMatch))
make_word_context(':',op_switch_to_pattern_matching, [TWordDefinition, TOutputTypeSignature],
                    [TWordDefinition, TOutputTypeSignature, TInputPatternMatch])                


def op_switch_to_pattern_compilation(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), OutputPatternMatch(TypeSignature)
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), OutputPatternMatch(TypeSignature), CodeCompile(Operation).

    Signifies the completion of the TypeSignature for the new word.
    Switches to start the definition of the word's behavior.

    Constructs a new Operation declaration from STUFF
    """
    c.log.debug("op_switch_to_pattern_compilation started.")
    out_pattern : StackObject = c.stack.pop()

    # Confirm our OutputTypePatternMatch output pattern matches the OutputTypeSignature's output sig.
    if out_pattern.value.stack_out.depth() != c.stack.tos().value.stack_out.depth():
        msg = "Pattern %s doesn't complete output type signature: %s." % (out_pattern.value.stack_out, c.stack.tos().value.stack_out)
        c.log.error(msg)
        raise Exception(msg)

    op_swap(c)
    op_name = c.stack.tos().value
    op_swap(c)
    c.stack.push(out_pattern)

    op = Operation(c.stack.tos().value, op_execute_compiled_word, sig=out_pattern.value)
    c.stack.push( StackObject(value=op, stype=TCodeCompile) )
make_word_context(';',op_switch_to_pattern_compilation, [TWordDefinition, TOutputTypeSignature, TOutputPatternMatch],
                    [TWordDefinition, TOutputTypeSignature, TOutputPatternMatch, TCodeCompile])                


# def op_switch_to_pattern_compilation(c: AF_Continuation) -> None:
#     """
#     CodeCompile(Operation') 
#         -> CodeCompile(PatternOperation'), MatchPattern([ (Sequence[StackObject],Operation)] )

#     Changes the Operation handler to be one that executes a pattern matching algorithm
#     rather than just executes a set of words. Then sets up the compilation to begin
#     capturing these patterns and associated operations.    
#     """
#     # For now, we're not allowing adding pattern matching if words have
#     # already been compiled.
#     if len(c.stack.tos().words): 
#         error_msg = "UNSUPPORTED : can't switch to pattern matching for word, '%s', which has already compiled these words: %s." \
#                         % (c.stack.tos().name, c.stack.tos().words)
#         c.log.error(error_msg)
#         raise Exception(error_msg)

#     # This 'patterns' instance gets bound to the new Op that will be in our CodeCompile object.
#     # New patterns get added to the Op via our MatchPattern compilation.
#     patterns : List[ Tuple[Sequence["StackObject"], Optional[Operation]] ] = [([],None)]

#     # Over-ride the Operation handler to be a pattern matching Operation.
#     c.stack.tos().the_op = match_and_execute_compiled_word(c, patterns)

#     MatchPattern = StackObject(value = patterns, stype=TMatchPattern)
#     c.stack.push(MatchPattern)
#     c.log.debug("'%s' is now a pattern matched word." % c.stack.tos().name)
# Type.add_op(Operation( ':',op_switch_to_pattern_compilation,
#             sig=TypeSignature([StackObject(stype=TCodeCompile)],
#                 [StackObject(stype=TCodeCompile), StackObject(stype=TMatchPattern)]) ),
#                 "CodeCompile")


def op_finish_word_compilation(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')
        -> WordDefinition

    TODO: MUST have this confirm Operation's TypeSignature matches
          the behavior of this Operation before storing it as a new word.
    """
    c.log.debug("finishing word compilation!")
    op : Operation = c.stack.pop().value
    s_in : Stack = op.sig.stack_in

    if s_in.is_empty() :
        c.log.debug("'%s' operation being added to global dictionary." % op.name)
        Type.add_op(op)
    else:
        s_in_tos : StackObject = s_in.tos()
        c.log.debug("'%s' operation being added to '%s' dictionary." % (op.name, s_in_tos.stype))
        Type.add_op(op, s_in_tos.stype.name)
    c.stack.pop()

make_word_context(';',op_finish_word_compilation, [TWordDefinition, TOutputTypeSignature, TCodeCompile],
                    [TWordDefinition])                 


def op_finish_word_definition(c: AF_Continuation) -> None:
    """
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation')
        -> (empty)
    """
    op_finish_word_compilation(c)
    c.stack.pop()
make_word_context('.',op_finish_word_definition, [TWordDefinition, TOutputTypeSignature, TCodeCompile], [])            


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
    assert c.symbol

    # Is this word actually a type?
    c.log.debug("Looking up a type called '%s'." % c.symbol.s_id)
    _type = Type.get_type(c.symbol.s_id)
    assert _type, "%s isn't an existing type : %s" % (_type, Type.types.keys())
    if type_name == "InputTypeSignature":
        c.stack.tos().value.stack_in.push(StackObject(stype=_type))
    else:
        c.stack.tos().value.stack_out.push(StackObject(stype=_type))


def compile_word_handler(c: AF_Continuation) -> None:
    """
    Expects stack signature of:
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), CodeCompile(Operation)
    
    OR

    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), OutputPatternMatch(TypeSignature), CodeCompile(Operation')

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

    op : Operation = c.stack.tos().value
    maybe_recursive_op : Optional[Operation] = None
    if op_name == op.name:  # This is potentially a recursive call.
        maybe_recursive_op = op

    tos_output_sig, is_matched = op.check_stack_effect(force_composite = True)

    context_type_name : Type_name = "Any"
    if len(tos_output_sig):
        context_type_name = tos_output_sig.tos().stype.name

    ##
    ## TODO : Now we have to discover potentially multiple viable Operations
    ##        that this word could invoke and, if more than one is found,
    ##        have the execution perform run-time pattern matching.
    ##

    all_named_words = [w for w in Type.find_named_ops_for_scope(op_name, context_type_name, maybe_recursive_op)]
    if context_type_name != "Any":
        all_named_words += [w for w in Type.find_named_ops_for_scope(op_name, "Any")]
    c.log.debug("All candidate words: %s." % all_named_words)

    def match_type_context(candidate: Operation, context: Stack) -> bool:
        # Walk backward down the stack.
        for test, pattern in zip_longest(candidate.sig.stack_in.contents()[::-1], context.contents()[::-1]):
            if test is None: return True # Nothing left to compare.
            if pattern is None: return False # Context Stack underflow!            
            if test.stype != pattern.stype: return False # Types didn't match.
            if test.value is not None and pattern.value is not None \
                and test.value != pattern.value: return False # Values didn't match.            
        return True        

    type_matched_words = [w for w in all_named_words if match_type_context(w, tos_output_sig)]
    c.log.debug("All type sig matching words: %s." % type_matched_words)

    def match_some_value_context(candidate: Operation, context: Stack) -> int:        
        count = 0
        for count, (test, pattern) in enumerate(zip_longest(candidate.sig.stack_in.contents()[::-1], context.contents()[::-1])):            
            if test is None and count == 0: return True
            if test is None: return False
            if test.value is not None and pattern.value is not None:
                if test.value == pattern.value: return True
        return count == 0
    value_some_matched_words = [w for w in type_matched_words if match_some_value_context(w, tos_output_sig)]   
    c.log.debug("The some value matching words: %s." % value_some_matched_words)           

    def match_all_value_context(candidate: Operation, context: Stack) -> int:        
        for count, (test, pattern) in enumerate(zip_longest(candidate.sig.stack_in.contents()[::-1], context.contents()[::-1])):            
            if test is None and count == 0: return True
            if test is None: return False
            if test.value is None: return False
            if pattern.value is None: return False  
        return True      
    value_exact_matched_words = [w for w in value_some_matched_words if match_all_value_context(w, tos_output_sig)]
    c.log.debug("The exact value matching words: %s." % value_exact_matched_words)

    #[type_matched_words.remove(word) for word in value_some_matched_words]
    #[value_some_matched_words.remove(word) for word in value_exact_matched_words]

    if value_exact_matched_words:
        assert len(value_exact_matched_words) == 1, "ERROR : more than one exact match! Not possible!"
        c.stack.tos().value.add_word(value_exact_matched_words[0])
        c.log.debug("Compiled exact match for '%s' => %s." % (op_name, value_exact_matched_words[0]))
        return

    # if value_some_matched_words or type_matched_words:
    #     value_some_matched_words.sort()
    #     type_matched_words.sort()
    #     candidate_words = value_some_matched_words + type_matched_words
    if type_matched_words:
        if len(type_matched_words)==1:
            c.stack.tos().value.add_word(type_matched_words[0])
            c.log.debug("Compiled exact match for '%s' => %s." % (op_name, type_matched_words[0]))
            return

        # Need to do some runtime pattern matching...
        matching_op = match_and_execute_compiled_word(type_matched_words)        

        # WHAT'S OUR TYPESIGNATURE?!?!?!?!?

        c.stack.tos().value.add_word(Operation(op_name, matching_op))



        c.log.debug("Compiled pattern matching op for '%s' => %s." % (op_name, type_matched_words))
        return

    c.log.debug("FAILED TO FIND WORD TO COMPILE '%s'" % c.symbol.s_id )

    c.log.debug("Compile as literal")
        
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


# def stack_from_patterns(c: AF_Continuation) -> Stack:
#     assert c.stack.tos().stype == "InputPatternMatch"
#     result: Stack = Stack()
#     patterns : List[ Tuple[Sequence["StackObject"], Optional[Operation]] ] = c.stack.tos().value
        
#     # Grab the last item of the list.
#     pat : Sequence["StackObject"]
#     pat, op = patterns[-1]
#     assert op is not None, "This can't happen!"
#     [result.push(sig) for sig in pat]

#     return result


def compile_pattern_handler(c: AF_Continuation) -> None:
    """
    Stack pattern looks like this:
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), InputPatternMatch(TypeSignature).

    OR

    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), OutputPatternMatch(TypeSignature).


    Allows for entry of Types and Typed Values which match the pattern of the OutputTypeSignature.
    """
    c.log.debug("compile_pattern_handler starting")
    assert c.symbol
    if compilation_word_handler(c): return

    # Pull the TypeSignature from the OutputTypeSignature
    # down one position in the stack and then bring InputPatternMatch back up top.
    op_swap(c)
    op_sig = c.stack.tos().value
    op_swap(c)

    sig_declaration : Stack
    current_sig : Stack

    if c.stack.tos().stype == TInputPatternMatch:
        sig_declaration = op_sig.stack_in
        current_sig = c.stack.tos().value.stack_in
    else:
        sig_declaration = op_sig.stack_out
        current_sig = c.stack.tos().value.stack_out

    c.log.debug("'%s' match requested for sig_declaration: %s already having matched: %s." % (c.symbol, sig_declaration, current_sig))
        
    remaining_matches : int = sig_declaration.depth()-current_sig.depth()
    if remaining_matches < 1 : raise Exception("Too many entries now.")
    next_type : Type = sig_declaration.contents(remaining_matches)[0].stype

    # First we see if this symbol refers to a Type match.
    _type : Optional[Type] = Type.get_type(c.symbol.s_id)
    if _type:
        if next_type != _type:
            error_msg = "'%s' being compiled does not match '%s' in the pattern." % (_type, next_type)
            c.log.error(error_msg)
            raise Exception(error_msg)

        # Append this type to the current pattern.
        current_sig.push( StackObject(stype=_type) )


    # If not a Type match then is there a ctor for this value as an atom for the Type?
    else:
        ctor : Optional[ Callable[["AF_Continuation"],None] ] = Type.find_ctor(next_type.name, [StackObject(stype=TAtom)])
        if ctor is None:
            error_msg = "'%s' is neither a Type %s nor is there a ctor to build that from this Atom." % (c.symbol, next_type.name)
            c.log.error(error_msg)
            raise Exception(error_msg)

        # Construct the Type and append to the current pattern.
        c.stack.push(StackObject(value=c.symbol.s_id,stype=TAtom))
        ctor(c) # This will throw an exception if it fails.
        s = c.stack.pop()
        current_sig.push( s )


def op_switch_to_output_pattern_sig(c: AF_Continuation) -> None:
    """
    Stack pattern looks like this:
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), InputPatternMatch(TypeSignature)
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), OutputPatternMatch(TypeSignature).
    """
    # Pull the TypeSignature input sequence from the InputTypeSignature
    # down one position in the stack and then bring InputPatternMatch back up top.
    c.log.debug("op_switch_to_output_pattern_sig starting")
    op_swap(c)
    input_sig : Stack = c.stack.tos().value.stack_in
    op_swap(c)
    matched_sig : Stack = c.stack.tos().value.stack_in

    if input_sig.depth() != matched_sig.depth():
        msg = "Input Signature : %s doesn't match pattern : %s!" % (input_sig, matched_sig)
        c.log.error(msg)
        raise Exception(msg)
    c.stack.tos().stype = TOutputPatternMatch
make_word_context('->', op_switch_to_output_pattern_sig, [TWordDefinition, TOutputTypeSignature, TInputPatternMatch],
                        [TWordDefinition, TOutputTypeSignature, TOutputPatternMatch])        


def compile_matched_pattern_to_word(c: AF_Continuation) -> None:
    """
    Stack pattern looks like this:
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), OutputPatternMatch(TypeSignature)
        -> WordDefinition(Op_name), OutputTypeSignature(TypeSignature), InputPatternMatch(TypeSignature).

    Takes the current OutputPatternMatch object and tries to turn it
    into a new Operation word for our dictionary.        
    """
    c.log.debug("compile_matched_pattern_to_word starting")
    op_swap(c)
    output_type_sig : TypeSignature = c.stack.tos().value
    op_swap(c)

    pattern_type_sig : TypeSignature = c.stack.tos().value
    pattern_output : Stack = pattern_type_sig.stack_out

    # First confirm that the OutputPattern sig.stack_out conforms to our OutputTypeSignature.
    # Note : compile_pattern_handler gurantees no invalid patterns exist for both input
    #        and output sigs, we just need to make sure the full output definition has 
    #        been fulfilled here so checking length is adequate.
    assert output_type_sig.stack_out.depth() == pattern_output.depth(), \
        "Error - Output TypeSignature for Operation: %s doesn't match Pattern Output: %s." % (output_type_sig.stack_out, pattern_output)

    # Now make sure there's a real value for each part of the pattern output.
    assert all([x.value is not None for x in pattern_output.contents()]), \
        "Error - None value found in the pattern output: %s, partial words not supported here." % pattern_output

    # OK we have a valid simple Operation! Can we simplify it?
    op : Operation_def
    in_sig : Stack = pattern_type_sig.stack_in.copy()
    out_sig : Stack = pattern_type_sig.stack_out.copy()

    first_position : Optional[int] = None
    output_vals : List["StackObject"] = []
    for pos, (a,b) in enumerate(zip_longest(in_sig.contents(), out_sig.contents())):
        if a.value != b.value and first_position is None: 
            first_position = pos
        if first_position is not None and b.value is not None:
            output_vals.append(b)

    if first_position is None:
        # Input & Output stacks match. This Operation is a no-op.
        op = op_nop 
    else:        
        # Create an op that consumes an appropriate number of items from the stack 
        # and then pushes our content delta onto the stack.
        pops = in_sig.depth() - first_position
        def op_curry_pop_and_push(pop_count: int, push_content: Sequence["StackObject"]) -> Operation_def:
            def pop_and_push(c: AF_Continuation) -> None:
                [c.stack.pop() for x in range(pop_count)]
                [c.stack.push(x) for x in push_content]
            return pop_and_push

        op = op_curry_pop_and_push(pops, output_vals)

    # Create our new word! (Will grab the name later.)
    new_op = Operation("", op, sig = pattern_type_sig)

    # Pop off the OutputPatternMatch
    c.stack.pop()

    # Get the word name from our WordDefinition
    op_swap(c)
    new_op.name = c.stack.tos().value
    op_swap(c)

    # Now add to the type's disctionary or the global one as appropriate.
    if pattern_type_sig.stack_in.depth() > 0:
        Type.add_op(new_op, pattern_type_sig.stack_in.tos().stype.name)
    else:
        Type.add_op(new_op)        

    # Create a new InputPatternMatch
    op_switch_to_pattern_matching(c)
make_word_context(':', compile_matched_pattern_to_word, [TWordDefinition, TOutputTypeSignature, TOutputPatternMatch],
                        [TWordDefinition, TOutputTypeSignature, TInputPatternMatch])        


def compile_and_complete_pattern_to_word(c: AF_Continuation) -> None:
    """
    Stack pattern looks like this:
    WordDefinition(Op_name), OutputTypeSignature(TypeSignature), OutputPatternMatch(TypeSignature)
        -> (empty).

    Takes the current OutputPatternMatch object and tries to turn it
    into a new Operation word for our dictionary. Then clears the entire WordDefinition.
    """
    c.log.debug("compile_and_complete_pattern_to_word starting")
    compile_matched_pattern_to_word(c)
    c.stack.pop()
    c.stack.pop()
    c.stack.pop()
make_word_context('.', compile_and_complete_pattern_to_word, [TWordDefinition, TOutputTypeSignature, TOutputPatternMatch], [])            


def match_and_execute_compiled_word(words: List[Operation]) -> Callable[["AF_Continuation"],None]:
    def op_curry_match_and_execute(c: AF_Continuation) -> None:
        c.log.debug("Attempting to pattern match with words = %s and this stack: %s." % (words,c.stack))
        word_sig : Sequence["StackObject"]
        #op : Optional[Operation]
        for word in words:
            matches : bool = True
            # Copy as many items off the stack as our pattern to match against.
            stack_frame = c.stack.contents(word.sig.stack_in.depth())
            word_sig = word.sig.stack_in.contents()
            c.log.debug("Matching stack: %s against word sig: %s." % (stack_frame, word_sig))
            pat : StackObject
            test : StackObject
            for w,s in zip(word_sig,stack_frame):
                # If our pattern has a value then the test value must match it.
                if w.value is not None:
                    if w.value != s.value: 
                        c.log.debug("Value mismatch: %s != %s." % (w.value, s.value))
                        matches = False
                        break
                if w.stype != s.stype: 
                        c.log.debug("Type mismatch: %s != %s." % (w.stype, s.stype))
                        matches = False
                        break
            # Everything matches - this is our op. Call it.
            if matches: 
                c.log.debug("Matched! Call the operator.")                
                return word(c)
        # If we got here then nothing matched!
        c.log.error("No matches found!")
        raise Exception("No matches found!")
    return op_curry_match_and_execute


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
