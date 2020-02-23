from typing import TextIO

from parser import Parser, Location, Symbol

from af_types import Operation, Type, TypeSignature, TAtom, make_atom, TAny 

from af_types.af_int import *
from af_types.af_bool import *

def interpret(stack: Stack, input_stream: TextIO, filename: Optional[str] = None, prompt: Optional[str] = None) -> Stack:    
    p = Parser()
    p.open_handle(input_stream, filename)

    interpret_mode = True

    if prompt: print(prompt,end='',flush=True)    
    for token in p.tokens():
        symbol = Symbol(token[0], Location(p.filename,token[1],token[2]), TAtom)
        if p.filename != "stdin":
            print(token[0])
        tos = stack.tos()
        found = False
        #print("\nStack = %s" % stack.contents())
        if tos is not Stack.Empty:
            # We first look for an atom specialized for the type/value on TOS.
            op, sig, flags, found = Type.op(symbol.s_id, stack, tos.type.name)

        if not found:
            # If Stack is empty or no specialized atom exists then search the global dictionary.
            op, sig, flags, found = Type.op(symbol.s_id, stack)
        
        try:
            if found:
                op(stack)
                print("Stack(%s) = %s " % (len(stack.contents()),stack.contents()))
                #     else:
                # if interpret_mode or flags.immediate:
                #     if sig.match_in(stack): # match stack types with type signature.
                #         op(stack)
                #         print("Stack(%s) = %s " % (len(stack.contents()),stack.contents()))
                #     else:
                #         raise Exception("Stack content doesn't match Op %s." % sig.stack_in)
                # else: # Compile mode!
                #     pass
            else:
                # No idea what this is so make an atom on the stack.
                make_atom(stack, symbol.s_id)
                print("Stack(%s) = %s " % (len(stack.contents()),stack.contents()))
        except Exception as x:
            print("Exception %s" % x)
            print("Interpreting symbol %s" % symbol)
            print("Stack(%s) = %s " % (len(stack.contents()),stack.contents()))
            
            # See what happens if we just keep going...
            #break
            raise
        if prompt: print(prompt,end='',flush=True)    

    return stack
