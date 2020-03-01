from typing import TextIO

from dataclasses import dataclass

from parser import Parser

from af_types import Type, TypeSignature, TAtom, make_atom, TAny 

from af_types.af_int import *
from af_types.af_bool import *

from compiler import *


@dataclass(frozen = True)
class Location:
    filename : str = "Unknown"
    linenum : int = 0
    column : int = 0

@dataclass(order = True)
class Symbol:
    s_id : str
    location : Location 
    type : Type 
    
    @property
    def size(self) -> int:
        return len(self.s_id)

    def __eq__(self, symbol = None) -> bool:
        if type(symbol) is Symbol:
            return symbol.s_id == self.s_id
        return symbol == self.s_id   

def interpret(stack: Stack, input_stream: TextIO, filename: Optional[str] = None, prompt: Optional[str] = None) -> Stack:    
    p = Parser()
    p.open_handle(input_stream, filename)

    interpret_mode = True

    if prompt: print(prompt,end='',flush=True)    
    for s_id, linenum, column in p.tokens():
        symbol = Symbol(s_id, Location(p.filename,linenum,column), TAtom)

        if p.filename != "stdin":
            print(s_id)
        tos = stack.tos()
        found = False
        #print("\nStack = %s" % stack.contents())
        if tos is not Stack.Empty:
            # We first look for an atom specialized for the type/value on TOS.
            #print("HACK tos = %s" % str(tos))
            op, sig, flags, found = Type.op(symbol.s_id, stack, tos.type.name)

        if not found:
            # If Stack is empty or no specialized atom exists then search the global dictionary.
            op, sig, flags, found = Type.op(symbol.s_id, stack)
        
        try:
            if found:
                op(stack, symbol.s_id)
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
