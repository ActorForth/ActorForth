from typing import TextIO

from dataclasses import dataclass

from continuation import Continuation, Stack, Symbol, Location

from parser import Parser

from af_types import Type, TypeSignature, TAtom, make_atom, TAny 

from af_types.af_int import *
from af_types.af_bool import *
from af_types.af_debug import *

from compiler import *
import compiler

def interpret(cont: Continuation, input_stream: TextIO, filename: Optional[str] = None, prompt: Optional[str] = None) -> Continuation:    
    p = Parser()
    p.open_handle(input_stream, filename)

    interpret_mode = True

    if prompt: print(prompt,end='',flush=True)    
    for s_id, linenum, column in p.tokens():
        cont.symbol = Symbol(s_id, Location(p.filename,linenum,column) ) #, TAtom)

        if p.filename != "stdin":
            print(s_id)

        # With new Type.find_op behaviour we can remove found if we want.
        cont.op, sig, flags, found = Type.op(cont.symbol.s_id, cont)

        try:
            cont.op(cont)
            print(cont)

            # if found:
            #     #print("Executing %s:" % cont.op)
            #     cont.op(cont)
            #     print(cont)
            # else:
            #     # No idea what this is so make an atom on the stack.
            #     #print("New Atom: '%s'" % symbol.s_id)
            #     raise Exception("IMPOSSIBLE ERROR IN INTERPRET!!")
            #     make_atom(cont, symbol.s_id)
            #     print(cont)
                
        except Exception as x:
            print("Exception %s" % x)
            print("Interpreting symbol %s" % cont.symbol)
            print(cont)
            
            # See what happens if we just keep going...
            #break
            raise
        if prompt: print(prompt,end='',flush=True)    

    return cont
