import sys
from dataclasses import dataclass
from typing import Callable, List, Tuple, Any, TextIO

from parser import Parser, Location, Symbol

from af_types import Operation, Type, TypeSignature, TAtom, make_atom, TAny 

from af_types.af_int import *
from af_types.af_bool import *

def interpret(stack: Stack, input_stream: TextIO, filename: Optional[str] = None) -> Stack:    
    p = Parser()
    p.open_handle(input_stream, filename)

    interpret_mode = True


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


    return stack


if __name__ == "__main__":

    print("ActorForth demo interpreter. ^C to exit.")
    print("Global Dictionary : %s" % [op[0] for op in Type.types["Any"]])
    for type in Type.types.keys():
        if type != "Any":
            ops = Type.types.get(type,[])
            if len(ops):
                print("%s Dictionary : %s" % (type,[op[0] for op in ops]))

    handle = sys.stdin
    filename = "stdin"
    if len(sys.argv) >= 2:
  
        filename = sys.argv[1]
        handle = open(filename)

        print("Interpreting file: '%s'." % sys.argv[1])


    stack = Stack()


    try:

        stack = interpret(stack, handle, filename)


    except KeyboardInterrupt as x:
        print(" key interrupt.")

    print(stack.contents())
    print("Stack max_depth = %s" % stack.max_depth())
    print("Stack depth_history = %s" % stack.depth_history())
    print("Stack total operations = %s" % stack.total_operations())
    print("\nend of line...")
