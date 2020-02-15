import sys
from dataclasses import dataclass
from typing import Callable, List, Tuple, Any

from parser import Parser, Location, Symbol

from af_types import Operation, Type, TypeSignature, TAtom, make_atom, TAny 

from af_types.af_int import *

if __name__ == "__main__":


    def check_input_type_sig(stack: Stack, op_name: Op_name) -> bool:
        """
        Eventually this needs to not only check the last items on the stack
        for a match, but also cooperate with check_output_type_sig to determine
        that the updated size of the stack also matches the expected
        outcome so there won't be false positive matches for stack pictures.
        """
        in_types = Type.op(op_name) # sig.stack_in
        if not len(in_types) : return True
        stack_types = [s.type for s in stack.contents()[len(in_types)*-1:] ]

        print("in_types = %s" % (in_types))
        print("stack_types = %s" % stack_types)
        for in_type in reversed(in_types):
            if in_type is TAny: continue
            """
            Should probably have TAny types transform to the discovered type
            so that manipulations across generics are still completely type safe.
            """
            stack_type = stack_types.pop()
            if in_type != stack_type:
                print("Stack type %s doesn't match input arg type %s." % (type,in_type))
                return False
        return True


    print("ActorForth demo interpreter. ^C to exit.")
    print("Global Dictionary : %s" % [op[0] for op in Type.types["Any"]])
    for type in Type.types.keys():
        if type != "Any":
            ops = Type.types.get(type,[])
            if len(ops):
                print("%s Dictionary : %s" % (type,[op[0] for op in ops]))

    stack = Stack()

    handle = sys.stdin
    filename = "stdin"
    if len(sys.argv) >= 2:
  
        filename = sys.argv[1]
        handle = open(filename)

        print("Interpreting file: '%s'." % sys.argv[1])
    
    p = Parser()
    p.open_handle(handle, filename)

    try:

        for token in p.tokens():
            symbol = Symbol(token[0], Location(p.filename,token[1],token[2]), TAtom)
            print(token[0])
            tos = stack.tos()
            found = False
            #print("\nStack = %s" % stack.contents())
            if tos is not Stack.Empty:
                # We first look for an atom specialized for the type/value on TOS.
                op, sig, found = Type.op(symbol.s_id,tos.type.name)

            if not found:
                # If Stack is empty or no specialized atom exists then search the global dictionary.
                op, sig, found = Type.op(symbol.s_id)
            
            try:
                if True: #### HERE!! check_input_type_sig(stack, op):
                    if found:
                        op(stack)
                    else:
                        # No idea what this is so make an atom on the stack.
                        make_atom(stack, symbol.s_id)
                    print("Stack = %s\n" % stack.contents())
                else:
                    raise Exception("Stack content doesn't match Op %s." % op.sig)
            except Exception as x:
                print("Exception %s" % x)
                print("Interpreting symbol %s" % symbol)
                print("Stack = %s : " % stack.contents())
                
                # See what happens if we just keep going...
                #break
                #raise

    except KeyboardInterrupt as x:
        print("\nend of line...")


    print(stack.contents())
    print("Stack max_depth = %s" % stack.max_depth())
    print("Stack depth_history = %s" % stack.depth_history())
    print("Stack total operations = %s" % stack.total_operations())
