from dataclasses import dataclass
from typing import Callable, List, Tuple, Any

#from graph import Symbol, Location, Type, TAtom, TInt, Stack, StackObject, TypeSignature
from parser import Parser, Location, Symbol

from af_types import forth_dict, Type, TAtom, op_atom, TAny

from stack import Stack

import sys

def find_atom(s: str) -> Tuple[Callable[[Stack, str], None], bool]:
    for atom in forth_dict:
        if atom[0] == s: return atom[1], True
    # Not found.
    return op_atom, False

def find_type_atom(type: Type, s: str) -> Tuple[Callable[[Stack, str], None], bool]:
    for atom in type.forth_dict:
        if atom[0] == s: return atom[1], True
    # Not found.
    return op_atom, False 

if __name__ == "__main__":

    print("ActorForth demo interpreter. ^C to exit.")
    print("Global Dictionary : %s" % forth_dict)
    for type in Type.types.keys():
        ops = Type.types.get(type)
        if len(ops):
            print("\n%s Dictionary : %s" % (type,ops))

    stack = Stack()

    handle = sys.stdin
    filename = "stdin"
    #print("len(sys.argv)==%s, sys.argv='%s'" % (len(sys.argv),sys.argv))
    if len(sys.argv) >= 2:
        #print("Is there a file: '%s'?" % filename)
    
        filename = sys.argv[1]
        handle = open(filename)

        print("Interpreting file: '%s'." % sys.argv[1])
    


    #p = Parser("samples/fundamentals01.a4")
    #p = Parser("samples/fib.a4")
    p = Parser()
    p.open_handle(handle, filename)

    def check_input_type_sig(stack: Stack, op: Callable[[Stack, str],None]) -> bool:
        """
        Eventually this needs to not only check the last items on the stack
        for a match, but also cooperate with check_output_type_sig to determine
        that the updated size of the stack also matches the expected
        outcome so there won't be false positive matches for stack pictures.
        """
        in_types = op.sig.stack_in
        if not len(in_types) : return True
        #stack_types = [s.type for s in reversed(stack.contents()[:len(in_types)]) ]
        stack_types = [s.type for s in stack.contents()[len(in_types)*-1:] ]

        print("in_types = %s" % in_types)
        print("stack_types = %s" % stack_types)
        #return in_types == stack_types
        for in_type in reversed(in_types):
            if in_type is TAny: continue
            """
            Should probably have TAny types transform to the discovered type
            so that manipulations across generics are still completely type safe.
            """
            stack_type = stack_types.pop()
            # BROKE - have to hack the check above.
            ## in_type MUST come first in this comparison
            ## in order to support generic type matching.
            if in_type != stack_type:
                print("Stack type %s doesn't match input arg type %s." % (type,in_type))
                return False
        return True

        

    def check_output_type_sig(stack: Stack, op: Callable[[Stack, str],None]) -> bool:
            return True        

    try:

        for token in p.tokens():
            symbol = Symbol(token[0], Location(p.filename,token[1],token[2]), TAtom)
            print(token[0])
            tos = stack.tos()
            #print("\nStack = %s" % stack.contents())
            if tos is not Stack.Empty:
                # We first look for an atom specialized for the type/value on TOS.
                op, found = find_type_atom(tos.type,symbol.s_id)
                if not found:
                    # If not specialized atom exists then search the global dictionary.
                    op, found = find_atom(symbol.s_id)
            else:
                # Stack's empty so search the global dictionary.
                op, found = find_atom(symbol.s_id)
            
            try:
                if check_input_type_sig(stack, op):
                    op(stack, symbol.s_id)
                    print("Stack = %s\n" % stack.contents())
                else:
                    raise Exception("Stack content doesn't match Op %s." % op.sig)
            except Exception as x:
                print("Exception %s" % x)
                print("Interpreting symbol %s" % symbol)
                print("Exception Stack = %s : " % stack.contents())
                
                # See what happens if we just keep going...
                #break

    except KeyboardInterrupt as x:
        print("\nend of line...")


    print(stack.contents())
    print("Stack max_depth = %s" % stack.max_depth())
    print("Stack depth_history = %s" % stack.depth_history())
    print("Stack total operations = %s" % stack.total_operations())
