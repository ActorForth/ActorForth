from dataclasses import dataclass
from typing import Callable, List, Tuple, Any

from graph import Symbol, Location, Type, TAtom, TInt, Stack, StackObject, TypeSignature
from parser import Parser


def op_int(s: Stack, s_id: str) -> None:
    print("op_int(s_id = '%s')\n" % s_id )
    i = int(s.pop().value)
    s.push(StackObject(i,TInt))
op_int.sig=TypeSignature([TAtom],[TInt])

def op_atom(s: Stack, s_id: str) -> None:
    print("op_atom(s_id = '%s')\n" % s_id) 
    s.push(StackObject(s_id,TAtom))
op_atom.sig=TypeSignature([],[TAtom])

def op_plus(s: Stack, s_id: str) -> None:
    print("op_plus(s_id = '%s')\n" % s_id) 
    op1 = s.pop().value
    op2 = s.pop().value
    s.push(StackObject(op1+op2,TInt))
op_plus.sig=TypeSignature([TInt,TInt],[TInt])    

def op_print(s: Stack, s_id: str) -> None:
    print("op_print(s_id = '%s')\n" % s_id) 
    op1 = s.pop().value
    print("'%s'" % op1)
op_print.sig=TypeSignature([TInt],[])


forth_dict : List[Tuple[str,Callable[[Stack, str],None]]] = []

forth_dict.insert(0,('int',op_int))
forth_dict.insert(0,('print',op_print))

TInt.forth_dict.insert(0,('+',op_plus))

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

print("forth_dict = %s" % forth_dict)

stack = Stack()

p = Parser("samples/fundamentals01.a4")
#p = Parser("samples/fib.a4")

def check_input_type_sig(stack: Stack, op: Callable[[Stack, str],None]) -> bool:
    in_types = op.sig.stack_in
    if not len(in_types) : return True
    #stack_types = [s.type for s in reversed(stack.contents()[:len(in_types)]) ]
    stack_types = [s.type for s in stack.contents()[len(in_types)*-1:] ]
    print("in_types = %s" % in_types)
    print("stack_types = %s" % stack_types)
    return in_types == stack_types
        

def check_output_type_sig(stack: Stack, op: Callable[[Stack, str],None]) -> bool:
        return True        

for token in p.tokens():
    symbol = Symbol(token[0], Location(p.filename,token[1],token[2]), TAtom)
    print(symbol)
    tos = stack.tos()
    print("\nStack = %s" % stack.contents())
    if tos is not Stack.Empty:
        op, found = find_type_atom(tos.type,symbol.s_id)
        if not found:
            op, found = find_atom(symbol.s_id)
    else:
        op, found = find_atom(symbol.s_id)
    
    try:
        if check_input_type_sig(stack, op):
            op(stack, symbol.s_id)
            print("Stack = %s\n" % stack.contents())
        else:
            raise Exception("Stack content doesn't match Op TypeSignature.")
    except Exception as x:
        print("Exception %s" % x)
        print("Interpreting symbol %s" % symbol)
        print("Exception Stack = %s : " % stack.contents())
        break


print(stack.contents())
print("Stack max_depth = %s" % stack.max_depth())
print("Stack depth_history = %s" % stack.depth_history())
print("Stack total operations = %s" % stack.total_operations())
