from dataclasses import dataclass
from typing import Callable, List, Tuple, Any

from graph import Symbol, Location, Type
from parser import Parser
from stack import Stack

@dataclass
class StackObject:
    value: Any
    type: Type 

def op_int(s: Stack, s_id: str) -> None:
    print("op_int(s_id = '%s')\n" % s_id )
    i = int(s.pop().value)
    s.push(StackObject(i,Type("Int")))

def op_atom(s: Stack, s_id: str) -> None:
    print("op_atom(s_id = '%s')\n" % s_id) 
    s.push(StackObject(s_id,Type("Atom")))

def op_plus(s: Stack, s_id: str) -> None:
    print("op_plus(s_id = '%s')\n" % s_id) 
    op1 = s.pop().value
    op2 = s.pop().value
    s.push(StackObject(op1+op2,Type("Int")))

def op_print(s: Stack, s_id: str) -> None:
    print("op_print(s_id = '%s')\n" % s_id) 
    op1 = s.pop().value
    print("'%s'" % op1)

forth_dict : List[Tuple[str,Callable[[Stack, str],None]]] = []

forth_dict.insert(0,('int',op_int))
forth_dict.insert(0,('print',op_print))

Type("Int").forth_dict.insert(0,('+',op_plus))

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

for token in p.tokens():
    symbol = Symbol(token[0], Location(p.filename,token[1],token[2]), Type("Atom"))
    print(symbol)
    tos = stack.tos()
    if tos is not Stack.Empty:
        op, found = find_type_atom(tos.type,symbol.s_id)
        if not found:
            op, found = find_atom(symbol.s_id)
    else:
        op, found = find_atom(symbol.s_id)
    print("Stack = %s : " % stack.contents())
    try:
        op(stack, symbol.s_id)
    except Exception as x:
        print("Exception %s" % x)
        print("Interpreting symbol %s" % symbol)
        print("Exception Stack = %s : " % stack.contents())
        break


print(stack.contents())
print("Stack max_depth = %s" % stack.max_depth())
print("Stack depth_history = %s" % stack.depth_history())
print("Stack total operations = %s" % stack.total_operations())
