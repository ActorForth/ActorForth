from typing import Callable, List, Tuple

from graph import Symbol, Location, Type
from parser import Parser
from stack import Stack

#@dataclass
#class DictNode:
#    s_id: str
#    interpret: 

def op_int(s: Stack, s_id: str) -> None:
    print("op_int(s_id = '%s')\n" % s_id )
    i = int(s.pop())
    s.push(i)

def op_atom(s: Stack, s_id: str) -> None:
    print("op_atom(s_id = '%s')\n" % s_id) 
    s.push(s_id)

def op_plus(s: Stack, s_id: str) -> None:
    print("op_plus(s_id = '%s')\n" % s_id) 
    op1 = s.pop()
    op2 = s.pop()
    s.push(op1+op2)

def op_print(s: Stack, s_id: str) -> None:
    print("op_print(s_id = '%s')\n" % s_id) 
    op1 = s.pop()
    print("'%s'" % op1)

forth_dict : List[Tuple[str,Callable[[Stack, str],None]]] = []

forth_dict.insert(0,('int',op_int))
forth_dict.insert(0,('+',op_plus))
forth_dict.insert(0,('print',op_print))

def find_atom(s: str) -> Callable[[Stack, str], None]:
    for atom in forth_dict:
        if atom[0] == s: return atom[1]
    # Not found.
    return op_atom

print("forth_dict = %s" % forth_dict)

stack = Stack()

p = Parser("samples/fundamentals01.a4")

for token in p.tokens():
    symbol = Symbol(token[0], Location(p.filename,token[1],token[2]), Type("Unknown"))
    print(symbol)
    op = find_atom(symbol.s_id)
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
