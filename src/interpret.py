from typing import Callable

from graph import Symbol, Location 
from parser import Parser
from stack import Stack

#@dataclass
#class DictNode:
#    name: str
#    interpret: 

def op_int(s: Stack, name: str) -> None:
    print("op_int(name = '%s')\n" % name )
    i = int(s.pop())
    s.push(i)

def op_atom(s: Stack, name: str) -> None:
    print("op_atom(name = '%s')\n" % name) 
    s.push(name)

def op_plus(s: Stack, name: str) -> None:
    print("op_plus(name = '%s')\n" % name) 
    op1 = s.pop()
    op2 = s.pop()
    s.push(op1+op2)

def op_print(s: Stack, name: str) -> None:
    print("op_print(name = '%s')\n" % name) 
    op1 = s.pop()
    print("'%s'" % op1)

forth_dict = []

forth_dict.append(('int',op_int))
forth_dict.append(('+',op_plus))
forth_dict.append(('print',op_print))

def find_atom(s: str) -> Callable[[Stack, str], None]:
    for atom in forth_dict:
        
        if atom[0] == s: return atom[1]
    return op_atom

print("forth_dict = %s" % forth_dict)

stack = Stack()

p = Parser("samples/fundamentals01.a4")

for token in p.tokens():
    symbol = Symbol(token[0], Location(p.filename,token[1],token[2]))
    op = find_atom(symbol.name)
    print("Stack = %s : " % stack.contents())
    op(stack, symbol.name)

print(stack.contents())
print("Stack max_depth = %s" % stack.max_depth())
print("Stack depth_history = %s" % stack.depth_history())
print("Stack total operations = %s" % stack.total_operations())
