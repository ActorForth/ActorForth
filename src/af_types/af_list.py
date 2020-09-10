from dataclasses import dataclass

from . import *

@dataclass
class AF_List:
    ltype: Type
    value: list

TList = Type("List")

def op_list(c: AF_Continuation) -> None:
    ltype = c.stack.pop().value
    _list = AF_List(ltype,[])
    c.stack.push(StackObject(value=_list, stype=TList))
make_word_context('list', op_list, [TType], [TList])    

def op_append(c: AF_Continuation) -> None:
    pass