from dataclasses import dataclass

from . import *

@dataclass
class AF_List:
    ltype: Type     # Corresponds to the Type of item held in the list.
    value: list     # Our actual list content.

TList = Type("List")

def op_list(c: AF_Continuation) -> None:
    ltype = c.stack.pop().value
    assert Type(ltype).is_generic() is False, "ERROR: Generic types not supported for Lists."
    _list = AF_List(ltype,[])
    c.stack.push(StackObject(value=_list, stype=TList))
make_word_context('list', op_list, [TType], [TList])    


def op_append(c: AF_Continuation) -> None:
    obj = c.stack.pop()
    list_type = c.stack.tos().value.ltype
    assert obj.stype == list_type, "'%s' invalid type for %s List." % (obj.stype, list_type)
    c.stack.tos().value.value.append(obj.value)
make_word_context('append', op_append, [TList, TAny], [TList])


def op_print(c: AF_Continuation) -> None:
    l = c.stack.pop().value
    print(str(l.value))
    if c.prompt:
        print(c.prompt,end='',flush=True)
make_word_context('print', op_print, [TList], [])
