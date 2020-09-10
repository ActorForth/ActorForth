from dataclasses import dataclass

from . import *
from compiler import compilation_word_handler

@dataclass
class AF_UserType:
    name: str    
    values: dict

def type_definition_handler(c: AF_Continuation) -> None:
    compile_type_definition(c)

def type_attribute_handler(c: AF_Continuation) -> None:
    compile_type_attribute(c)

TTypeDefinition = Type("TypeDefinition", handler = type_definition_handler)
TTypeAttribute = Type("TypeAttribute", handler = type_attribute_handler)


def op_type(c: AF_Continuation) -> None:
    """
    Atom -> TypeDefinition(AF_UserType)
    """
    obj = AF_UserType(name=c.stack.tos().value, values = dict())
    c.stack.tos().stype = TTypeDefinition
    c.stack.tos().value = obj
make_word_context('type', op_type, [TAtom], [TTypeDefinition])     


def op_finish_type(c: AF_Continuation) -> None:
    """
    TypeDefinition(AF_UserType) -> 
    """
    s = c.stack.tos().value.name
    assert Type.types.get(s) is None, "Error: A Type called '%s' already exists." % s
    udt = Type(s)
    Type.udts[s] = c.stack.tos().value.values
    assert udt.is_udt(), "ERROR: Don't know how this isn't a UDT now: %s." % c.stack.tos()
    c.stack.pop()
make_word_context('.', op_finish_type, [TTypeDefinition], [])


def compile_type_definition(c: AF_Continuation) -> None:
    """
    TypeDefinition(AF_UserType) -> TypeDefinition(AF_UserType) TypeAttribute(SO(name))
    """
    if compilation_word_handler(c): return
    s = c.symbol.s_id
    assert Type.types.get(s) is None, "A Type called '%s' already exists." % s
    #obj = StackObject(value=s, stype=None)
    c.stack.push(StackObject(stype=TTypeAttribute, value=s))


def compile_type_attribute(c: AF_Continuation) -> None:
    """
    TypeDefinition(AF_UserType) TypeAttribute(SO(name)) -> TypeDefinition(AF_UserType)
    """
    if compilation_word_handler(c): return
    td : Optional[TypeDefinition] = Type.types.get(c.symbol.s_id)
    assert td, "'%s' is not a valid Type." % c.symbol.s_id
    obj : StackObject = c.stack.pop()
    udt : AF_UserType = c.stack.tos().value
    attrib : Optional[Type] = udt.values.get(obj.value)
    assert attrib is None, "An attribute '%s' already exists for UDT %s." % (obj.value, udt.name)
    udt.values[obj.value] = Type(c.symbol.s_id)

