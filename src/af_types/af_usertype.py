from dataclasses import dataclass

from . import *
from af_types.af_any import op_swap
from compiler import compilation_word_handler

@dataclass
class AF_UserType:
    name: str    
    values: dict

@dataclass
class UDTAttribute:
    name: str
    udta_type: Optional[Type]

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
    #print("\nop_finish_type %s" % c.stack)
    s = c.stack.tos().value.name
    assert Type.types.get(s) is None, "Error: A Type called '%s' already exists." % s
    udt = Type(s)
    Type.udts[s] = c.stack.tos().value.values
    assert udt.is_udt(), "ERROR: Don't know how this isn't a UDT now: %s." % c.stack.tos()
    c.stack.pop()
make_word_context('.', op_finish_type, [TTypeDefinition], [])


def op_finish_attribute_then_type(c: AF_Continuation) -> None:
    #print("\nop_finish_attribute_then_type %s" % c.stack)
    
    obj : StackObject = c.stack.tos()
    op_swap(c)
    udt : AF_UserType = c.stack.tos().value
    op_swap(c)

    # This is a new attribute or end of TypeDefinition. Close it up.
    c.stack.pop()
    udt.values[obj.value.name] = obj.value.udta_type

    op_finish_type(c)
make_word_context('.', op_finish_attribute_then_type, [TTypeDefinition, TTypeAttribute], [])    


def compile_type_definition(c: AF_Continuation) -> None:
    """
    TypeDefinition(AF_UserType) -> TypeDefinition(AF_UserType) TypeAttribute(SO(name))
    """
    if compilation_word_handler(c): return
    s = c.symbol.s_id
    assert Type.types.get(s) is None, "A Type called '%s' already exists." % s
    obj = UDTAttribute(name=s, udta_type=None)
    c.stack.push(StackObject(stype=TTypeAttribute, value=obj))


def compile_type_attribute(c: AF_Continuation) -> None:
    """
    TypeDefinition(AF_UserType) TypeAttribute(SO(name)) -> TypeDefinition(AF_UserType)

    OR

    TypeDefinition(AF_UserType) TypeAttribute(SO(name)) -> TypeDefinition(AF_UserType) TypeAttribute(SO(name))

    Depending on whether or not the attribute definition is complete.
    This can be due to the type being specialized like a List.
    """
    if compilation_word_handler(c): return
    td : Optional[TypeDefinition] = Type.types.get(c.symbol.s_id)

    # Is this a new attribute or one being modified?
    obj : StackObject = c.stack.tos()
    op_swap(c)
    udt : AF_UserType = c.stack.tos().value
    op_swap(c)
    if obj.value.udta_type is None:
        # This is a new attribute that has no type yet.
        print("\nBrand new attribute without a type : %s." % obj)
        assert td, "'%s' is not a valid Type." % c.symbol.s_id
        attrib : Optional[Type] = udt.values.get(obj.value.name)
        assert attrib is None, "An attribute '%s' already exists for UDT %s." % (obj.value, udt.name)
        obj.value.udta_type = Type(c.symbol.s_id)
    else:
        if td is not None:
            print("\nSpecialized type? symbol='%s' %s" % (c.symbol.s_id,c.stack))
            # This could be a specialized type such as List.        
            pass
        # else:
        #     # This is a new attribute or end of TypeDefinition. Close it up.
        #     print("\nNew attribute or end of TypeDefinition.")
        #     print("New begin: %s" % c.stack)
        #     c.stack.pop()
        #     udt.values[obj.value.name] = obj.value.udta_type
        #     print("New end: %s" % c.stack)
      

