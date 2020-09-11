from dataclasses import dataclass

from . import *
from af_types.af_debug import *
from af_types.af_any import op_swap
from compiler import compilation_word_handler
from continuation import Continuation

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


def compile_finish_attribute(c: AF_Continuation) -> None:
    obj : StackObject = c.stack.tos()
    op_swap(c)
    udt : AF_UserType = c.stack.tos().value
    op_swap(c)

    # This is a new attribute or end of TypeDefinition. Close it up.
    c.stack.pop()
    udt.values[obj.value.name] = obj.value.udta_type    


def op_finish_attribute_then_type(c: AF_Continuation) -> None:
    #print("\nop_finish_attribute_then_type %s" % c.stack)
    compile_finish_attribute(c)
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
    c.log.debug("Evaluating symbol '%s' for TypeAttribute against %s." % (c.symbol.s_id,c.stack))
    if compilation_word_handler(c): return
    s_id = c.symbol.s_id
    td : Optional[TypeDefinition] = Type.types.get(s_id)

    # Is this a new attribute or one being modified?
    attrib_so : StackObject = c.stack.tos()
    op_swap(c)
    udt : AF_UserType = c.stack.tos().value
    op_swap(c)
    if attrib_so.value.udta_type is None:
        # This is a new attribute that has no type yet.
        c.log.debug("\nBrand new attribute without a type : %s." % attrib_so)
        assert td, "'%s' is not a valid Type." % s_id
        attrib : Optional[Type] = udt.values.get(attrib_so.value.name)
        assert attrib is None, "An attribute '%s' already exists for UDT %s." % (attrib_so.value, udt.name)
        attrib_so.value.udta_type = Type(s_id)
    else:
        attrib_t = attrib_so.value.udta_type
        if attrib_t is not None:
            c.log.debug("\nSpecialized type? symbol='%s' %s" % (s_id,c.stack))
            # This could be a specialized type such as List.        
            tmp_stack = Stack()
            tmp_stack.push(StackObject(stype=TType, value=attrib_t.name))
            tr_stack = Stack()
            tmp_cont = Continuation(tmp_stack, tr_stack)
            op, found = Type.op(s_id, tmp_cont)
            if found:
                c.log.debug("Calling %s with %s" % (op,tmp_cont.stack))
                op(tmp_cont)

                c.log.debug("Executed conversion and got: %s" % tmp_cont.stack)
                attrib_so.value.udta_type = tmp_cont.stack.tos().stype
            else:
                # Not specialized - starting a new attribute.
                # Finish this one off.
                compile_finish_attribute(c)
                # Now treat the symbol as a new one.
                compile_type_definition(c)
        else:
            c.log.error("New attribute or end of TypeDefinition.")
            assert False
