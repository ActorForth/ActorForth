from dataclasses import dataclass
from copy import deepcopy

from . import *
from af_types.af_debug import *
from af_types.af_any import op_swap
from compiler import compilation_word_handler
from continuation import Continuation
from operation import Operation_def

@dataclass
class AF_UserType:
    name: str    
    values: dict

@dataclass
class UDTAttribute:
    name: str
    udta_type: Optional[Type]


#
#   An instance of an AF_UserType on the stack will be like:
#
#   StackObject( stype = Type(name=<TypeName>), 
#                value = Dict{ attribute_name : 
#                              StackObject( stype = Type(name=<Attribute Type),
#                                           value = <value>)
#                             } )    

def type_definition_handler(c: AF_Continuation) -> None:
    compile_type_definition(c)

def type_attribute_handler(c: AF_Continuation) -> None:
    compile_type_attribute(c)

TTypeDefinition = Type("TypeDefinition", handler = type_definition_handler)
TTypeAttribute = Type("TypeAttribute", handler = type_attribute_handler)
TAttrReference = Type("AttrReference")


def op_type(c: AF_Continuation) -> None:
    """
    Atom -> TypeDefinition(AF_UserType)
    """
    obj = AF_UserType(name=c.stack.tos().value, values = dict())
    assert obj.name != obj.name.lower(), "ERROR - Type can't have only lower case letters as this prevents creation of ctor."
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

    # Now construct a ctor for this UDT.
    ctor_name = udt.name.lower()

    def get_udt_ctor() -> Operation_def:

        _attribs = list(Type.udts[s].items())
        _attribs.reverse()

        def udt_ctor(c: AF_Continuation) -> None:
            _values : Dict[str,StackObject] = {}
            for attrib_name, attrib_type in _attribs:
                attrib_value = c.stack.pop().value
                _values[attrib_name] = StackObject(value = attrib_value, stype = attrib_type)

            c.stack.push(StackObject(stype=Type(s), value=_values))

        return udt_ctor

    # Now make a word to access each attribute of our User Defined Type.
    for attrib_name, attrib_type in list(Type.udts[s].items()):
        def getter(c: AF_Continuation) -> None:
            so : StackObject = c.stack.tos().value[attrib_name]
            attrib_type = Type.udts[s][attrib_name]
            c.stack.push(StackObject(stype = attrib_type, value = so.value))
        make_word_context(attrib_name, getter, [Type(s)], [attrib_type])

    attribute_types = list(Type.udts[s].values())
    make_word_context(ctor_name, get_udt_ctor(),attribute_types,[udt] )

    # Now make a reference operation.
    make_word_context('->', lambda c: c.stack.push(StackObject(stype=TAttrReference, value=Type(s))), [Type(s)], [Type(s), TAttrReference])

    # Now make a word to refereemce each attribute of our User Defined Type.
    for attrib_name, attrib_type in list(Type.udts[s].items()):
        def ref_getter(c: AF_Continuation) -> None:
            c.stack.pop()
            so : Dict[str,StackObject] = c.stack.tos().value
            attrib_type = Type.udts[s][attrib_name]

            def ref_attrib(val: Any) -> Any:
                if val is not None:
                    so[attrib_name].value = val
                #c.log.warning("so of type %s == %s" % (type(so),so))
                return so[attrib_name].value


            c.stack.push(StackObject(stype = attrib_type, value = ref_attrib))

        make_word_context(attrib_name, ref_getter, [Type(s), TAttrReference], [Type(s), attrib_type])

    attribute_types = list(Type.udts[s].values())
    make_word_context(ctor_name, get_udt_ctor(),attribute_types,[udt] )

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
