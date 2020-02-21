#from stack import Stack

from . import *

TBool = Type("Bool")

#
#   Boolean algebra handling
#

def op_bool(s: Stack) -> None:
    stack_object = s.pop()
    print("op_bool stack_object = %s" % stack_object)

    result = StackObject(None, TBool)
    if stack_object.type == "Bool":
        print ("Got a Bool")
        result.value = stack_object.value
    if stack_object.type == TAtom:
        print ("Got an Atom")
        if stack_object.value == "True":
            print ("Found a True Atom")
            result.value = True
        if  stack_object.value == "False":
            print ("Found a False Atom")
            result.value = False

    assert result.value is not None, "%s is not a valid Boolean value." % stack_object.value
    
    s.push(result)


def optionally_infer_type_from_atom(s: Stack) -> StackObject:
    sobj1 = s.pop()
    sobj2 = s.tos()

    # If the top item is an Atom but the second item
    # is not, then automatically infer the top item's
    # type from the second item then perform the comparison.
    if sobj1.type is TAtom and sobj2 is not TAtom:
        # Pass along the entire list of types from the stack
        # in case the type's ctor takes multiple parameters.
        ctor = sobj2.type.find_ctor([o.type for o in s.contents()])
        assert ctor, "Couldn't find a ctor to infer a new %s type from %s." % (sobj2.type, sobj1)
        # Call the ctor and put its result on the stack.
        s.push(sobj1)
        ctor(s)
        sobj1 = s.pop()
        print("Converted from %s to %s." % (sobj1,s.tos().type))
        print("New stack is %s." % s.contents())
    
    return sobj1


def op_equals(s: Stack) -> None:
    sobj1 = optionally_infer_type_from_atom(s)
    sobj2 = s.pop()
    # Now we pop off whatever is the ultimate object that's 
    # possibly been inferred.
    sobj2 = s.pop()
    s.push(StackObject(sobj1 == sobj2, TBool))

def op_not_equals(s: Stack) -> None:
    op_equals(s)
    result = s.tos()
    result.value = not result.value

def op_less_than(s: Stack) -> None:
    sobj1 = optionally_infer_type_from_atom(s)
    sobj2 = s.pop()
    print("is %s (%s) < %s (%s)?" % (sobj2.value, type(sobj2.value), sobj1.value, type(sobj1.value)))
    s.push(StackObject(sobj2.value < sobj1.value, TBool))

def op_greater_than(s: Stack) -> None:
    sobj1 = optionally_infer_type_from_atom(s)
    sobj2 = s.pop()
    s.push(StackObject(sobj2.value > sobj1.value, TBool))    

def op_less_than_or_equal_to(s: Stack) -> None:
    sobj1 = optionally_infer_type_from_atom(s)
    sobj2 = s.pop()
    s.push(StackObject(sobj2.value <= sobj1.value, TBool))

def op_greater_than_or_equal_to(s: Stack) -> None:
    sobj1 = optionally_infer_type_from_atom(s)
    sobj2 = s.pop()
    s.push(StackObject(sobj2.value >= sobj1.value, TBool))


def op_not(s: Stack) -> None:
    # Restrict to only workong on Bools!
    op1 = s.tos().value = not s.tos().value

#   Int dictionary
TBool.register_ctor('bool',op_bool,[TAtom])
TBool.register_ctor('==',op_equals,[TAny])
TBool.register_ctor('!=',op_not_equals,[TAny])
TBool.register_ctor('<',op_less_than,[TAny])
TBool.register_ctor('>',op_greater_than,[TAny])
TBool.register_ctor('<=',op_less_than_or_equal_to,[TAny])
TBool.register_ctor('>=',op_greater_than_or_equal_to,[TAny])

#Type.add_op('int', op_int, TypeSignature([TAtom],[TInt]))
flags = WordFlags()
Type.add_op('not', op_not, TypeSignature([TBool],[TBool]), flags, "Bool")
