 #from stack import Stack

from . import *

TBool = Type("Bool")

#
#   Boolean algebra handling
#

def op_bool(c: AF_Continuation) -> None:
    stack_object = c.stack.pop()
    #print("op_bool stack_object = %s" % stack_object)

    result = StackObject(None, TBool)
    if stack_object.type == TBool:
        #print ("Got a Bool")
        result.value = stack_object.value
    if stack_object.type == TAtom:
        #print ("Got an Atom")
        if stack_object.value == "True":
            #print ("Found a True Atom")
            result.value = True
        if  stack_object.value == "False":
            #print ("Found a False Atom")
            result.value = False

    assert result.value is not None, "%s is not a valid Boolean value." % stack_object.value
    
    c.stack.push(result)


def optionally_infer_type_from_atom(c: AF_Continuation) -> StackObject:
    sobj1 = c.stack.pop()
    sobj2 = c.stack.tos()

    # If the top item is an Atom but the second item
    # is not, then automatically infer the top item's
    # type from the second item then perform the comparison.
    if sobj1.type is TAtom and sobj2 is not TAtom:
        #print("Trying to infer %s type from %s given the following: %s." % (sobj2.type, sobj1, [o.type for o in s.contents()]))
        # Pass along the entire list of types from the stack
        # in case the type's ctor takes multiple parameters.
        ##ctor = sobj2.type.find_ctor([o.type for o in s.contents()])
        ctor = Type.find_ctor( (sobj2.type.name), [o.type for o in c.stack.contents()] )
        assert ctor, "Couldn't find a ctor to infer a new %s type from %s." % (sobj2.type, sobj1)
        # Call the ctor and put its result on the stack.
        c.stack.push(sobj1)
        ctor(c)
        sobj1 = c.stack.pop()
        #print("Converted from %s to %s." % (sobj1,s.tos().type))
        #print("New stack is %s." % s.contents())
    
    return sobj1


def op_equals(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    # Now we pop off whatever is the ultimate object that's 
    # possibly been inferred.
    sobj2 = c.stack.pop()
    c.stack.push(StackObject(sobj1 == sobj2, TBool))

def op_not_equals(c: AF_Continuation) -> None:
    op_equals(c)
    result = c.stack.tos()
    result.value = not result.value

def op_less_than(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    sobj2 = c.stack.pop()
    print("is %s (%s) < %s (%s)?" % (sobj2.value, type(sobj2.value), sobj1.value, type(sobj1.value)))
    c.stack.push(StackObject(sobj2.value < sobj1.value, TBool))

def op_greater_than(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    sobj2 = c.stack.pop()
    c.stack.push(StackObject(sobj2.value > sobj1.value, TBool))    

def op_less_than_or_equal_to(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    sobj2 = c.stack.pop()
    c.stack.push(StackObject(sobj2.value <= sobj1.value, TBool))

def op_greater_than_or_equal_to(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    sobj2 = c.stack.pop()
    c.stack.push(StackObject(sobj2.value >= sobj1.value, TBool))


def op_not(c: AF_Continuation) -> None:
    # Restrict to only workong on Bools!
    op1 = c.stack.tos().value = not c.stack.tos().value
Type.add_op(Operation('not', op_not, sig=TypeSignature([TBool],[TBool]) ), "Bool")
    

#   Bool dictionary
Type.register_ctor('Bool',Operation('bool',op_bool),[TAtom])
Type.register_ctor('Bool',Operation('bool',op_bool),[TBool])
Type.register_ctor('Bool',Operation('==',op_equals),[[TAny,0],[TAny,0]])
Type.register_ctor('Bool',Operation('!=',op_not_equals),[[TAny,0],[TAny,0]])
Type.register_ctor('Bool',Operation('<',op_less_than),[[TAny,0],[TAny,0]])
Type.register_ctor('Bool',Operation('>',op_greater_than),[[TAny,0],[TAny,0]])
Type.register_ctor('Bool',Operation('<=',op_less_than_or_equal_to),[[TAny,0],[TAny,0]])
Type.register_ctor('Bool',Operation('>=',op_greater_than_or_equal_to),[[TAny,0],[TAny,0]])

#Type.add_op('int', op_int, TypeSignature([TAtom],[TInt]))
