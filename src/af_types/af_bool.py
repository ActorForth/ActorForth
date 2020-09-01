 #from stack import Stack

from . import *
from aftype import StackObject

TBool = Type("Bool")

#
#   Boolean algebra handling
#

def op_bool(c: AF_Continuation) -> None:
    stack_object = c.stack.pop()
    #print("op_bool stack_object = %s" % stack_object)

    result = StackObject(value=None, stype=TBool)
    if stack_object.stype == TBool:
        #print ("Got a Bool")
        result.value = stack_object.value
    if stack_object.stype == TAtom:
        #print ("Got an Atom")
        if stack_object.value == "True":
            #print ("Found a True Atom")
            result.value = True
        if  stack_object.value == "False":
            #print ("Found a False Atom")
            result.value = False

    assert result.value is not None, "%s is not a valid Boolean value." % stack_object.value

    c.stack.push(result)
Type.register_ctor('Bool',Operation('bool',op_bool),[StackObject(stype=TAtom)])
Type.register_ctor('Bool',Operation('bool',op_bool),[StackObject(stype=TBool)])
#Type.register_ctor('Bool',Operation('bool',op_bool),[StackObject(stype=TAny)]) # HACK BDM TODO NASTY!


### TODO :  if issue 4 ( https://github.com/ActorForth/ActorForth/issues/4 ) 
###         is working properly then we shouldn't need this, right?
make_word_context('bool', op_bool, [TAtom], [TBool])
make_word_context('bool', op_bool, [TBool], [TBool])


# TODO : issue #17 wait for created gerneralize type infer
def optionally_infer_type_from_atom(c: AF_Continuation) -> StackObject:
    sobj1 = c.stack.pop()
    sobj2 = c.stack.tos()

    # If the top item is an Atom but the second item
    # is not, then automatically infer the top item's
    # type from the second item then perform the comparison.
    if sobj1.stype is TAtom and sobj2 is not TAtom:
        c.log.debug("Trying to infer %s type from %s given the following: %s." % (sobj2.stype, sobj1, [c.stack.contents()]))
        # Pass along the entire list of types from the stack
        # in case the type's ctor takes multiple parameters.
        
        ctor = Type.find_ctor( (sobj2.stype.name), c.stack.contents() )
        assert ctor, "Couldn't find a ctor to infer a new %s type from %s." % (sobj2.stype, sobj1)
        # Call the ctor and put its result on the stack.
        c.stack.push(sobj1)
        ctor(c)
        sobj1 = c.stack.pop()
        c.log.debug("Converted from %s to %s." % (sobj1,c.stack.tos().stype))

    return sobj1


def op_equals(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    # Now we pop off whatever is the ultimate object that's
    # possibly been inferred.
    sobj2 = c.stack.pop()
    c.stack.push(StackObject(value=(sobj1 == sobj2), stype=TBool))
make_word_context('==', op_equals, [TAny,TAny], [TBool])


def op_not_equals(c: AF_Continuation) -> None:
    op_equals(c)
    result = c.stack.tos()
    result.value = not result.value
make_word_context('!=', op_not_equals, [TAny,TAny], [TBool])


def op_less_than(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    sobj2 = c.stack.pop()
    c.log.debug("is %s (%s) < %s (%s)?" % (sobj2.value, type(sobj2.value), sobj1.value, type(sobj1.value)))
    c.stack.push(StackObject(value=(sobj2.value < sobj1.value), stype=TBool))
make_word_context('<', op_less_than, [TAny,TAny], [TBool])


def op_greater_than(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    sobj2 = c.stack.pop()
    c.stack.push(StackObject(value=(sobj2.value > sobj1.value), stype=TBool))
make_word_context('>', op_greater_than, [TAny,TAny], [TBool])


def op_less_than_or_equal_to(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    sobj2 = c.stack.pop()
    c.stack.push(StackObject(value=(sobj2.value <= sobj1.value), stype=TBool))
make_word_context('<=', op_less_than_or_equal_to, [TAny,TAny], [TBool])


def op_greater_than_or_equal_to(c: AF_Continuation) -> None:
    sobj1 = optionally_infer_type_from_atom(c)
    sobj2 = c.stack.pop()
    c.stack.push(StackObject(value=(sobj2.value >= sobj1.value), stype=TBool))
make_word_context('>=', op_greater_than_or_equal_to, [TAny,TAny], [TBool])


def op_not(c: AF_Continuation) -> None:
    # Restrict to only workong on Bools!
    op1 = c.stack.tos().value = not c.stack.tos().value
make_word_context('not', op_not, [TBool], [TBool])


def op_assert(c: AF_Continuation) -> None:
    predicate = c.stack.pop().value
    assert predicate
make_word_context('assert', op_assert, [TBool], [])


def op_assert_msg(c: AF_Continuation) -> None:
    msg = c.stack.pop().value
    predicate = c.stack.pop().value
    assert predicate, msg
make_word_context('assert', op_assert_msg, [TBool, TAtom], [])