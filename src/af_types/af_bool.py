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
    else:
        print("Wasn't an Atom")

    #if result.value is None:

    assert result.value is not None, "%s is not a valid Boolean value." % stack_object.value
    
    s.push(result)

#def op_plus(s: Stack) -> None:
#    op1 = s.pop().value
#    op2 = s.pop().value
#    result = op1+op2
#    # Guarantee output is valid and not overflow.
#    assert int(result) - op2 == op1, "python math error"
#    s.push(StackObject(result,TInt))
#    op_int(s) # We're cheating here cause, for now, op_int is supposed to take a TAtom!


#   Int dictionary
TBool.register_ctor('bool',op_bool,[TAtom])
#TBool.register_ctor('bool',op_bool,[TBool])

#Type.add_op('int', op_int, TypeSignature([TAtom],[TInt]))
flags = WordFlags()
#Type.add_op('+', op_plus, TypeSignature([TInt,TInt],[TInt]), flags, "Int")
