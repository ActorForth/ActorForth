#from stack import Stack

from . import *

TInt = Type("Int")

#
#   Integer handling
#

# Constructors
def op_int(s: Stack, s_id: Op_name) -> None:
    i = int(s.pop().value)
    assert i <  999999999999, "int overflow > 999999999999"
    assert i > -999999999999, "int underflow < -999999999999"
    s.push(StackObject(i,TInt))


# Operations

def op_plus(s: Stack, s_id: Op_name) -> None:
    op1 = s.pop().value
    op2 = s.pop().value
    result = op1+op2
    # Guarantee output is valid and not overflow.
    assert int(result) - op2 == op1, "python math error"
    s.push(StackObject(result,TInt))
    op_int(s, s_id) # We're cheating here cause, for now, op_int is supposed to take a TAtom!

def op_minus(s: Stack, s_id: Op_name) -> None:
    op1 = s.pop().value
    op2 = s.pop().value
    result = op2-op1
    # Guarantee output is valid and not overflow.
    assert int(result) + op1 == op2, "python math error"
    s.push(StackObject(result,TInt))
    op_int(s, s_id) # We're cheating here cause, for now, op_int is supposed to take a TAtom!

def op_multiply(s: Stack, s_id: Op_name) -> None:
    op1 = s.pop().value
    op2 = s.pop().value
    result = op2*op1
    # Guarantee output is valid and not overflow.
    if op1 != 0: # Protect against divide by zero error on check.
        assert int(result) / op1 == op2, "python math error"

    s.push(StackObject(result,TInt))
    op_int(s, s_id) # We're cheating here cause, for now, op_int is supposed to take a TAtom!

def op_divide(s: Stack, s_id: Op_name) -> None: 
    assert s.tos().value != 0, "int division by zero error."
    op1 = s.pop().value
    op2 = s.pop().value
    result = int(op2/op1)
    remainder = op2 - (result * op1)
    s.push(StackObject(result, TInt))
    s.push(StackObject(remainder, TInt))

#   Int dictionary
Type.register_ctor('Int', Operation('int',op_int), [TInt])
Type.register_ctor('Int', Operation('int',op_int), [TAtom])


#Type.add_op('int', op_int, TypeSignature([TAtom],[TInt]))
flags = WordFlags()
Type.add_op(Operation('+',op_plus), TypeSignature([TInt,TInt],[TInt]), flags, "Int")
Type.add_op(Operation('-',op_minus), TypeSignature([TInt,TInt],[TInt]), flags, "Int")
Type.add_op(Operation('*',op_multiply), TypeSignature([TInt,TInt],[TInt]), flags, "Int")
Type.add_op(Operation('/',op_divide), TypeSignature([TInt,TInt],[TInt,TInt]), flags, "Int")
