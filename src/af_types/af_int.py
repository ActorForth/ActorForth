#from stack import Stack

from . import *

TInt = Type("Int")
flags = WordFlags()

#
#   Integer handling
#

# Constructors
def op_int(c: Continuation) -> None:
    i = int(c.stack.pop().value)
    assert i <  999999999999, "int overflow > 999999999999"
    assert i > -999999999999, "int underflow < -999999999999"
    c.stack.push(StackObject(i,TInt))
#   Int dictionary
Type.register_ctor('Int', Operation('int',op_int), [TInt])
Type.register_ctor('Int', Operation('int',op_int), [TAtom])

# Operations

def op_plus(c: Continuation) -> None:
    op1 = c.stack.pop().value
    op2 = c.stack.pop().value
    result = op1+op2
    # Guarantee output is valid and not overflow.
    assert int(result) - op2 == op1, "python math error"
    c.stack.push(StackObject(result,TInt))
    op_int(c) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
Type.add_op(Operation('+',op_plus), TypeSignature([TInt,TInt],[TInt]), flags, "Int")    

def op_minus(c: Continuation) -> None:
    op1 = c.stack.pop().value
    op2 = c.stack.pop().value
    result = op2-op1
    # Guarantee output is valid and not overflow.
    assert int(result) + op1 == op2, "python math error"
    c.stack.push(StackObject(result,TInt))
    op_int(c) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
Type.add_op(Operation('-',op_minus), TypeSignature([TInt,TInt],[TInt]), flags, "Int")    

def op_multiply(c: Continuation) -> None:
    op1 = c.stack.pop().value
    op2 = c.stack.pop().value
    result = op2*op1
    # Guarantee output is valid and not overflow.
    if op1 != 0: # Protect against divide by zero error on check.
        assert int(result) / op1 == op2, "python math error"

    c.stack.push(StackObject(result,TInt))
    op_int(c) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
Type.add_op(Operation('*',op_multiply), TypeSignature([TInt,TInt],[TInt]), flags, "Int")    

def op_divide(c: Continuation) -> None: 
    assert c.stack.tos().value != 0, "int division by zero error."
    op1 = c.stack.pop().value
    op2 = c.stack.pop().value
    result = int(op2/op1)
    remainder = op2 - (result * op1)
    c.stack.push(StackObject(result, TInt))
    c.stack.push(StackObject(remainder, TInt))
Type.add_op(Operation('/',op_divide), TypeSignature([TInt,TInt],[TInt,TInt]), flags, "Int")


