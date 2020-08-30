#from stack import Stack

from . import *

TInt = Type("Int")

#
#   Integer handling
#

# Constructors
def op_int(c: AF_Continuation) -> None:
    #print("\nop_int c.stack.contents = %s." % c.stack.contents())
    i = int(c.stack.pop().value)
    assert i <  999999999999, "int overflow > 999999999999"
    assert i > -999999999999, "int underflow < -999999999999"
    c.stack.push(StackObject(value=i, stype=TInt))
#   Int dictionary
# BDM HACK TODO - Type.register_ctor('Int',Operation('int',op_int),[StackObject(stype=TAny)])
make_word_context('int', op_int, [TAny],[TInt])


# Operations
def op_plus(c: AF_Continuation) -> None:
    op1 = c.stack.pop().value
    op2 = c.stack.pop().value
    result = op1+op2
    # Guarantee output is valid and not overflow.
    assert int(result) - op2 == op1, "python math error"
    c.stack.push(StackObject(value=result, stype=TInt))
    op_int(c) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
make_word_context('+', op_plus, [TInt, TInt],[TInt])

def op_minus(c: AF_Continuation) -> None:
    op1 = c.stack.pop().value
    op2 = c.stack.pop().value
    result = op2-op1
    # Guarantee output is valid and not overflow.
    assert int(result) + op1 == op2, "python math error"
    c.stack.push(StackObject(value=result, stype=TInt))
    op_int(c) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
make_word_context('-', op_minus, [TInt, TInt],[TInt])

def op_multiply(c: AF_Continuation) -> None:
    op1 = c.stack.pop().value
    op2 = c.stack.pop().value
    result = op2*op1
    # Guarantee output is valid and not overflow.
    if op1 != 0: # Protect against divide by zero error on check.
        assert int(result) / op1 == op2, "python math error"

    c.stack.push(StackObject(value=result, stype=TInt))
    op_int(c) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
make_word_context('*', op_multiply, [TInt, TInt],[TInt])

def op_divide(c: AF_Continuation) -> None:
    assert c.stack.tos().value != 0, "int division by zero error."
    op1 = c.stack.pop().value
    op2 = c.stack.pop().value
    result = int(op2/op1)
    remainder = op2 - (result * op1)
    c.stack.push(StackObject(value=result, stype=TInt))
    c.stack.push(StackObject(value=remainder, stype=TInt))
make_word_context('/', op_divide, [TInt, TInt],[TInt, TInt])
