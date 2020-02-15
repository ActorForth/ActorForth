#from stack import Stack

from . import *

TInt = Type("Int")

#
#   Integer handling
#

def op_int(s: Stack) -> None:
    i = int(s.pop().value)
    assert i <  999999999999, "int overflow > 999999999999"
    assert i > -999999999999, "int underflow < -999999999999"
    s.push(StackObject(i,TInt))
#op_int.sig=TypeSignature([TAtom],[TInt])

def op_plus(s: Stack) -> None:
    op1 = s.pop().value
    op2 = s.pop().value
    result = op1+op2
    # Guarantee output is valid and not overflow.
    assert int(result) - op2 == op1, "python math error"
    s.push(StackObject(result,TInt))
    op_int(s) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
#op_plus.sig=TypeSignature([TInt,TInt],[TInt])

def op_minus(s: Stack) -> None:
    op1 = s.pop().value
    op2 = s.pop().value
    result = op2-op1
    # Guarantee output is valid and not overflow.
    assert int(result) + op1 == op2, "python math error"
    s.push(StackObject(result,TInt))
    op_int(s) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
#op_minus.sig=TypeSignature([TInt,TInt],[TInt])

def op_multiply(s: Stack) -> None:
    op1 = s.pop().value
    op2 = s.pop().value
    result = op2*op1
    # Guarantee output is valid and not overflow.
    assert int(result) / op1 == op2, "python math error"
    s.push(StackObject(result,TInt))
    op_int(s) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
#op_multiply.sig=TypeSignature([TInt,TInt],[TInt])

def op_divide(s: Stack) -> None: 
    assert s.tos().value != 0, "int division by zero error."
    op1 = s.pop().value
    op2 = s.pop().value
    result = int(op2/op1)
    remainder = op2 - (result * op1)
    s.push(StackObject(result, TInt))
    s.push(StackObject(remainder, TInt))
#op_divide.sig=TypeSignature([TInt,TInt],[TInt,TInt])

#   Int dictionary
Type.add_op('int', op_int, TypeSignature([TAtom],[TInt]))
Type.add_op('+', op_plus, TypeSignature([TInt,TInt],[TInt]), "Int")
Type.add_op('-', op_minus, TypeSignature([TInt,TInt],[TInt]), "Int")
Type.add_op('*', op_multiply, TypeSignature([TInt,TInt],[TInt]), "Int")
Type.add_op('/', op_divide, TypeSignature([TInt,TInt],[TInt,TInt]), "Int")
