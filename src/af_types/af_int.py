#from stack import Stack

from . import *

TInt = Type("Int")

#
#   Integer handling
#

def op_int(s: Stack, s_id: str) -> None:
    print("op_int(s_id = '%s')\n" % s_id )
    i = int(s.pop().value)
    assert i <  999999999999, "int overflow > 999999999999"
    assert i > -999999999999, "int underflow < -999999999999"
    s.push(StackObject(i,TInt))
op_int.sig=TypeSignature([TAtom],[TInt])

def op_plus(s: Stack, s_id: str) -> None:
    print("op_plus(s_id = '%s')\n" % s_id) 
    op1 = s.pop().value
    op2 = s.pop().value
    result = op1+op2
    # Guarantee output is valid and not overflow.
    assert int(result) - op2 == op1, "python math error"
    s.push(StackObject(result,TInt))
    op_int(s,s_id) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
op_plus.sig=TypeSignature([TInt,TInt],[TInt])

def op_minus(s: Stack, s_id: str) -> None:
    print("op_minus(s_id = '%s')\n" % s_id) 
    op1 = s.pop().value
    op2 = s.pop().value
    result = op2-op1
    # Guarantee output is valid and not overflow.
    assert int(result) + op1 == op2, "python math error"
    s.push(StackObject(result,TInt))
    op_int(s,s_id) # We're cheating here cause, for now, op_int is supposed to take a TAtom!
op_minus.sig=TypeSignature([TInt,TInt],[TInt])

#   Int dictionary
forth_dict.insert(0,('int',op_int))
TInt.forth_dict.insert(0,('+',op_plus))
TInt.forth_dict.insert(0,('-',op_minus))