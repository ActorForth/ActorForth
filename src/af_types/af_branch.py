from itertools import tee
from dataclasses import dataclass
from typing import Iterator, Tuple

from . import *
from .af_int import *
from stack import *

"""

DmovR : _ -> r:_.

PCSave : -> r:PC;
	cont.

countdown : Int -> r:PC;
	DmovR PCSave

loop : Int r:PC -> r:PC | r:None;
	rswap
	rdup
	rdup PCRestore

10 countdown 
"""

@dataclass
class PCSave:
	val : int
	count : int 
	pc : Iterator[Tuple[int,Tuple[Operation,Symbol]]]
	op : Operation
	symbol : Symbol

TPCSave = Type("PCSave")

###
### NOTE : We only perform type/stack checking on the dstack for now.
###



def op_rdup(c: AF_Continuation) -> None:
    op1 = c.rstack.tos()
    c.rstack.push(op1)
make_word_context('rdup', op_rdup)


def op_rswap(c: AF_Continuation) -> None:
    op1 = c.rstack.pop()
    op2 = c.rstack.pop()
    c.rstack.push(op1)
    c.rstack.push(op2)
make_word_context('rswap', op_rswap)


def op_rdrop(c: AF_Continuation) -> None:
    op1 = c.rstack.pop()
make_word_context('rdrop', op_rdrop)


def op_2rdup(c: AF_Continuation) -> None:
    op1 = c.rstack.tos()
    op_rswap(c)
    op2 = c.rstack.tos()
    op_rswap(c)
    c.rstack.push(op2)
    c.rstack.push(op1)
make_word_context('2rdup', op_2rdup)


def op_rstack(c: AF_Continuation) -> None:
    if c.rstack.depth() == 0:
        print("(stack empty)")
    else:
        for n in reversed(c.rstack.contents()):
            print('%s'%str(n))
    if c.prompt:
        print(c.prompt,end='',flush=True)
make_word_context('rstack', op_rstack)


def op_mov_to_rstack(c: AF_Continuation) -> None:
	assert c.stack.depth() != 0
	x = c.stack.pop()
	c.rstack.push(x)
make_word_context('to_rstack', op_mov_to_rstack, [TAny], [])


def op_mov_to_dstack(c: AF_Continuation) -> None:
	assert c.rstack.depth() != 0
	x = c.rstack.pop()
	c.stack.push(x)
make_word_context('to_dstack', op_mov_to_dstack, [], [TAny])


def op_pcsave(c: AF_Continuation) -> None:
	i = c.rstack.pop().value
	assert i >= 0
	c.pc, pc = tee(c.pc)
	#pc = c.pc
	c.rstack.push(StackObject(value=PCSave(i,i,pc,c.op,c.symbol), stype=TPCSave))
make_word_context('pcsave', op_pcsave)


def op_start_countdown(c: AF_Continuation) -> None:
	op_mov_to_rstack(c)
	op_pcsave(c)
make_word_context('countdown', op_start_countdown, [TInt], [])


def op_start_countdown_atom(c: AF_Continuation) -> None:
	op_int(c)
	op_start_countdown(c)
make_word_context('countdown', op_start_countdown_atom, [TAtom], [])


def op_loop(c: AF_Continuation) -> None:
	pcobj = c.rstack.tos()
	assert pcobj != KStack.Empty and pcobj.stype == TPCSave
	pc = pcobj.value
	pc.count -=1
	if pc.count == 0:
		op_rdrop(c)
	else:
		c.pc, pc.pc = tee(pc.pc)
		#c.op = pc.op 
		#c.symbol = pc.symbol
		
make_word_context('loop', op_loop)

def op_loop_count(c: AF_Continuation) -> None:
	pcobj = c.rstack.tos()
	assert pcobj.stype == TPCSave
	pc = pcobj.value
	c.stack.push(StackObject(value=pc.val - (pc.val-pc.count), stype=TInt))
make_word_context('lcount', op_loop_count, [], [TInt])	

#def op_test_loop(c: AF_Continuation) -> None:
#	count = c.stack.tos().value
#	op_start_countdown(c)
	

