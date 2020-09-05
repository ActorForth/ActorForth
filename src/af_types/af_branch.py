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
	pc : Iterator[Tuple[int,Tuple[Operation,Symbol]]]
	op : Operation
	symbol : Symbol

	val: Optional[int] = None
	count: Optional[int] = None

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
        print("(return stack empty)")
    else:
        for n in reversed(c.rstack.contents()):
            print('%s'%str(n))
    if c.prompt:
        print(c.prompt,end='',flush=True)
make_word_context('rstack', op_rstack)
make_word_context('.r', op_rstack)


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
	c.log.debug("pcsave stack before op:%s sym:%s stack:%s." % (c.op.name, c.symbol.s_id, c.rstack))
	c.pc, pc = tee(c.pc)
	c.rstack.push(StackObject(value=PCSave(pc,c.op,c.symbol), stype=TPCSave))
	c.log.debug("pcsave stack after op:%s sym:%s stack:%s." % (c.op.name, c.symbol.s_id, c.rstack))
	#print("op_pcsave : %s" % c.op.name)
make_word_context('pcsave', op_pcsave)


def op_loop_pcsave(c: AF_Continuation) -> None:
	i = c.rstack.pop().value
	assert i >= 0
	op_pcsave(c)
	c.rstack.tos().value.val = i
	c.rstack.tos().value.count = i
	#print("op_loop_pcsave : %s" % i)


def op_pcreturn(c: AF_Continuation) -> None:
	"""
	TODO: Forget about saving loop objects, alert
		  on them and raise an error because it
		  means we have looping cross-levels and 
		  that won't work!
	"""
	c.log.debug("pcreturn stack before op:%s sym:%s stack:%s." % (c.op.name, c.symbol.s_id, c.rstack))
	assert c.rstack.tos().stype == TPCSave
	# loops = []
	# # Save any loop objects we encounter...
	# while c.rstack.depth() and c.rstack.tos().value.val is not None:
	# 	loops.append(c.rstack.pop())
	pc = c.rstack.tos().value
	c.pc, pc.pc = tee(pc.pc)
	c.op = pc.op
	c.symbol = pc.symbol
	op_rdrop(c)
	# # Restore the loop objects.
	# for l in loops:
	# 	c.rstack.push(l)
	#print("op_pcreturn : %s" % c.op.name)
	c.log.debug("pcreturn stack after op:%s sym:%s stack:%s." % (c.op.name, c.symbol.s_id, c.rstack))


def op_start_countdown(c: AF_Continuation) -> None:
	assert c.stack.tos().value >= 0, "Cannot countdown from a negative number."
	op_mov_to_rstack(c)
	op_loop_pcsave(c)
make_word_context('countdown', op_start_countdown, [TInt], [])


def op_start_countdown_atom(c: AF_Continuation) -> None:
	op_int(c)
	op_start_countdown(c)
make_word_context('countdown', op_start_countdown_atom, [TAtom], [])


def op_loop(c: AF_Continuation) -> None:
	"""
	TODO: Forget about saving return objects, alert
		  on them and raise an error because it
		  means we have looping cross-levels and 
		  that won't work!
	"""	
	#returns = []
	# Save any regular returns we encounter...
	#while c.rstack.tos().value.val is None:
	#	returns.append(c.rstack.pop())
	pcobj = c.rstack.tos()
	assert pcobj != KStack.Empty and pcobj.stype == TPCSave
	pc = pcobj.value
	if pc.count > 0 : pc.count -=1
	if pc.count == 0:
		op_rdrop(c)
		# Restore our returns.
		#for r in returns:
		#	c.rstack.push(r)
		print("op_loop continues...")
	else:
		c.pc, pc.pc = tee(pc.pc)
		c.op = pc.op 
		c.symbol = pc.symbol
		#for r in returns[-1::]:
		#	print("dropping return for : %s" % r.value.op.name)
		print("op_loop loops back to : %s" % c.op.name)
make_word_context('loop', op_loop)

def op_loop_count(c: AF_Continuation) -> None:
	pcobj = c.rstack.tos()
	assert pcobj.stype == TPCSave
	pc = pcobj.value
	c.stack.push(StackObject(value=pc.val - (pc.val-pc.count), stype=TInt))
make_word_context('lcount', op_loop_count, [], [TInt])	

	

