from . import *

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
	x = c.stack.pop()
	c.rstack.push(x)
make_word_context('to_rstack', op_mov_to_rstack, [TAny], [])


def op_mov_to_dstack(c: AF_Continuation) -> None:
	x = c.rstack.pop()
	c.stack.push(x)
make_word_context('to_dstack', op_mov_to_dstack, [], [TAny])


def op_start_countdown(c: AF_Continuation) -> None:
	pass

	