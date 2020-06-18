from . import *

TDebug = Type("Debug")

def op_debug(c: AF_Continuation) -> None:
    c.stack.push(StackObject("Debug",TDebug))
# Type.register_ctor('Debug',Operation('debug',op_debug),[])
Type.add_op(Operation('debug', op_debug, sig=TypeSignature([],[TDebug]) ))


def op_on(c: AF_Continuation) -> None:
    c.debug = True
    c.stack.pop()
Type.add_op(Operation('on', op_on, sig=TypeSignature([TDebug],[]) ), "Debug")

def op_off(c: AF_Continuation) -> None:
    c.debug = False
    c.stack.pop()
Type.add_op(Operation('off', op_off, sig=TypeSignature([TDebug],[]) ), "Debug")
