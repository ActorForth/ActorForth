import logging
from . import *

TDebug = Type("Debug")

def op_debug(c: AF_Continuation) -> None:
    c.stack.push(StackObject(value="Debug",type=TDebug))
Type.register_ctor('Debug',Operation('debug',op_debug),[TAny])
Type.add_op(Operation('debug', op_debug, sig=TypeSignature([],[TDebug]) ))


def op_on(c: AF_Continuation) -> None:
    c.debug = True
    c.log.setLevel(logging.DEBUG)
    root_log = logging.getLogger()
    root_log.setLevel(logging.DEBUG)
    c.stack.pop()
Type.add_op(Operation('on', op_on, sig=TypeSignature([TDebug],[]) ), "Debug")

def op_off(c: AF_Continuation) -> None:
    c.debug = False
    c.log.setLevel(logging.WARNING)
    root_log = logging.getLogger()
    root_log.setLevel(logging.WARNING)
    c.stack.pop()
Type.add_op(Operation('off', op_off, sig=TypeSignature([TDebug],[]) ), "Debug")
