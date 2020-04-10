from . import *

TDebug = Type("Debug")

flags = WordFlags()

def op_debug(c: Continuation) -> None:
    c.stack.push(StackObject("Debug",TDebug))
Type.register_ctor('Debug',Operation('debug',op_debug),[])

def op_on(c: Continuation) -> None:
    c.debug = True
    c.stack.pop()
Type.add_op(Operation('on', op_on), TypeSignature([TDebug],[]), flags, "Debug")    

def op_off(c: Continuation) -> None:
    c.debug = False
    c.stack.pop()
Type.add_op(Operation('off', op_off), TypeSignature([TDebug],[]), flags, "Debug")    




