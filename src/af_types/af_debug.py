import logging
from . import *

TDebug = Type("Debug")

def op_debug(c: AF_Continuation) -> None:
    c.stack.push(StackObject(value="Debug",stype=TDebug))
Type.register_ctor('Debug',Operation('debug',op_debug),[StackObject(stype=TAny)])
make_word_context('debug', op_debug, [], [TDebug])


def op_on(c: AF_Continuation) -> None:
    c.debug = True
    c.log.setLevel(logging.DEBUG)
    root_log = logging.getLogger()
    root_log.setLevel(logging.DEBUG)
    c.stack.pop()
make_word_context("on", op_on, [TDebug])


def op_off(c: AF_Continuation) -> None:
    c.debug = False
    c.log.setLevel(logging.WARNING)
    root_log = logging.getLogger()
    root_log.setLevel(logging.WARNING)
    c.stack.pop()
make_word_context("off", op_off, [TDebug])