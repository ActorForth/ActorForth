"""
continuation.py - the ultimate Context of all state and computation centers here.

INTRO 3 : The Continuation contains all state of the system except for the
          word dictionaries. (TODO: Later all but the global 'Any' dictionaries)
          may be moved into the Continuation as well.)
"""

from typing import Optional, Iterator, Tuple

from dataclasses import dataclass

from stack import Stack, KStack
from af_types import AF_Continuation, Symbol, TAny, Tuple
from operation import Operation, op_nop

import logging
import sys

ROOT_LOGGING_DEFAULT = logging.WARNING # logging.DEBUG # 
LOGGING_DEFAULT = logging.DEBUG

root_log = logging.getLogger()
root_log.setLevel(ROOT_LOGGING_DEFAULT)

ch = logging.StreamHandler(sys.stdout)
ch.setLevel(LOGGING_DEFAULT)
FORMAT = "[%(filename)s:%(lineno)s - %(funcName)20s() ] %(message)s"
formatter = logging.Formatter(FORMAT)
ch.setFormatter(formatter)
root_log.addHandler(ch)


class Continuation(AF_Continuation):
    """
    INTRO 3.1 :  Continuation consists of the Stack, (Soon a Return Stack
                 will be added) the current Symbol being interpreted, and
                 the Operation that was discovered for this context to
                 operate on the Symbol.
    """
    def __init__(self, stack : Stack = None, rstack : Stack = None, symbol : Symbol = None):
        self.pc : Iterator[Tuple[int,Tuple[Operation,Symbol]]]
        self.stack = stack or Stack()
        self.rstack = rstack or Stack()
        self.symbol = symbol or Symbol() 
        self.op : Operation = Operation("nop",op_nop)


        """
        INTRO 3.2 : We also track a Debug state.
        """

        self.prompt: str = "ok: "

        self.debug : bool = False
        self.cdepth : int = 0        # Depth of calls for debug tab output.
        self.log : logging.Logger = root_log


    """
    INTRO 3.3 : When a Continuation is executed it looks at the Type of the
                object on top of the stack (tos) and makes that the context
                by which it will executed. If the stack is empty it will
                default to the global 'Any' type word dictionary.
    """
    def execute(self, next_word : Iterator[Tuple[Operation,Symbol]] ) -> AF_Continuation:

        #print("ENTERING INTO EXECUTE.")
        try:
            self.pc = enumerate(iter(next_word))
            while self.pc:

                pos, (op, symbol) = next(self.pc)
                self.op = op
                self.symbol = symbol
                self.log.debug("EXECUTING WORD #%s: Op=%s, Symbol=%s." % (pos,self.op.name,self.symbol))
                #print("EXECUTING WORD #%s: Op=%s, Symbol=%s." % (pos,self.op.name,self.symbol))

                # Assume that we're an empty stack and will use the TAny op_handler.
                type_context = TAny
                tos = self.stack.tos()
                if tos != KStack.Empty:
                    # Make the tos type's op_handler our context instead.
                    type_context = tos.stype


                """
                    INTRO 3.4 : Execute the operation according to our context.
                                All Type handlers take a Continuation and return nothing.
                                (See af_types/__init__.py for the default handler.)
        
                                Continue to aftype.py for INTRO stage 4.
                """
                handler = type_context.handler()
                #return handler(self)
                handler(self)
        except StopIteration:
            pass
        self.log.debug("RETURNING FROM EXECUTE: %s" % self.op.name)
        #print("RETURNING FROM EXECUTE: %s" % self.op.name)
        return self


    def __str__(self) -> str:
        result = "Cont:\n\tsym=%s\n\t op=%s" % (self.symbol, self.op)
        if self.debug:
            result += "\n\tDebug : On (Call Depth:%s)" % self.cdepth

            content = ""
            if self.stack.is_empty():
                content += "empty"
            else:
                for n, s in enumerate(self.stack.contents()[::-1]):
                    content += "%s) type=%s,  val=%s\n\t\t" % (n, s.stype.name, s.value)
            content += "\n"

            result += "\n\tStack =\t%s " % (content)
        return result
