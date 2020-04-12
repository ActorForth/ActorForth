#
#	continuation.py		-	Continuation state of our environment.
#

from typing import Callable, List, Optional

from dataclasses import dataclass

from stack import Stack, KStack

#from aftype import AF_Type, AF_Continuation, Symbol, Location
from af_types import *

from operation import Operation

@dataclass
class Continuation(AF_Continuation):
    stack : Stack
    symbol : Optional[Symbol] = None
    op : Operation = Operation("nop",op_nop)

    debug : bool = False
    ddepth : int = 0        # Depth of calls for debug tab output.


    def execute(self) -> None:
        # Assume that we're an empty stack and will use the TAny op_handler.
        type_context = TAny
        tos = self.stack.tos()
        if tos != KStack.Empty:
            # Make the tos type's op_handler our context instead.
            type_context = tos.type

        # Execute the operation according to our context.            
        handler = type_context.handler()
        return handler(self)


    def __str__(self) -> str:
        result = "Cont: %s" % self.op
        if self.debug:
            result += "\nDebug : On (Depth:%s)" % self.ddepth

        result += "\n      Stack(%s) = %s " \
            % (len(self.stack.contents()),self.stack.contents())
        return result
