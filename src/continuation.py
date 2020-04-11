#
#	continuation.py		-	Continuation state of our environment.
#

from typing import Callable, List, Optional

from dataclasses import dataclass

from stack import Stack

from aftype import AF_Type, AF_Continuation

from operation import Operation


@dataclass(frozen = True)
class Location:
    filename : str = "Unknown"
    linenum : int = 0
    column : int = 0


@dataclass(order = True)
class Symbol:
    s_id : str
    location : Location 
    #type : Type 
    
    @property
    def size(self) -> int:
        return len(self.s_id)

    def __eq__(self, symbol = None) -> bool:
        #if type(symbol) is Symbol:
        #    return symbol.s_id == self.s_id
        return symbol == self.s_id  





def op_nop(c: "AF_Continuation") -> None:
    pass     


@dataclass
class Continuation(AF_Continuation):
    stack : Stack
    symbol : Optional[Symbol] = None
    op : Operation = Operation("nop",op_nop)

    debug : bool = False
    ddepth : int = 0        # Depth of calls for debug tab output.

    def __str__(self) -> str:
        result = "Cont: %s" % self.op
        if self.debug:
            result += "\nDebug : On (Depth:%s)" % self.ddepth

        result += "\n      Stack(%s) = %s " \
            % (len(self.stack.contents()),self.stack.contents())
        return result
