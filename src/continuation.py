#
#	continuation.py		-	Continuation state of our environment.
#

from typing import Callable, List, Optional

from dataclasses import dataclass

from stack import Stack


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


Op_name = str
Operation_def = Callable[["Continuation"],None]

class Operation:

    def __init__(self, name: Op_name, op: Operation_def, words: List["Operation"] = None) -> None:
        self.name = name
        self.the_op : Operation_def = op
        self.words : List["Operation"] = words or []

    def add_word(self, op: "Operation") -> bool:
        # Should check for valid stack type signature.
        self.words.append(op)
        return True

    def __call__(self, cont: "Continuation") -> None:
        self.the_op(cont)

    def __str__(self) -> str:
        result = "Op{'%s':(%s)" % (self.name, self.the_op.__qualname__)
        result += str(self.words)
        result += "}"
        return result

    def __repr__(self) -> str:
        return self.__str__()

    def short_name(self) -> str:
    	return self.name    	


def op_nop(c: "Continuation") -> None:
    pass     


@dataclass
class Continuation:
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
