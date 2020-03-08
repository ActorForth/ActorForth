#
#	continuation.py		-	Continuation state of our environment.
#

from typing import Dict, List, Tuple, Callable, Any, Optional

from dataclasses import dataclass

from stack import Stack

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
	op : Operation = Operation("nop",op_nop)

	def __str__(self) -> str:
		result = "Cont: %s" % self.op
		result += "\n      Stack(%s) = %s " \
			% (len(self.stack.contents()),self.stack.contents())
		return result			

	