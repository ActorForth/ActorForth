from typing import Dict, List, Tuple, Callable, Any, Optional, Sequence
from dataclasses import dataclass

from aftype import AF_Type, AF_Continuation

from stack import Stack


@dataclass
class TypeSignature:
    stack_in : Sequence["AF_Type"]
    stack_out : Sequence["AF_Type"]

    def match_in(self, stack: Stack) -> bool:
        if not len(self.stack_in): return True
        if len(self.stack_in) > stack.depth():
            print("match_in: input stack too short for signature.")
            return False
        stack_types = [s.type for s in stack.contents()[len(self.stack_in)*-1:] ]

        print("\nmatch_in: in_types = %s" % (self.stack_in))
        print("match_in: stack_types = %s" % stack_types)
        for in_type in reversed(self.stack_in):
            ## This is now handled in Type class overloads!!
            ## if in_type == TAny: continue
            """
            Should probably have TAny types transform to the discovered type
            so that manipulations across generics are still completely type safe.
            """
            stack_type = stack_types.pop()
            if in_type != stack_type and stack_type != "Any" and in_type != "Any":
                print("match_in: Stack type %s doesn't match input arg type %s." % (type,in_type))
                return False
        print("match_in: Found matching type for stack_in: %s" % self.stack_in)
        return True

    def match_out(self, on_stack_types: List["AF_Type"]) -> bool:
        return True

    def __str__(self) -> str:
        out = "["
        for t in self.stack_in:
            out += " %s," % t.name
        out += "] -> ["

        for t in self.stack_out:
            out += " %s," % t.name

        out += "]"
        return out

Op_name = str
Operation_def = Callable[["AF_Continuation"],None]

class Operation:

    def __init__(self, name: Op_name, op: Operation_def, words: List["Operation"] = None, sig: TypeSignature = None) -> None:
        self.name = name
        self.the_op : Operation_def = op
        self.words : List["Operation"] = words or []
        self.sig : TypeSignature = sig or TypeSignature([],[])

    def add_word(self, op: "Operation") -> bool:
        # Should check for valid stack type signature.
        self.words.append(op)
        return True

    def __call__(self, cont: "AF_Continuation") -> None:
        self.the_op(cont)

    def __str__(self) -> str:
        result = "Op{'%s' %s :(%s)" % (self.name, self.sig, self.the_op.__qualname__)
        result += " %s" % str(self.words)
        result += "}"
        return result

    def __repr__(self) -> str:
        return self.__str__()

    def short_name(self) -> str:
        return self.name        
  

#Op_list = List[Tuple[Operation, TypeSignature]]
Op_list = List[Operation]

Op_map = List[Tuple[Sequence["AF_Type"],Operation]]




def op_nop(c: "AF_Continuation") -> None:
    pass   



