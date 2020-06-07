"""
operation.py -  the operations which make up the executable words of ActorForth.

INTRO 6 : Named words which implement the behavior of ActorForth. New words
          can be built (presently only global words for 'Any' type) and are
          first class citizens as if they were primitives. Operations are
          stored in various Type dictionaries.
"""
from typing import Dict, List, Tuple, Callable, Any, Optional, Sequence
from dataclasses import dataclass

from aftype import AF_Type, AF_Continuation

from stack import Stack


@dataclass
class TypeSignature:
    stack_in : Sequence["AF_Type"]
    stack_out : Sequence["AF_Type"]

    # Produces a mapped type sequence that accounts for "Any" types.
    def map_from_input_sig(self, sig: Sequence["AF_Type"]) -> Sequence["AF_Type"]:
        result_sig : List["AF_Type"] = []
        assert len(self.stack_in) <= len(sig), "Error! In Stack '%s' longer than Sig '%s'." % (self.stack_in,sig)

        # Iterate over both sequences in reverse.
        for in_s, m_s in zip(self.stack_in[::-1],sig[::-1]):
            # Upgrade "Any" types to whatever they're being paired with.
            if m_s == "Any":
                m_s = in_s
            elif in_s == "Any":
                in_s = m_s

            assert m_s == in_s, "Error! Input Type '%s' not equivalent to Sig Type '%s' for In Stack = %s matched with Sig %s." % (in_s, m_s, self.stack_in, sig)
            result_sig.insert(0,m_s)
        return result_sig

    # Used by the runtime interpreter to check for mathing types for words.
    def match_in(self, stack: Stack) -> bool:
        try:
            result = self.map_from_input_sig([i.type for i in stack.contents()])
            return True
        except AssertionError:
            return False


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
        qualified_name = "Anonymous"
        try:
            qualified_name = self.the_op.__qualname__
        except AttributeError:
            pass         
        result = "Op{'%s' %s :(%s)" % (self.name, self.sig, qualified_name)
        result += " %s" % str(self.words)
        result += "}"
        return result

    def __repr__(self) -> str:
        return self.__str__()

    def short_name(self) -> str:
        return self.name

    def check_stack_effect(self):
        """
        Check that the body of the operation is well-typed.
        """
        def matches(l,r):
            """
            Does the stack signature l match the stack signature r?
            """
            if len(l) == len(r):
                constraints = zip(l,r)
                for (a,b) in constraints:
                    if a == b or a == "Any" or b == "Any":
                        continue
                    else:
                        return False
                return True
            else:
                return False

        # TODO: go through each word and check if the input sig
        # matches output sig of previous word
 
  

#Op_list = List[Tuple[Operation, TypeSignature]]
Op_list = List[Operation]

Op_map = List[Tuple[Sequence["AF_Type"],Operation]]




def op_nop(c: "AF_Continuation") -> None:
    pass   



