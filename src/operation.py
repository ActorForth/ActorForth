"""
operation.py -  the operations which make up the executable words of ActorForth.

INTRO 6 : Named words which implement the behavior of ActorForth. New words
          can be built (presently only global words for 'Any' type) and are
          first class citizens as if they were primitives. Operations are
          stored in various Type dictionaries.
"""

import logging
from typing import Dict, List, Tuple, Callable, Any, Optional, Sequence
from dataclasses import dataclass
from itertools import zip_longest

from aftype import AF_Type, AF_Continuation

from stack import Stack



class TypeSignature:

    def __init__(self, in_seq: Sequence["AF_Type"] = None, out_seq: Sequence["AF_Type"] = None ):
        if in_seq is None: in_seq = []
        if out_seq is None: out_seq = []

        self.stack_in : Stack = Stack(in_seq)
        self.stack_out : Stack = Stack(out_seq)


    # Produces a mapped type sequence that accounts for "Any" types.
    def map_from_input_sig(self, sig: Sequence["AF_Type"]) -> Sequence["AF_Type"]:
        result_sig : List["AF_Type"] = []
        assert len(self.stack_in) <= len(sig), "Error! In Stack '%s' longer than Sig '%s'." % (self.stack_in,sig)

        # Iterate over both sequences in reverse.
        for in_s, m_s in zip(self.stack_in.contents()[::-1],sig[::-1]):
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
        for t in self.stack_in.contents():
            out += " %s," % t.name
        out += "] -> ["

        for t in self.stack_out.contents():
            out += " %s," % t.name

        out += "]"
        return out

    def __eq__(self, s : object) -> bool:
        if not isinstance(s, TypeSignature):
            return NotImplemented  
        return (self.stack_in == s.stack_in) and (self.stack_out == s.stack_out)

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

    def check_stack_effect(self, sig_in : Optional[ Stack ] = None) -> Tuple[ Stack, bool ]:
        """
        Returns the output stack effect of this operation given an optional input stack.
        "Any" types will be specialized to a concrete type if matched against it.

        If no sequence is passed in we just copy the input type signature for this Operation
        and treat that as the input sequence for purposes of stack effect.

        Also returns a secondary Boolean that is true only if the output matches the 
        output type signature declared for this Operation.
        """
        matches: bool = True
        sig_out: Stack = Stack()
        if sig_in is None: sig_out = self.sig.stack_in.copy()
        else: sig_out = sig_in.copy()
        consume_in = self.sig.stack_in.copy()

        # Consume as much of the input as our input signature requires.
        for i in range(len(self.sig.stack_in)):
            in_type = sig_out.pop()
            match_type = consume_in.pop()

            if match_type == "Any":
                match_type = in_type
            if in_type == "Any":
                in_type = match_type

            logging.debug("in_type:%s =?= match_type:%s : %s" % (in_type, match_type, in_type==match_type) )

            if in_type != match_type: matches = False

        # Tack on the output stack effect that we're claiming.
        out_sig = self.sig.stack_out.contents()
        for i in out_sig:
            logging.debug("adding output type:%s" % i)
            sig_out.push(i)

        # Make sure we match on the final output signature. But only up until as
        # much as the output signature declares. Extra deeper items are ok.
        test_match_out = self.sig.stack_out.copy()
        test_sig_out = sig_out.copy()
        for i in range(len(test_match_out)):
            x = test_match_out.pop()
            y = test_sig_out.pop()
            if x != y:
                matches = False
                logging.debug("sig_out: %s != stack_out: %s" % (sig_out, self.sig.stack_out))

        logging.debug("Returning output signature: %s with matching = %s." % (sig_out,matches))
        return sig_out, matches
  

#Op_list = List[Tuple[Operation, TypeSignature]]
Op_list = List[Operation]

Op_map = List[Tuple[Sequence["AF_Type"],Operation]]




def op_nop(c: "AF_Continuation") -> None:
    pass   



