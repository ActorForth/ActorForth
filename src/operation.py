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

from aftype import AF_Type, AF_Continuation, StackObject

from stack import Stack

class SigValueTypeMismatchException(Exception): pass

class TypeSignature:

    def __init__(self, in_seq: Sequence["StackObject"] = None, out_seq: Sequence["StackObject"] = None ):
        if in_seq is None: in_seq = []
        if out_seq is None: out_seq = []

        self.stack_in : Stack = Stack(in_seq)
        self.stack_out : Stack = Stack(out_seq)


    # Produces a mapped type sequence that accounts for "Any" types.
    # Will fail an assertion if the types don't match.
    def map_from_input_sig(self, sig: Sequence[StackObject]) -> Sequence[StackObject]:
        result_sig : List[StackObject] = []
        assert len(self.stack_in) <= len(sig), "Error! In Stack '%s' longer than Sig '%s'." % (self.stack_in,sig)

        # Iterate over both sequences in reverse.
        in_s : StackObject
        m_s  : StackObject
        for in_s, m_s in zip(self.stack_in.contents()[::-1],sig[::-1]):
            # Upgrade "Any" types to whatever they're being paired with.

            logging.debug("in_s type is '%s' : %s." % (type(in_s), in_s) )
            logging.debug("m_s type is '%s' : %s." % (type(m_s),m_s) )

            match_type : AF_Type = m_s.stype
            if match_type == "Any":
                m_s.stype = in_s.stype
            elif in_s.stype == "Any":
                in_s.stype = match_type

            assert m_s.stype == in_s.stype, "Error! Input Type '%s' not equivalent to Sig Type '%s' for In Stack = %s matched with Sig %s." % (in_s, m_s, self.stack_in, sig)

            ### TODO: Confirm stack content here!!!

            result_sig.insert(0,m_s)
        return result_sig


    # Used by the runtime interpreter to check for mathing types for words.
    def match_in(self, stack: Stack) -> bool:
        logging.debug("match_in in_s=%s, matching against stack=%s" % (self.stack_in, stack))
        try:
            #result = self.map_from_input_sig([i.stype for i in stack.contents()])
            result = self.map_from_input_sig(stack.contents())
            logging.debug("match_in returns True.")
            return True
        except AssertionError:
            logging.debug("match_in returns False.")
            return False


    def __str__(self) -> str:
        out = "TSig["
        for t in self.stack_in.contents():
            out += "t=%s" % t.stype.name
            if t.value is not None:
                out += ", v='%s'" % t.value
            out += ', '

        out += "] -> ["

        for t in self.stack_out.contents():
            out += "t=%s" % t.stype.name
            if t.value is not None:
                out += ", v='%s'" % t.value
            out += ', '
        out += "]"
        return out

    def __repr__(self):
      return self.__str__()        

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
        result += " words=%s" % str(self.words)
        result += "}"
        return result

    def __repr__(self) -> str:
        return self.__str__()

    def short_name(self) -> str:
        return self.name

    def check_stack_effect(self, sig_in : Optional[ Stack ] = None, force_composite : bool = False) -> Tuple[ Stack, bool ]:
        """
        force_composite is used for compiling new composite words that may not yet have a word 
        in their word list so would otherwise appear as primitive words and return the final 
        stack effect rather than the starting one which is appropriate when compiling.
        """
        logging.debug("op: %s with sig_in = %s." % (self, sig_in) )
        start_stack : Stack
        match_stack : Stack

        # The start_stack is what we're trying to match with.
        if sig_in is None:
            start_stack = self.sig.stack_in.copy()
            logging.debug("Use our default input stack signature instead: %s." % start_stack)
        else: 
            start_stack = sig_in.copy()

        # We're matching the start_stack against our input stack.
        match_stack = self.sig.stack_in.copy()
        matches : bool = True

        if len(self.sig.stack_in) > len(start_stack):
            logging.error("Input stack underrun! Match target len=%s:%s > match candidate len%s:%s" % (len(self.sig.stack_in), self.sig.stack_in, len(start_stack), start_stack) )
            raise Exception("Stack Underrun!")

        if len(self.words) == 0 and not force_composite:
            # This is a primitive operation. Just consume, adjust for stack effect.
            for i in range(len(match_stack)):
                match = match_stack.pop()
                test = start_stack.pop()
                logging.debug("Testing match:%s against test:%s." % (match, test))

                # Upgrade "Any" types if present.
                if match.stype == "Any":
                    logging.debug("Upgrading match 'Any' to test: %s." % test)
                    match = test 
                elif test.stype == "Any":
                    logging.debug("Upgrading test 'Any' to match: %s." % match)
                    test = match

                # Check against value if match has a value!
                if match.value is not None:
                    matches = (match.value == test.value)
                if not matches: 
                    msg = "match.value(%s) != test.value(%s)!" % (match.value, test.value)
                    logging.debug(msg)
                    raise SigValueTypeMismatchException("Value mis-match! %s" % msg)

                # Check if the types match.
                matches = (match.stype == test.stype)
                if not matches: 
                    msg = "match.stype(%s) != test.stype(%s)!" % (match.stype, test.stype)                
                    logging.debug(msg)
                    raise SigValueTypeMismatchException("Type mis-match! %s" % msg)
            
            # Tack on the output stack effect that we're claiming.            
            for i in self.sig.stack_out.contents():
                logging.debug("adding output type:%s" % i)
                start_stack.push(i)

            logging.debug("Returning following stack effect for primitive word: %s, matches = %s." % (start_stack, matches))
            return start_stack, matches

        # We're composite word so walk through the stack effect for each one.
        logging.debug("Composite word so walking through implementation.")
        last_word : Operation
        for word in self.words:
            if not matches:
                logging.error("This probably isn't possible. Broke match on word: %s." % last_word)
                raise Exception("Stack mis-match for composite word!")
            last_word = word
            start_stack, matches = word.check_stack_effect(start_stack)

        return start_stack, matches




Op_list = List[Operation]

Op_map = List[Tuple[Sequence["StackObject"],Operation]]




def op_nop(c: "AF_Continuation") -> None:
    pass   



