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

from aftype import AF_Type, AF_Continuation, StackObject, Symbol

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
            # Upgrade Generic types to whatever they're being paired with.
            logging.debug("in_s type is '%s' : %s." % (type(in_s), in_s) )
            logging.debug("m_s type is '%s' : %s." % (type(m_s),m_s) )

            match_type : AF_Type = m_s.stype
            if match_type.is_generic():
                m_s.stype = in_s.stype
            elif in_s.stype.is_generic():
                in_s.stype = match_type

            assert m_s.stype == in_s.stype, "Error! Input Type '%s' not equivalent to Sig Type '%s' for In Stack = %s matched with Sig %s." % (in_s, m_s, self.stack_in, sig)

            ### TODO: Confirm stack content here!!!

            result_sig.insert(0,m_s)
        return result_sig


    # Used by the runtime interpreter to check for mathing types for words.
    def match_in(self, stack: Stack) -> bool:
        logging.debug("match_in in_s=%s, matching against stack=%s" % (self.stack_in, stack))
        try:
            result = self.map_from_input_sig(stack.contents())
            logging.debug("match_in returns True.")
            return True
        except AssertionError:
            logging.debug("match_in returns False.")
            return False


    def __str__(self) -> str:
        out = "TSig(["
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
        out += "])"
        return out

    def __repr__(self):
      return self.__str__()        

    def __eq__(self, s : object) -> bool:
        if not isinstance(s, TypeSignature): return NotImplemented  
        return (self.stack_in == s.stack_in) and (self.stack_out == s.stack_out)

    def __lt__(self, s: object) -> bool:
        if not isinstance(s, TypeSignature): return NotImplemented 
        # Longest stack_in comes first.
        if self.stack_in.depth() > s.stack_in.depth(): return True
        if self.stack_in.depth() < s.stack_in.depth(): return False

        # Go ahead and eliminate equal stacks early on as a likely case.
        if self.stack_in == s.stack_in: return False

        # Stacks with the most TypeValues (prioritized top to bottom) come first.
        us = self.stack_in.copy()
        them = s.stack_in.copy()
        while us.depth():
            i = us.pop()
            j = them.pop()
            if i.s_type < j.stype: return True
            if i.s_type > j.stype: return False
            if i.value is None and j.value is None: continue
            if i.value is not None and j.value is None: return True
            if i.value is None and j.value is not None: return False

        # We only get here if all the types match and have values.
        # So we're left to comparing values.
        us = self.stack_in.copy()
        them = s.stack_in.copy()
        while us.depth():
            i = us.pop()
            j = them.pop()
            if i.value < j.value : return True
            if i.value > j.value : return False

        # We should never get here because we already checked for
        # equal stacks earlier!
        return False


Op_name = str
Operation_def = Callable[["AF_Continuation"],None]

class Operation:

    def __init__(self, name: Op_name, op: Operation_def, words: List["Operation"] = None, sig: TypeSignature = None, symbol: Symbol = None) -> None:
        self.name = name
        self.the_op : Operation_def = op
        self.words : List["Operation"] = words or []
        self.sig : TypeSignature = sig or TypeSignature([],[])
        self.symbol : Symbol = symbol or Symbol()

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
        #result += " words=%s" % str(self.words)
        #result += " from=%s" % self.symbol.location
        result += "}"
        return result

    def __repr__(self) -> str:
        return self.__str__()

    def __lt__(self, o: object) -> bool:
        if not isinstance(o, Operation): return NotImplemented        
        if self.name < o.name: return True
        return self.sig < o.sig

    def short_name(self) -> str:
        return self.name

    def check_stack_effect(self, context : Optional[ Stack ] = None, force_composite : bool = False) -> Tuple[ Stack, bool ]:
        """
        force_composite is used for compiling new composite words that may not yet have a word 
        in their word list so would otherwise appear as primitive words and return the final 
        stack effect rather than the starting one which is appropriate when compiling.
        """
        logging.debug("op: %s with context = %s." % (self, context) )
        start_stack : Stack
        match_stack : Stack

        # The start_stack is what we're trying to match with.
        if context is None:
            start_stack = self.sig.stack_in.copy()
            logging.debug("Use our default input stack signature instead: %s." % start_stack)
        else: 
            start_stack = context.copy()

        # We're matching the start_stack against our input stack.
        match_stack = self.sig.stack_in.copy()
        matches : bool = True

        if len(self.sig.stack_in) > len(start_stack):
            logging.error("Input stack underrun! Match target len=%s:%s > match candidate len%s:%s" % (len(self.sig.stack_in), self.sig.stack_in, len(start_stack), start_stack) )
            raise Exception("Stack Underrun!")

        generic_map : Dict["AF_Type", "StackObject"] = {}

        if len(self.words) == 0 and not force_composite:
            logging.debug("This is a primitive operation. Just consume, adjust for stack effect.")
            for i in range(len(match_stack)):
                match = match_stack.pop()
                test = start_stack.pop()
                logging.debug("Testing match:%s against test:%s." % (match, test))

                # Fixup any Generic mappings.
                o : StackObject 
                for o in [match, test]:
                    if o.stype.is_generic() and generic_map.get(o.stype):
                        o = generic_map[o.stype]

                # Upgrade Generic types if present.
                for (m,t) in [(match, test), (test,match)]:
                    if m.stype.is_generic():
                        logging.debug("Upgrading %s to test: %s." % (m,t))
                        generic_map[m.stype] = t
                        m = t

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
                      
            for o in self.sig.stack_out.contents():
                # See if we have a generic we need to specialize.
                # NOTE - we may need to loop over this until .get returns nothing
                #        in case there's more than one level of mapping. 
                #        If so beware of infinite loops!
                if generic_map.get(o.stype):
                    logging.debug("Specializing %s to %s." % (o,generic_map[o.stype]))
                    o = generic_map[o.stype]
                logging.debug("adding output:%s" % o)
                start_stack.push(o)

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

        logging.debug("Returning output stack: %s with matches = %s." % (start_stack, matches))
        return start_stack, matches


Op_list = List[Operation]

Op_map = List[Tuple[Sequence["StackObject"],Operation]]

def op_nop(c: "AF_Continuation") -> None:
    pass   
