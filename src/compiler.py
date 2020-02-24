#
#   compiler.py     - Building new words/types for our language.
#

from typing import Dict, List, Tuple, Callable, Any, Optional
from dataclasses import dataclass

from af_types import *


# class TWordDefinition(Type):

#     def __init__(self, name: str) -> None:
#         self.sigs : List[ Tuple[TypeSignature, Operation] ] = []
#         self.complete : bool = False
#         super(TWordDefinition, self).__init__(name)
TWordDefinition = Type("WordDefinition")


def op_new_word(s: Stack) -> None:
    # Take an Atom, confirm that it's not already an active op,
    # and turn it into a new WordDefinition.
    itype = s.tos().type
    assert (itype == TAtom) or (itype == TWordDefinition), \
        "New words must be atoms or new word definitions. %s is a %s." % (s.tos().value, itype)

    op, sig, flags, found = Type.op(s.tos().value, s)
    assert not found, "Can't redefine an existing op." 

    if itype == TAtom:
        s.tos().type = TWordDefinition

Type.add_op(':', op_new_word, TypeSignature([TAtom],[TWordDefinition]))
Type.add_op(':', op_new_word, TypeSignature([TWordDefinition],[TWordDefinition]))
