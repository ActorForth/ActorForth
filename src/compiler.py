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
TInputTypeSignature = Type("InputTypeSignature")
TOutputTypeSignature = Type("OutputTypeSignature")


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

def op_start_input_sig(s: Stack) -> None:
    # Works only there's a WordDefinition followed by an Atom on tos.
    # Creates a new TypeSignature, adds the first item
    # and pushes it to the stack.
    # Does NOT consume the WordDefinition.
    type_name = s.pop().value
    assert Type.types.get(type_name, False) is not False, \
        "%s is not a valid type name.\nValid types are: %s." % (type_name, [n for n in Type.types.keys()])
    print("Got valid type: %s" % type_name)
    sig = TypeSignature([Type(type_name)],[])
    s.push((sig,TInputTypeSignature))


Type.add_op(':', op_new_word, TypeSignature([TAtom],[TWordDefinition]))
Type.add_op(':', op_new_word, TypeSignature([TWordDefinition],[TWordDefinition]))

for type_name in Type.types.keys():
    Type.add_op(type_name, op_start_input_sig, TypeSignature([TWordDefinition],[TWordDefinition,TInputTypeSignature]), WordFlags(), "WordDefinition")




