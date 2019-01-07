#
#   types.py    -   Built in types for our language.
#
from typing import Iterable, Any, Union

def is_constexpr( exprs: Iterable ) -> bool:
    return all([i.is_constexpr() for i in exprs])

class Nat:

    def __init__(self, value : int) -> None:
        if Nat.isa(value):
            self.value = value
        else:            
            raise ValueError("'%s' is not a valid instance of the Nat type. Nats are positive integers only 0..Inf." % value)

    def isa( value : Any ) -> bool:
        return isinstance(value, int) and value >= 0

    def is_constexpr() -> bool:
        return False

class Literal:

    def __init__(self, value : Union[int, str]) -> None:
        if Literal.isa(value):
            self.value = value
        else: 
            raise ValueError("'%s' is not a valid instance of the Literal type. Literals are const strings or numbers only." % value)

    def isa( value : Any ) -> bool:
        return isinstance(value,  int) or isinstance(value, str)

    def is_constexpr() -> bool:
        return True        
