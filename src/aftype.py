from typing import Callable, List, Optional, Any
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


class AF_Type:
    pass

@dataclass 
class AF_Continuation:
    stack : Stack
    symbol : Optional[Symbol] = None
    op : Any = None # Becomes an Operation in Continuation



    debug : bool = False
    cdepth : int = 0        # Depth of calls for debug tab output.
