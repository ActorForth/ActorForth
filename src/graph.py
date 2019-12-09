#
#   graph.py    - Call graph/AST for our language.
#
from typing import Dict
from enum import Enum
from dataclasses import dataclass

@dataclass(frozen = True)
class Location:
    filename : str = "Unknown"
    linenum : int = 0
    column : int = 0

@dataclass(frozen = True, order = True)
class Type:    
    name: str

@dataclass(frozen = True, order = True)
class Atom(Type):
    pass

@dataclass(frozen = True, order = True)
class Int(Type):
    pass


@dataclass(order = True)
class Symbol:
    s_id : str
    location : Location 
    type : Type 
    
    @property
    def size(self):
        return len(self.s_id)

    def __eq__(self, symbol = None):
        if type(symbol) is Symbol:
            return symbol.s_id == self.s_id
        return symbol == self.s_id

