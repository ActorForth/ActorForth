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

@dataclass(frozen = True)
class Symbol:
    class Type(Enum):
        Unknown = 0
        Function = 1
        Constant = 2
        Expression = 3

    name : str
    location : Location = Location()
    type : Type = Type.Unknown
    
    @property
    def size(self):
        return len(self.name)

    def __eq__(self, name = None, symbol = None):
        if name:
            return name == self.name
        return symbol.name == self.name

