#
#   graph.py    - Call graph/AST for our language.
#
from typing import Dict
from enum import Enum
from dataclasses import dataclass

@dataclass
class Location:

    filename : str = "Unknown"
    linenum : int = 0
    column : int = 0

    #def __init__(self, filename = None, linenum: int = 0, column: int = 0) -> None:
    #    self.filename = filename
    #    self.linenum = linenum
    #    self.column = column
    #    if not filename:
    #        self.filename = "Unknown"

@dataclass
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

