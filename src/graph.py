#
#   graph.py    - Call graph/AST for our language.
#
from typing import Dict
from enum import Enum

class Location:

    def __init__(self, filename = None, linenum: int = 0, column: int = 0) -> None:
        self.filename = filename
        self.linenum = linenum
        self.column = column
        if not filename:
            self.filename = "Unknown"

class Symbol:

    Globals : Dict[str, "Symbol"] = {}

    class Type(Enum):
        Unknown = 0
        Function = 1
        Constant = 2
        Expression = 3

    def __init__(self, name: str, location: Location = None) -> None: 
        self.name = name
        self.size = len(name)
        self.type = Symbol.Type.Unknown
        self.location = location
        if not location:
            self.location = Location()




