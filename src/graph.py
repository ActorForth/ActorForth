from enum import Enum


class Type(Enum):
    Unknown = 0
    Function = 1
    Constant = 2
    Expression = 3


class Location:

    def __init__(self, filename = "Unknown", linenum: int = 0, column: int = 0) -> None:
        self.filename = filename
        self.linenum = linenum
        self.column = column

class Symbol:
    def __init__(self, name: str, location: Location = None) -> None: 
        self.name = name
        self.size = len(name)
        self.type = Type.Unknown
        self.location = location
        if not location:
            self.location = Location()




