from enum import Enum


class Type(Enum):
    Unknown = 0
    Function = 1
    Constant = 2
    Expression = 3

class Symbol:
    def __init__(self, name, location ): 
        self.name = name
        self.size = len(name)
        self.type = Type.Unknown
        self.location = location
        if not location:
            self.location = Location()


class Location:

    def __init__(self, filename = "Unknown", linenum = 0, column = 0):
        self.filename = filename
        self.linenum = linenum
        self.column = column

