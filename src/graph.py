#
#   graph.py    - Call graph/AST for our language.
#
from typing import Dict, List, Tuple, Callable
from enum import Enum
from dataclasses import dataclass

from stack import Stack

@dataclass(frozen = True)
class Location:
    filename : str = "Unknown"
    linenum : int = 0
    column : int = 0

@dataclass(frozen = True, order = True)
class Type:    
    name: str


class Atom(Type):
    forth_dict : List[Tuple[str,Callable[[Stack, str],None]]] = []


class Int(Type):
    forth_dict : List[Tuple[str,Callable[[Stack, str],None]]] = []


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

