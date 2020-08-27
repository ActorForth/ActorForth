"""
aftype.property - the basis of the ActorForth Type System

INTRO 4 : This is mainly here to satisfy module dependencies in the python
          implementation and serve as a pre-requisite for af_types/__init__.py.
"""

import logging
from typing import Callable, List, Optional, Any
from dataclasses import dataclass

from stack import Stack

"""
INTRO 4.1: A Location refers to the position in the source filestream where the 
           Symbol was discovered.
"""
@dataclass(frozen = True)
class Location:
    filename : str = "Unknown"
    linenum : int = 0
    column : int = 0

"""
INTRO 4.2: A Symbol is the string representation of the token plus its Location
           as identified by the Parser. The Interpreter pulls the token and
           Location data from the Parser to create the Symbol. The Interpreter
           then updates the Symbol in the Continuation and executes it. 
           (See INTRO 2.4 in interpret.py)
"""
@dataclass(order = True)
class Symbol:
    s_id : str = ''
    location : Location = Location()
    
    @property
    def size(self) -> int:
        return len(self.s_id)

    def __eq__(self, symbol) -> bool:
        if type(symbol) == Symbol:
            return symbol.s_id == self.s_id
        return False


@dataclass
class AF_Type:
    name : str

    def is_generic(self) -> bool:
        # Any types names "Any" or that start with underscore, '_', refer to 
        # generic types and will share the same word lookup.
        result = self.name == "Any" or self.name.startswith('_')
        #print("is_generic for %s is: %s." % (self.name, result))
        return result

"""
INTRO 4.3 : Stacks strictly contain StackObjects. Every StackObject
            consists of the object's value and it's Type.

           Continue to af_types/__init__.py for INTRO stage 5.             
"""
@dataclass
class StackObject:
    stype: AF_Type
    value: Any = None

    def __str__(self):
      if self.value is not None:
        return "SObject(t='%s',v='%s')" % (self.stype, self.value)
      return "SObject(t='%s')" % self.stype

    def __repr__(self):
      return self.__str__()


@dataclass 
class AF_Continuation:
    stack : Stack
    rstack : Stack
    symbol : Symbol 
    op : Any = None # Becomes an Operation in Continuation

    prompt: str = "ok: "
    
    debug : bool = False
    cdepth : int = 0        # Depth of calls for debug tab output.
    log : logging.Logger = logging.getLogger()
