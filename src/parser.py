#
#   parser.py   -   Parser for our language.
#
#from __future__ import annotations
from typing import List, TextIO, Optional

class Parser:

    def __init__(self, filename: str = None) -> None:
        self.reset()
        if filename:
            self.open(filename)

    def reset(self) -> "Parser":
        self.linenum = 0
        self.column = 0
        self.filename = str("")
        self.file_handle : Optional[TextIO] = None
        return self

    def open(self, filename: str) -> "Parser":
        self.reset()
        self.filename = filename
        self.file_handle : Optional[TextIO] = open(filename)
        return self






