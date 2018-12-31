#
#   parser.py   -   Parser for our language.
#
from typing import List

class Parser:

    def __init__(self, filename: str = None) -> None:
        self.reset()
        if filename:
            self.open(filename)

    def reset(self):
        self.linenum = 0
        self.column = 0
        self.filename = str("")
        self.file_handle = None
        self.content = []
        return self

    def open(self, filename: str):
        self.reset()
        self.filename = filename
        self.file_handle = open(filename)
        self.content = self.file_handle.readlines()
        return self

    def lines(self) -> List[str]:
        return self.content





