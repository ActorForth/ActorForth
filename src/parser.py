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

    def tokens(self):

        def get_char():
            while True:
                last_char = self.file_handle.read(1)
                if not last_char:
                    break
                yield last_char

        token = ""
        token_column = 0
        linenum = 1
        column = 1
        white_space = False
        while True:
            for char in get_char():
                if char is '\n':
                    linenum += 1
                    column = 1
                    white_space = True

                elif char is ' ' or char is '\t':
                    column += 1
                    white_space = True

                # Punctuation doesn't need whitespace!
                elif char is '.' or char is ':':
                    column += 1
                    white_space = False

                    if token:
                        # Flush out the previous token.
                        yield token
                    # Start a new token
                    token = "".join(char)
                    token_column = column

                else:
                    column += 1
                    if white_space:
                        token_column = column
                    token += char
                    white_space = False
                    

                if token and white_space:
                    yield token
                    token = ""
            if token:
                yield token
            break










