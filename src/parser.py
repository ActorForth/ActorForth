#
#   parser.py   -   Parser for our language.
#

from typing import List, TextIO, Optional, Iterator, Tuple
from io import StringIO


from af_types import Type 

class Parser:

    def __init__(self, filename: str = None) -> None:
        self.file_handle : Optional[TextIO] = None
        self.reset()
        if filename:
            self.open(filename)

    def reset(self) -> "Parser":
        self.filename = str("")
        if self.file_handle:
            self.file_handle.close()

        self.file_handle = None
        return self

    def open(self, filename: str) -> "Parser":
        self.reset()
        self.filename = filename
        self.file_handle = open(filename)
        assert self.file_handle
        return self

    def open_handle(self, handle : TextIO, filename: str = None) -> "Parser":
        self.reset()
        self.filename = filename or "Unknown"
        self.file_handle = handle
        assert self.file_handle
        return self

    def tokens(self) -> Iterator[Tuple[str, int, int]]:
        """
        Generator yielding tuples of 
          (token : str, linenum : int, token_column : int)
        """
        assert self.file_handle

        token = ""
        token_column = 0
        linenum = 1
        column = 1
        white_space = True
        quotes = False
        comment = False

        # TODO : we're not yet dealing with any quoted strings or escape characters.        
        while char := self.file_handle.read(1):

            # Handle comments
            if comment:
                if char == '\n':
                    column = 0
                    linenum += 1
                    # Comment Token ended via line feed. Send it.
                    yield (token, linenum, token_column)
                    token = ""
                    token_column = column
                    white_space = True
                    comment = False
                else:
                    token += char
                    column += 1
                continue
            elif char == '#':
                comment = True
                # Beginning a comment - which is always a new token.
                if token:
                    # Token ended via reserved punctuation. Send it.
                    yield (token, linenum, token_column)
                token = "".join(char)
                column += 1
                token_column = column
                white_space = False
                continue

            # Handle quotes
            if char == '"':
                column += 1
                white_space = False
                if quotes:
                    # Ending a string.
                    token += char
                    yield (token, linenum, token_column)
                    token = ""
                else:
                    # Beginning a string - which is always a new token.
                    if token:
                        # Token ended via reserved punctuation. Send it.
                        yield (token, linenum, token_column)
                    token = "".join(char)
                    token_column = column
                quotes = not quotes
                continue
            elif quotes:
                # While quotes are on we'll take every char there is.
                column += 1
                token += char
                if char == '\n':
                    column = 0
                    linenum += 1
                continue

            # Handle white space first.
            if char == ' ' or char == '\t' or char == '\n':
                column += 1 # \n will reset column later.
                # Tabs are assumed to occur on every 4th character
                # for purposes of column counting.
                if char == '\t': column += 4 - (column % 4)
                white_space = True

            # Punctuation doesn't need whitespace!
            elif char == '.' or char == ':' or char == ';':
                white_space = False

                if token:
                    # Token ended via reserved punctuation. Send it.
                    yield (token, linenum, token_column)
                # Start a new token
                token = "".join(char)
                token_column = column
                column += 1

            # Add this regular character to the current token.
            else: 
                if white_space:
                    token_column = column
                column += 1
                token += char
                white_space = False
                
            if token and white_space: # Token complete. Send it.
                yield (token, linenum, token_column)
                token = ""

            if char == '\n':
                linenum += 1
                column = 1  # reset the column as promised.

        # Handle any incremental token that may be left over.
        if token:
            yield (token, linenum, token_column)                      
