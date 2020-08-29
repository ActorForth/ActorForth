import unittest
import sys
import io

from parser import Parser, Type

test_file_name = "samples/aspire/fib.a4"
test_token_list = [('fib', 1, 1),
 (':', 1, 5),
 ('nat', 1, 7),
 ('->', 1, 11),
 ('nat', 1, 14),
 ('.', 1, 17),
 ('fib', 2, 5),
 (':', 2, 9),
 ('0', 2, 11),
 ('->', 2, 13),
 ('0', 2, 16),
 ('.', 2, 17),
 ('fib', 3, 5),
 (':', 3, 9),
 ('1', 3, 11),
 ('->', 3, 13),
 ('1', 3, 16),
 ('.', 3, 17),
 ('fib', 4, 5),
 (':', 4, 9),
 ('_', 4, 11),
 ('->', 4, 13),
 ('nat', 4, 16),
 (';', 4, 19),
 ('dup', 5, 9),
 ('1', 6, 9),
 ('-', 6, 11),
 ('fib', 7, 9),
 ('swap', 8, 9),
 ('2', 9, 9),
 ('-', 9, 11),
 ('fib', 10, 9),
 ('+', 11, 9),
 ('.', 11, 10)]


class TestParser(unittest.TestCase):

    def test_default_parser(self) -> None:
        p = Parser()
        assert p.filename == ''

    def test_open_file(self) -> None:
        p = Parser(test_file_name)
        assert p.filename == test_file_name
        p.reset()
        assert not p.filename

    def test_open_handle(self) -> None:
        handle = sys.stdin
        filename = "stdin"

        p = Parser()
        p.open_handle(handle, filename)

        assert p.filename == filename
        assert p.file_handle == handle 
        p.reset()
        assert not p.filename

    def test_tokens(self) -> None:
        p = Parser(test_file_name)
        results = zip(p.tokens(), test_token_list)
        for a,b in results:
            assert a==b     

    def test_parse_file_with_ending_white_space(self) -> None:
        p = Parser("samples/square.a4")
        [t for t in p.tokens()]
        assert p.filename == "samples/square.a4"


    def test_simple_string(self) -> None:
        text =  """
"This is one string"
This is four tokens"Bonus string"
                """
        p = Parser()
        p.open_handle(io.StringIO(text))
        tokens = [t for t in p.tokens()]
        results = [('"This is one string"',2,1),
                    ("This",3,1), ("is",3,6), ("four",3,9), 
                    ("tokens",3,14), ('"Bonus string"',3,20)]
        test = [a==b for a,b in zip(tokens,results)]
        #print("")
        #print(tokens)
        #print(results)
        assert all(test)

    def test_multi_line_string(self) -> None:
        test =  """
"This
    is
        four
            lines." """
        p = Parser()
        p.open_handle(io.StringIO(test))
        i = iter(p.tokens())
        t = next(i)[0]
        #print(test)
        #print(t)
        assert t == test.strip()

    def test_comment(self) -> None:
        text =  """
Code
"A String"
# This is a comment
# This is a comment with a "string"!
Code# Comment
                """
        p = Parser()
        p.open_handle(io.StringIO(text))
        results = [("Code",2,1), ('"A String"',3,1), ("# This is a comment",4,1),
                    ('# This is a comment with a "string"!',5,1), ("Code",6,1),
                    ("# Comment",6,5)]
        tokens = [t for t in p.tokens()]
        #print("")
        #print(tokens)
        #print(results)
        test = [a==b for a,b in zip(tokens,results)]
        assert all(test)
