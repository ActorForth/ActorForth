import unittest

from parser import Parser

test_file_name = "samples/fib.a4"
test_token_list = [('fib', 1, 1),
 (':', 1, 5),
 ('nat', 1, 7),
 ('->', 1, 11),
 ('nat', 1, 14),
 ('.', 1, 17),
 ('fib', 2, 5),
 (':', 2, 9),
 ('0', 2, 11),
 (':', 2, 13),
 ('0', 2, 15),
 ('.', 2, 16),
 ('fib', 3, 5),
 (':', 3, 9),
 ('1', 3, 11),
 (':', 3, 13),
 ('1', 3, 15),
 ('.', 3, 16),
 ('fib', 4, 5),
 (':', 4, 9),
 ('_', 4, 11),
 (':', 4, 13),
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
        assert p.filename is ''

    def test_open_file(self) -> None:
        p = Parser(test_file_name)
        assert p.filename is test_file_name
        p.reset()
        assert not p.filename

    def test_tokens(self) -> None:
        p = Parser(test_file_name)
        results = zip(p.tokens(), test_token_list)
        for a,b in results:
            assert a==b     

