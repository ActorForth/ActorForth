import unittest

from parser import Parser, Symbol, Location, Type

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

    def test_parse_file_with_ending_white_space(self) -> None:
        p = Parser("samples/square.a4")
        [t for t in p.tokens()]
        assert p.filename is "samples/square.a4"


class TestLocation(unittest.TestCase):

    def test_empty_location(self) -> None:
        l = Location()
        assert l.filename is "Unknown"
        assert l.linenum is 0
        assert l.column is 0

    def test_normal_location(self) -> None:
        l = Location("fib.a4", linenum=1, column=2)
        assert l.filename is "fib.a4"
        assert l.linenum is 1
        assert l.column is 2

class TestSymbol(unittest.TestCase):

    def test_normal_symbol_without_location(self) -> None:
        s = Symbol("fib", Location(), Type("Unknown"))
        assert s.s_id is "fib"
        assert s.size is 3
        assert s.type == Type("Unknown")
        assert s.location.filename is "Unknown"

        x = Symbol("fib", Location(), Type("SomethingElse"))
        assert s == "fib"
        assert s == x

    def test_normal_symbol_with_location(self) -> None:
        l = Location("fib.a4", linenum=10, column=4)    
        s = Symbol("fib", l, Type("Unknown"))
        assert s.location.filename is "fib.a4"
        assert s.location.linenum is 10
        assert s.location.column is 4
