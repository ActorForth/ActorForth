import unittest

from graph import Symbol, Location, Type

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

    def test_normal_symbol_with_location(self) -> None:
        l = Location("fib.a4", linenum=10, column=4)    
        s = Symbol("fib", l, Type("Unknown"))
        assert s.location.filename is "fib.a4"
        assert s.location.linenum is 10
        assert s.location.column is 4
