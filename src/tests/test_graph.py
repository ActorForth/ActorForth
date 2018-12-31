import unittest

from graph import Symbol, Location

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
        s = Symbol("fib")
        assert s.name is "fib"
        assert s.size is 3
        assert s.type is Symbol.Type.Unknown
        assert s.location.filename is "Unknown"


