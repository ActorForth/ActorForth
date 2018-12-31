import unittest

from parser import Parser

test_file_name = "samples/fib.a4"

class TestParser(unittest.TestCase):

    def test_default_parser(self) -> None:
        p = Parser()
        assert p.linenum is 0

    def test_open_file(self) -> None:
        p = Parser(test_file_name).lines()
        o = open(test_file_name).readlines()
        for x,y in zip(p,o):
            assert x == y

