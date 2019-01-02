import unittest

from parser import Parser

test_file_name = "samples/fib.a4"

class TestParser(unittest.TestCase):

    def test_default_parser(self) -> None:
        p = Parser()
        assert p.filename is ''

    def test_open_file(self) -> None:
        p = Parser(test_file_name)
        o = open(test_file_name)


