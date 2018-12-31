import unittest

from parser import Parser

class TestParser(unittest.TestCase):

    def test_default_parser(self) -> None:
        p = Parser()
        assert p.linenum is 0