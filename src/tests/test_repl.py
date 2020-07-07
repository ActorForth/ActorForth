import unittest

from repl import *

class TestRepl(unittest.TestCase):

    def test_simple_code(self) -> None:
        code = afc("1 int 2 int +")
        assert do_repl("test", code) == 3
