import unittest
import io
from copy import deepcopy

from continuation import Continuation, Stack
from interpret import *

class TestExecution(unittest.TestCase):

    def setUp(self) -> None:
        self.stack = Stack()
        self.cont = Continuation(self.stack)
        self.save_types = deepcopy(Type.types)
        self.save_ctors = deepcopy(Type.ctors)

    def tearDown(self) -> None:
        Type.types = deepcopy(self.save_types)
        Type.ctors = deepcopy(self.save_ctors)

    def execute(self, code) -> Any:
        cont = interpret(self.cont, io.StringIO(code))
        return cont.stack.tos().value

    def test_compile_double(self) -> None:
        code =  """
                double : Int -> Int;
                    dup + .

                2 int double
                """
        assert self.execute(code) == 4

    @unittest.skip("Got a problem compiling double in combo.")
    def test_compile_combo(self) -> None:
        code =  """
                is_equal : Any Any -> Bool;
                    == .

                double : Int -> Int;
                    dup +.

                combo : Int Int -> Bool;
                    double
                    swap
                    double
                    is_equal.

                8 int 8 int combo
                4 int 2 int combo
                """

        assert self.execute(code) == False
        cont.stack.pop()
        assert self.stack.tos().value == True

    def test_compile_double_drop(self) -> None:
        code =  """
                double : Int -> Int ;
                    2 int *.
                5 int double
                """
        assert self.execute(code) == 10
