import unittest
import io
from copy import deepcopy

from stack import Stack
from interpret import *

class TestInterpreter(unittest.TestCase):

    def setUp(self) -> None:
        self.stack = Stack()
        self.save_types = deepcopy(Type.types)

    def tearDown(self) -> None:
        Type.types = deepcopy(self.save_types)

    def testMakeAtom(self) -> None:
        code = """
        Junk Junk
        """
        stack = interpret(self.stack, io.StringIO(code))

        assert stack.tos().value == "Junk"
        assert stack.tos().type == TAtom 
        assert stack.depth() == 2

        # Pretend to be stdin.
        stack = interpret(self.stack, io.StringIO(code), "stdin")
        assert stack.depth() == 4

    def testInterpretIntOps(self) -> None:
        code = "14 int 28 int +"
        stack = interpret(self.stack, io.StringIO(code))
        assert stack.tos().value == 42
        stack = interpret(stack, io.StringIO("17 int -"))
        assert stack.tos().value == 25
        stack = interpret(stack, io.StringIO("5 int /"))
        assert stack.pop().value == 0 # Our remainder
        assert stack.tos().value == 5
        stack = interpret(stack, io.StringIO("3 int *"))
        assert stack.tos().value == 15

    def testInterpretBoolOps(self) -> None:
        code = "14 int 28 int 2dup <"
        stack = interpret(self.stack, io.StringIO(code))
        assert stack.pop().value is True
        stack = interpret(stack, io.StringIO("2dup <="))
        assert stack.pop().value is True
        stack = interpret(stack, io.StringIO("2dup =="))
        assert stack.pop().value is False
        stack = interpret(stack, io.StringIO("2dup >"))
        assert stack.pop().value is False
        stack = interpret(stack, io.StringIO("2dup >="))
        assert stack.pop().value is False
        stack = interpret(stack, io.StringIO("2dup !="))
        assert stack.pop().value is True
        stack = interpret(stack, io.StringIO("dup dup =="))
        assert stack.pop().value is True
        stack = interpret(stack, io.StringIO("dup dup !="))
        assert stack.pop().value is False
        stack = interpret(stack, io.StringIO("True bool"))
        assert stack.tos().value is True
        stack = interpret(stack, io.StringIO("not"))
        assert stack.pop().value is False

    def testInterpretInferenceBoolOps(self) -> None:
        stack = interpret(self.stack, io.StringIO("False bool"))
        assert stack.tos().value is False
        stack = interpret(stack, io.StringIO("False =="))
        assert stack.tos().value is True

    def testOverloadingBoolCtor(self) -> None:
        TBool.register_ctor('bool',op_bool,[TBool])
        stack = interpret(self.stack, io.StringIO("True bool"))
        assert stack.tos().type == TBool
        assert stack.tos().value is True
        stack = interpret(stack, io.StringIO("bool"))
        assert stack.tos().type == TBool
        assert stack.pop().value is True



