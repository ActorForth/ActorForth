import unittest
import io
from copy import deepcopy

from continuation import Continuation, Stack
from interpret import *
from af_types.af_bool import *

class TestInterpreter(unittest.TestCase):

    def setUp(self) -> None:
        self.stack = Stack()
        self.cont = Continuation(self.stack)
        self.save_types = deepcopy(Type.types)
        self.save_ctors = deepcopy(Type.ctors)

    def tearDown(self) -> None:
        Type.types = deepcopy(self.save_types)
        Type.ctors = deepcopy(self.save_ctors)

    def testMakeAtom(self) -> None:
        code = """
        Junk Junk
        """
        cont = interpret(self.cont, io.StringIO(code))

        assert cont.stack.tos().value == "Junk"
        assert cont.stack.tos().type == TAtom 
        assert cont.stack.depth() == 2

        # Pretend to be stdin.
        cont = interpret(self.cont, io.StringIO(code), "stdin")
        assert cont.stack.depth() == 4

    def testInterpretIntOps(self) -> None:
        code = "14 int 28 int +"
        cont = interpret(self.cont, io.StringIO(code))
        assert cont.stack.tos().value == 42
        cont = interpret(cont, io.StringIO("17 int -"))
        assert cont.stack.tos().value == 25
        cont = interpret(cont, io.StringIO("5 int /"))
        assert cont.stack.pop().value == 0 # Our remainder
        assert cont.stack.tos().value == 5
        cont = interpret(cont, io.StringIO("3 int *"))
        assert cont.stack.tos().value == 15
        cont = interpret(cont, io.StringIO("0 int *"))
        assert cont.stack.tos().value == 0

    def testInterpretBoolOps(self) -> None:
        code = "14 int 28 int 2dup <"
        cont = interpret(self.cont, io.StringIO(code))
        assert cont.stack.pop().value is True
        cont = interpret(cont, io.StringIO("2dup <="))
        assert cont.stack.pop().value is True
        cont = interpret(cont, io.StringIO("2dup =="))
        assert cont.stack.pop().value is False
        cont = interpret(cont, io.StringIO("2dup >"))
        assert cont.stack.pop().value is False
        cont = interpret(cont, io.StringIO("2dup >="))
        assert cont.stack.pop().value is False
        cont = interpret(cont, io.StringIO("2dup !="))
        assert cont.stack.pop().value is True
        cont = interpret(cont, io.StringIO("dup dup =="))
        assert cont.stack.pop().value is True
        cont = interpret(cont, io.StringIO("dup dup !="))
        assert cont.stack.pop().value is False
        cont = interpret(cont, io.StringIO("True bool"))
        assert cont.stack.tos().value is True
        cont = interpret(cont, io.StringIO("not"))
        assert cont.stack.pop().value is False

    def testInterpretInferenceBoolOps(self) -> None:
        cont = interpret(self.cont, io.StringIO("False bool"))
        assert cont.stack.tos().value is False
        cont = interpret(cont, io.StringIO("False =="))
        assert cont.stack.tos().value is True

    def testOverloadingBoolCtor(self) -> None:
        Type.register_ctor('Bool',Operation('bool',op_bool),[TBool])
        cont = interpret(self.cont, io.StringIO("True bool"))
        assert cont.stack.tos().type == TBool
        assert cont.stack.tos().value is True
        cont = interpret(cont, io.StringIO("bool"))
        assert cont.stack.tos().type == TBool
        assert cont.stack.pop().value is True


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
        s = Symbol("fib", Location())
        assert s.s_id is "fib"
        assert s.size is 3
        assert s.location.filename is "Unknown"

        x = Symbol("fib", Location())
        assert s != "fib"
        assert s == x

    def test_normal_symbol_with_location(self) -> None:
        l = Location("fib.a4", linenum=10, column=4)    
        s = Symbol("fib", l)
        assert s.location.filename is "fib.a4"
        assert s.location.linenum is 10
        assert s.location.column is 4

