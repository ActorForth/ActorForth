import unittest
import io
from copy import deepcopy

from continuation import Continuation, Stack
from interpret import *
from af_types.af_bool import *
from af_types.af_debug import *

class TestInterpreter(unittest.TestCase):

    def setUp(self) -> None:
        self.stack = Stack()
        self.cont = Continuation(self.stack)
        self.save_types = deepcopy(Type.types)
        self.save_ctors = deepcopy(Type.ctors)
        #op_debug(self.cont)
        #op_on(self.cont)

    def tearDown(self) -> None:
        Type.types = deepcopy(self.save_types)
        Type.ctors = deepcopy(self.save_ctors)

    def testMakeAtom(self) -> None:
        code = """
        Junk Junk
        """
        cont = self.cont.execute(interpret(self.cont, io.StringIO(code)))

        assert cont.stack.tos().value == "Junk"
        assert cont.stack.tos().stype == TAtom 
        assert cont.stack.depth() == 2

        # Pretend to be stdin.
        cont = self.cont.execute(interpret(self.cont, io.StringIO(code), "stdin"))
        assert cont.stack.depth() == 4

    def testInterpretIntOps(self) -> None:
        code = "14 int 28 int +"
        cont = self.cont.execute(interpret(self.cont, io.StringIO(code)))
        assert cont.stack.tos().value == 42
        cont = self.cont.execute(interpret(cont, io.StringIO("17 int -")))
        assert cont.stack.tos().value == 25
        cont = self.cont.execute(interpret(cont, io.StringIO("5 int /")))
        assert cont.stack.pop().value == 0 # Our remainder
        assert cont.stack.tos().value == 5
        cont = self.cont.execute(interpret(cont, io.StringIO("3 int *")))
        assert cont.stack.tos().value == 15
        cont = self.cont.execute(interpret(cont, io.StringIO("0 int *")))
        assert cont.stack.tos().value == 0

    def testInterpretBoolOps(self) -> None:
        code = "14 int 28 int 2dup <"
        cont = self.cont.execute(interpret(self.cont, io.StringIO(code)))
        assert cont.stack.pop().value is True
        cont = cont.execute(interpret(cont, io.StringIO("2dup <=")))
        assert cont.stack.pop().value is True
        cont = cont.execute(interpret(cont, io.StringIO("2dup ==")))
        assert cont.stack.pop().value is False
        cont = cont.execute(interpret(cont, io.StringIO("2dup >")))
        assert cont.stack.pop().value is False
        cont = cont.execute(interpret(cont, io.StringIO("2dup >=")))
        assert cont.stack.pop().value is False
        cont = cont.execute(interpret(cont, io.StringIO("2dup !=")))
        assert cont.stack.pop().value is True
        cont = cont.execute(interpret(cont, io.StringIO("dup dup ==")))
        assert cont.stack.pop().value is True
        cont = cont.execute(interpret(cont, io.StringIO("dup dup !=")))
        assert cont.stack.pop().value is False
        cont = cont.execute(interpret(cont, io.StringIO("True bool")))
        assert cont.stack.tos().value is True
        cont = cont.execute(interpret(cont, io.StringIO("not")))
        assert cont.stack.pop().value is False

    def testInterpretInferenceBoolOps(self) -> None:
        cont = self.cont.execute(interpret(self.cont, io.StringIO("False bool")))
        assert cont.stack.tos().value is False
        cont = self.cont.execute(interpret(cont, io.StringIO("False ==")))
        assert cont.stack.tos().value is True

    def testOverloadingBoolCtor(self) -> None:
        Type.register_ctor('Bool',Operation('bool',op_bool),[TBool])
        cont = self.cont.execute(interpret(self.cont, io.StringIO("True bool")))
        assert cont.stack.tos().stype == TBool
        assert cont.stack.tos().value is True
        cont = self.cont.execute(interpret(cont, io.StringIO("bool")))
        assert cont.stack.tos().stype == TBool
        assert cont.stack.pop().value is True


class TestLocation(unittest.TestCase):

    def test_empty_location(self) -> None:
        l = Location()
        assert l.filename == "Unknown"
        assert l.linenum == 0
        assert l.column == 0

    def test_normal_location(self) -> None:
        l = Location("fib.a4", linenum=1, column=2)
        assert l.filename == "fib.a4"
        assert l.linenum == 1
        assert l.column == 2


class TestSymbol(unittest.TestCase):

    def test_normal_symbol_without_location(self) -> None:
        s = Symbol("fib", Location())
        assert s.s_id == "fib"
        assert s.size == 3
        assert s.location.filename == "Unknown"

        x = Symbol("fib", Location())
        assert s != "fib"
        assert s == x

    def test_normal_symbol_with_location(self) -> None:
        l = Location("fib.a4", linenum=10, column=4)    
        s = Symbol("fib", l)
        assert s.location.filename == "fib.a4"
        assert s.location.linenum == 10
        assert s.location.column == 4

