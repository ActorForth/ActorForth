import unittest
import io
from copy import deepcopy

from continuation import Continuation, Stack
from interpret import *

from aftype import StackObject
from af_types import Type, TypeSignature, \
                    make_atom, TAtom

TBool = Type("Bool")
TInt = Type("Int")


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
                debug on
                double : Int -> Int;
                    dup + .

                2 int double
                """
        assert self.execute(code) == 4

    def test_compile_double_literal(self) -> None:
        code =  """
                double : Int -> Int;
                    2 int * .

                2 int double
                """
        assert self.execute(code) == 4

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
        assert self.cont.stack.depth() == 2
        self.cont.stack.pop()
        assert self.cont.stack.depth() == 1
        assert self.cont.stack.tos().value == True

    def test_compile_double_drop(self) -> None:
        code =  """
                double : Int -> Int ;
                    2 int *.
                5 int double
                """
        assert self.execute(code) == 10

    def test_compile_no_input_no_output(self) -> None:
        code =  """
                noinputnooutput : -> ;
                stack .
                """
        stack = Stack()
        cont = Continuation(stack)
        cont = interpret(cont, io.StringIO(code))

        op, found = Type.op("noinputnooutput", cont ) #, "Test")

        assert found
        assert op.sig == TypeSignature([],[])

        op, found = Type.op("not found", cont)
        assert not found

    def test_compile_multi_input_multi_output(self) -> None:
        code =  """
                multiinput : Bool Int Int Int -> Int ;
                + == == .
                False bool 1 int 1 int 1 int
                """
        stack = Stack()
        cont = Continuation(stack)
        cont = interpret(cont, io.StringIO(code))

        op, found = Type.op("multiinput", cont ) #, "Test")

        assert found
        assert op.sig == TypeSignature([StackObject(stype=TBool),StackObject(stype=TInt),StackObject(stype=TInt),StackObject(stype=TInt)],
                                [StackObject(stype=TInt)])

        op, found = Type.op("not found", cont)
        assert not found

        