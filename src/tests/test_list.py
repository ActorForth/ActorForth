import io
import unittest
from copy import deepcopy

from continuation import Continuation, Stack
from interpret import *

from aftype import StackObject
from af_types import Type, TypeSignature, \
                    make_atom, TAtom

from af_types.af_int import *
from af_types.af_bool import *
from af_types.af_any import *
from af_types.af_list import *

TTest = Type("Test")
TParm1 = Type("Parm1")
TAny = Type("Any")


class TestList(unittest.TestCase):

    def setUp(self) -> None:
        self.stack = Stack()
        self.cont = Continuation(self.stack)

    def execute(self, code) -> StackObject:
        self.cont.execute(interpret(self.cont, io.StringIO(code)))
        return self.cont.stack.tos()

    def test_list_ctor(self) -> None:
        code =  """
                Int list
                """
        assert self.execute(code).stype == TList
        assert self.cont.stack.tos().value.ltype == TInt


