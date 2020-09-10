import io
import sys
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
        assert self.cont.stack.tos().value.value == []

    def test_list_append(self) -> None:
        code = """
                Int list
                4 int append
                3 int append
                2 int append 
                1 int append
                """
        assert self.execute(code).stype == TList                
        assert self.cont.stack.tos().value.value == [4,3,2,1]
        assert self.cont.stack.tos().value.ltype == TInt

    def test_list_print(self) -> None:
        for prompt in ['',"ok: "]:
            self.cont.prompt = prompt
            self.test_list_append()
            code = "print"
            # Save old stdout.
            oldstdout = sys.stdout
            with io.StringIO() as out:
                sys.stdout = out
                self.execute(code)
                sys.stdout = oldstdout
                out.seek(0)
                output = out.read()
                assert output == '[4, 3, 2, 1]\n' + prompt

