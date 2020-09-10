import io
import sys
import unittest
from copy import deepcopy

from continuation import Continuation, Stack
from interpret import *

from aftype import StackObject
from af_types import Type, TypeSignature, TAtom

from af_types.af_int import *
from af_types.af_bool import *
from af_types.af_any import *
from af_types.af_environment import *
from af_types.af_usertype import *


TTest = Type("Test")
TParm1 = Type("Parm1")
TAny = Type("Any")
TBytes = Type("Bytes")


class TestUserType(unittest.TestCase):

    def setUp(self) -> None:        
        self.stack = Stack()
        self.cont = Continuation(self.stack)
        op_checkpoint(self.cont)

    def tearDown(self) -> None:    
        op_restore(self.cont)
           
    def execute(self, code) -> StackObject:
        self.cont.execute(interpret(self.cont, io.StringIO(code)))
        return self.cont.stack.tos()

    def test_typedef_begin(self) -> None:
        code =  """
                MyType type
                """
        assert self.execute(code).stype == TTypeDefinition
        assert self.cont.stack.tos().value.name == "MyType"

    def test_typedef_first_attribute_name(self) -> None:
        code = """
                MyType type
                    hash
                """        
        assert self.execute(code).stype == TTypeAttribute
        assert self.cont.stack.tos().value == "hash"

    def test_typedef_first_attribute_type(self) -> None:
        code = """
                MyType type
                    hash Bytes
                """
        #self.test_typedef_first_attribute_name()
        assert self.execute(code).stype == TTypeDefinition
        #print("\n\n%s\n\n" %self.cont.stack.tos())
        assert self.cont.stack.tos().value.values.get("hash") == TBytes

    def test_typedef_completion(self) -> None:
        code = """
                MyType type
                    hash Bytes.
                """
        self.execute(code)
        assert Type("MyType").is_udt()