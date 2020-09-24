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
        assert self.cont.stack.tos().value.name == "hash"

    def test_typedef_first_attribute_type(self) -> None:
        code = """
                MyType type
                    hash Bytes
                """    
        assert self.execute(code).stype == TTypeAttribute

    def test_typedef_completion(self) -> None:
        code = """
                MyType type
                    hash Bytes
                    .
                """
        self.execute(code)
        print("test_typedef_completion: %s" % self.cont.stack)
        assert self.cont.stack.depth() == 0
        assert Type("MyType").is_udt()

    def test_specialized_attribute(self) -> None:
        code = """
                MyType type
                    values Int list
                """
        self.execute(code)
        print("test_specialized_attribute: %s" % self.cont.stack)
        assert self.cont.stack.depth() == 2
        assert self.cont.stack.tos().stype == TTypeAttribute
        op_drop(self.cont)
        assert self.cont.stack.tos().stype == TTypeDefinition

    def test_specialized_typedef_completion(self) -> None:
        code = """
                MyType type
                    values Int list
                    .
                """
        self.execute(code)
        print("test_specialized_typedef_completion: %s" % self.cont.stack)
        assert self.cont.stack.depth() == 0
        assert Type("MyType").is_udt()        

    def test_two_attribute_completion(self) -> None:
        code = """
                MyType type
                    values Int list
                    active Bool
                    .
                """
        self.execute(code)
        print("test_two_attribute_completion: %s" % self.cont.stack)
        assert self.cont.stack.depth() == 0
        assert Type("MyType").is_udt()         

    def test_construct_typedef(self) -> None:
        self.test_two_attribute_completion()
        code = """
                Int list 
                17 int append
                True bool
                mytype               
                active
                """ 
        self.execute(code)
        #assert self.cont.stack.tos().stype == Type("MyType")
        assert self.cont.stack.tos().value == True

    def test_assign_reference(self) -> None:
        self.test_construct_typedef()
        code = """
                drop
                -> active
                False bool
                =
               """
        self.execute(code)
        assert self.cont.stack.tos().value == False
        #print("MyType dictionary: %s" % Type.types["MyType"])

    def test_broken_attributes(self) -> None:
        code = """
                MyType type
                    one Int
                    two Int
                    three Int.

                1 int 2 int 3 int
                mytype
                dup print
                three 3 int == "three != 3" assert
                two 2 int == "two != 2" assert
                one 1 int == "one != 1" assert 
               """
        self.execute(code)

    def test_broken_ref_attributes(self) -> None:
        code = """
                MyType type
                    one Int
                    two Int
                    three Int.

                1 int 2 int 3 int
                mytype
                dup print
                -> three 3 int == "three != 3" assert
                -> two 2 int == "two != 2" assert
                -> one 1 int == "one != 1" assert 
               """
        self.execute(code)