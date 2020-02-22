import unittest

from af_types import Stack, StackObject, Type, TypeSignature, \
                    make_atom, TAtom, op_print, op_dup, op_swap, \
                    op_drop, op_2dup

from af_types.af_int import *
from af_types.af_bool import *

TTest = Type("Test")
TParm1 = Type("Parm1")
TAny = Type("Any")

TOp = lambda stack : stack

TTest.register_ctor("nop", TOp, [TParm1])

class TestTypeSignature(unittest.TestCase):

    def test_match_in(self) -> None:
        s = Stack()
        s.push(StackObject(None, TParm1))
        sig = TypeSignature([TParm1],[])

        # We can do this twice because it doesn't consume the stack.
        assert sig.match_in(s)
        assert sig.match_in(s)

        s.push(StackObject(None, TTest))

        assert sig.match_in(s) == False

    def test_match_out(self) -> None:
        # match_out hasn't really been implemented yet.
        sig = TypeSignature([],[])

        s = Stack()
        s.push(StackObject(None, TParm1))

        assert sig.match_out(s)

    def test_find_ctor(self) -> None:
        l = [TParm1]
        assert TTest.find_ctor(l) == TOp

        l = [TAny]
        assert TTest.find_ctor(l) == TOp

        l = [TTest]
        assert TTest.find_ctor(l) == None

        l = []
        assert TTest.find_ctor(l) == None

    def test_op(self) -> None:
       Type.add_op("test", lambda stack: 42, TypeSignature([TParm1],[]) ) #, "Test")

       op, sig, flag, found = Type.op("test" ) #, "Test")

       assert found
       assert op(None) == 42
       assert sig == TypeSignature([TParm1],[])
       assert flag.immediate == False

       op, sig, flag, found = Type.op("not found")
       assert not found

class TestGenericTypeStuff(unittest.TestCase):       

    def setUp(self) -> None:
        self.s = Stack()

    def test_make_atom(self) -> None:
        op_name = "test"
        make_atom(self.s, op_name)
        item = self.s.pop()
        assert item.value == op_name
        assert item.type == TAtom 

    def test_op_print(self) -> None:
        self.s.push(StackObject("test", TAtom))
        op_print(self.s)
        assert self.s.depth() == 0

    def test_op_dup(self) -> None:
        self.test_make_atom()
        op_dup(self.s)
        item1 = self.s.pop()
        item2 = self.s.pop()
        assert item1 == item2
        
    def test_op_swap(self) -> None:
        make_atom(self.s, "first")
        make_atom(self.s, "second")
        op_swap(self.s)
        item1 = self.s.pop()
        item2 = self.s.pop()
        assert item1.value == "first"
        assert item2.value == "second"

    def test_op_drop(self) -> None:
        self.s.push(StackObject("test", TAtom))
        op_drop(self.s)
        assert self.s.depth() == 0

    def test_op_2dup(self) -> None:
        make_atom(self.s, "first")
        make_atom(self.s, "second")
        op_2dup(self.s)
        assert self.s.depth() == 4
        item1 = self.s.pop()
        item2 = self.s.pop()
        item3 = self.s.pop()
        item4 = self.s.pop()
        assert item1.value == item3.value
        assert item2.value == item4.value
        assert item1.value != item2.value
        assert self.s.depth() == 0

