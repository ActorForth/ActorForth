import unittest
from copy import deepcopy

from continuation import Continuation, Stack

from aftype import StackObject
from af_types import Type, TypeSignature, \
                    make_atom, TAtom

from af_types.af_int import *
from af_types.af_bool import *
from af_types.af_any import *
from af_types.af_branch import *

TTest = Type("Test")
TParm1 = Type("Parm1")
TAny = Type("Any")


def make_ratom(c: AF_Continuation) -> None:
	c.rstack.push(StackObject(value=c.symbol.s_id,stype=TAtom))

class TestGenericReturnStackOps(unittest.TestCase):

    def setUp(self) -> None:
        self.s = Stack()
        self.r = Stack()
        self.c = Continuation(self.s, symbol = Symbol("Unknown", Location()))
        self.c.rstack = self.r

    def test_make_ratom(self) -> None:
        self.c.symbol.s_id = "test"
        make_ratom(self.c)
        item = self.c.rstack.pop()
        assert item.value == "test"
        assert item.stype == TAtom

    def test_op_rdup(self) -> None:
        self.test_make_ratom()
        op_rdup(self.c)
        item1 = self.c.rstack.pop()
        item2 = self.c.rstack.pop()
        assert item1 == item2

    def test_op_rswap(self) -> None:
        self.c.symbol.s_id = "first"
        make_ratom(self.c)
        self.c.symbol.s_id = "second"
        make_ratom(self.c)
        op_rswap(self.c)
        item1 = self.c.rstack.pop()
        item2 = self.c.rstack.pop()
        assert item1.value == "first"
        assert item2.value == "second"

    def test_op_rdrop(self) -> None:
        self.r.push(StackObject(stype=TAtom, value="test"))
        op_rdrop(self.c)
        assert self.c.rstack.depth() == 0

    def test_op_2rdup(self) -> None:
        self.c.symbol.s_id = "first"
        make_ratom(self.c)
        self.c.symbol.s_id = "second"
        make_ratom(self.c)
        op_2rdup(self.c)
        assert self.c.rstack.depth() == 4
        item1 = self.c.rstack.pop()
        item2 = self.c.rstack.pop()
        item3 = self.c.rstack.pop()
        item4 = self.c.rstack.pop()
        assert item1.value == item3.value
        assert item2.value == item4.value
        assert item1.value != item2.value
        assert self.c.rstack.depth() == 0

    def test_rstack(self) -> None:
    	op_rstack(self.c)
    	make_ratom(self.c)
    	op_rstack(self.c)
    	self.c.prompt = None
    	op_rstack(self.c)

    def test_move_between_stacks(self) -> None:
    	self.c.symbol.s_id = "test"
    	make_ratom(self.c)
    	assert self.c.rstack.tos().value == "test"
    	assert self.c.stack.depth() == 0
    	op_mov_to_dstack(self.c)
    	assert self.c.stack.tos().value == "test"
    	assert self.c.rstack.depth() == 0
    	op_mov_to_rstack(self.c)
    	assert self.c.rstack.tos().value == "test"
    	assert self.c.stack.depth() == 0
