import unittest
from copy import deepcopy

from continuation import Continuation, Stack

from aftype import StackObject
from af_types import Type, TypeSignature, \
                    make_atom, TAtom

from af_types.af_int import *
from af_types.af_bool import *
from af_types.af_any import *

TTest = Type("Test")
TParm1 = Type("Parm1")
TAny = Type("Any")

TOp = lambda stack : stack



class TestTypeSignature(unittest.TestCase):

    def setUp(self) -> None:
        self.stack = Stack()
        self.cont = Continuation(self.stack)
        self.save_types = deepcopy(Type.types)
        self.save_ctors = deepcopy(Type.ctors)
        # Clear up all the types.
        the_types = ["Any","CodeCompile","Parm1","Test"]
        for t in the_types:
            x = Type(t)

        Type.register_ctor("Test",Operation('nop', TOp), [StackObject(stype=TParm1)])

    def tearDown(self) -> None:
        Type.types = deepcopy(self.save_types)
        Type.ctors = deepcopy(self.save_ctors)

    def test_match_in(self) -> None:
        empty_sig = TypeSignature([],[])
        s = Stack()
        assert empty_sig.match_in(s)

        s.push(StackObject(stype=TParm1))
        sig = TypeSignature([StackObject(stype=TParm1)],[])

        # We can do this twice because it doesn't consume the stack.
        assert sig.match_in(s)
        assert sig.match_in(s)

        s.push(StackObject(stype=TTest))

        assert sig.match_in(s) == False


    def test_find_ctor(self) -> None:
        l = [StackObject(stype=TParm1)]
        assert Type.find_ctor("Test",l).the_op == TOp

        # Execute the lambda so we get full code coverage.
        assert TOp("fake_stack") == "fake_stack"

        l = [StackObject(stype=TAny)]
        assert Type.find_ctor("Test",l).the_op == TOp

        l = [StackObject(stype=TTest)]
        assert Type.find_ctor("Test",l) == None

        l = []
        assert Type.find_ctor("Test",l) == None

    def test_op_with_type_signature(self) -> None:

        stack = Stack()
        cont = Continuation(stack)
        cont.stack.push(StackObject(stype=TParm1, value="tparm"))
        make_word_context("test", lambda cont: 42, [TParm1])

        print_words()

        op, found = Type.op("test", cont ) #, "Test")

        assert found
        assert op.sig == TypeSignature([StackObject(stype=TParm1)],[])

        op, found = Type.op("not found", cont)
        assert not found

    def test_op_with_type_multi_input_signature(self) -> None:

        stack = Stack()
        cont = Continuation(stack)
        cont.stack.push(StackObject(stype=TParm1, value="tparm"))
        cont.stack.push(StackObject(stype=TParm1, value="tparm"))
        make_word_context("test", lambda cont: 42, [TParm1, TParm1])

        print_words()

        op, found = Type.op("test", cont ) #, "Test")

        assert found
        assert op.sig == TypeSignature([StackObject(stype=TParm1),StackObject(stype=TParm1)],[])

        op, found = Type.op("not found", cont)
        assert not found

    def test_op_with_type_multi_input_multi_output_signature(self) -> None:

        stack = Stack()
        cont = Continuation(stack)
        cont.stack.push(StackObject(stype=TTest, value="ttest"))
        cont.stack.push(StackObject(stype=TParm1, value="tparm"))
        cont.stack.push(StackObject(stype=TParm1, value="tparm"))

        make_word_context("test", lambda cont: 42, [TTest, TParm1, TParm1], [TTest, TParm1])

        print_words()

        op, found = Type.op("test", cont ) #, "Test")

        assert found
        assert op.sig == TypeSignature([StackObject(stype=TTest),StackObject(stype=TParm1),StackObject(stype=TParm1)],
                        [StackObject(stype=TTest),StackObject(stype=TParm1)])

        op, found = Type.op("not found", cont)
        assert not found

    def test_op_with_type_multi_output_signature(self) -> None:

        stack = Stack()
        cont = Continuation(stack)

        make_word_context("test", lambda cont: 42, [], [TTest, TParm1])

        print_words()

        op, found = Type.op("test", cont ) #, "Test")

        assert found
        assert op.sig == TypeSignature([],[StackObject(stype=TTest),StackObject(stype=TParm1)])

        op, found = Type.op("not found", cont)
        assert not found

    def test_op_with_wrong_type_signature(self) -> None:
        stack = Stack()
        stack.push(StackObject(stype=TTest, value="tparm"))

        make_word_context("test", op_print, [TParm1])

        with self.assertRaises( Exception ):
            Type.op("test", stack)
            # Never get here -> print("op='%s', sig='%s', flag='%s', found='%s'" % (op,sig,flag,found))

    def test_op_with_no_type_signature(self) -> None:
        def stack_fun(s: Stack) -> None:
            s.push(42)

        s = Stack()
        c = Continuation(s)

        make_word_context("test", stack_fun)
        op, found = Type.op("test", c)

        assert found
        assert op.sig == TypeSignature([],[])


class TestGenericTypeStuff(unittest.TestCase):

    def setUp(self) -> None:
        self.s = Stack()
        self.c = Continuation(self.s, symbol = Symbol("Unknown", Location()))

    def test_make_atom(self) -> None:
        self.c.symbol.s_id = "test"
        #op_name = "test"
        make_atom(self.c)
        item = self.c.stack.pop()
        assert item.value == "test"
        assert item.stype == TAtom

    def test_op_print(self) -> None:
        self.c.stack.push(StackObject(stype=TAtom, value="test"))
        op_print(self.c)
        assert self.c.stack.depth() == 0

    def test_op_dup(self) -> None:
        self.test_make_atom()
        op_dup(self.c)
        item1 = self.c.stack.pop()
        item2 = self.c.stack.pop()
        assert item1 == item2

    def test_op_swap(self) -> None:
        self.c.symbol.s_id = "first"
        make_atom(self.c)
        self.c.symbol.s_id = "second"
        make_atom(self.c)
        op_swap(self.c)
        item1 = self.c.stack.pop()
        item2 = self.c.stack.pop()
        assert item1.value == "first"
        assert item2.value == "second"

    def test_op_drop(self) -> None:
        self.s.push(StackObject(stype=TAtom, value="test"))
        op_drop(self.c)
        assert self.c.stack.depth() == 0

    def test_op_2dup(self) -> None:
        self.c.symbol.s_id = "first"
        make_atom(self.c)
        self.c.symbol.s_id = "second"
        make_atom(self.c)
        op_2dup(self.c)
        assert self.c.stack.depth() == 4
        item1 = self.c.stack.pop()
        item2 = self.c.stack.pop()
        item3 = self.c.stack.pop()
        item4 = self.c.stack.pop()
        assert item1.value == item3.value
        assert item2.value == item4.value
        assert item1.value != item2.value
        assert self.c.stack.depth() == 0
