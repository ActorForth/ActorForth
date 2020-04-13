import unittest

from continuation import Continuation, Stack

from af_types import StackObject, Type, TypeSignature, \
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
        # Clear up all the types.
        Type.types = {}
        the_types = ["Any","CodeCompile","Parm1","Test"]
        for t in the_types:
            Type.types[t] = []
            Type.ctors[t] = []
            x = Type(t)

        Type.register_ctor("Test",Operation('nop', TOp), [TParm1])


    def test_match_in(self) -> None:
        empty_sig = TypeSignature([],[])
        s = Stack()
        assert empty_sig.match_in(s)

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
        assert Type.find_ctor("Test",l).the_op == TOp

        # Execute the lambda so we get full code coverage.
        assert TOp("fake_stack") == "fake_stack"

        l = [TAny]
        assert Type.find_ctor("Test",l).the_op == TOp

        l = [TTest]
        assert Type.find_ctor("Test",l) == None

        l = []
        assert Type.find_ctor("Test",l) == None

    def test_op_with_type_signature(self) -> None:

        stack = Stack()
        cont = Continuation(stack)
        cont.stack.push(StackObject("tparm", TParm1))
        Type.add_op(Operation("test", lambda cont: 42, sig=TypeSignature([TParm1],[]) )) #, "Test")

        print_words()

        op, found = Type.op("test", cont ) #, "Test")

        assert found
        assert op.sig == TypeSignature([TParm1],[])

        op, found = Type.op("not found", cont)
        assert not found

    def test_op_with_wrong_type_signature(self) -> None:
        stack = Stack()
        stack.push(StackObject("tparm", TTest))

        Type.add_op(Operation("test", op_print, sig = TypeSignature([TParm1],[])))

        with self.assertRaises( Exception ):
            Type.op("test", stack)
            # Never get here -> print("op='%s', sig='%s', flag='%s', found='%s'" % (op,sig,flag,found))
        
    def test_op_with_no_type_signature(self) -> None:
        def stack_fun(s: Stack) -> None:
            s.push(42)

        s = Stack()        
        c = Continuation(s)    

        Type.add_op(Operation("test", stack_fun, sig = TypeSignature([],[]) ) )

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
        assert item.type == TAtom 

    def test_op_print(self) -> None:
        self.c.stack.push(StackObject("test", TAtom))
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
        self.s.push(StackObject("test", TAtom))
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

