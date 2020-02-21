import unittest

from af_types import Stack, StackObject, Type, TypeSignature

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

