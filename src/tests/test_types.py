import unittest

from af_types import Stack, StackObject, Type, TypeSignature

TTest = Type("Test")
TParm1 = Type("Parm1")

TTest.register_ctor("nop",lambda stack : stack, [TParm1] )

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
