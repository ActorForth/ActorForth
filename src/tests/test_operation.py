import unittest

from operation import TypeSignature, Operation, op_nop, Stack

from af_types.af_bool import *
from af_types.af_int import *
from af_types.af_any import *

class OperationTests(unittest.TestCase):


    def test_simple_stack_effect(self) -> None:
        op = Operation("nop", op_nop, sig=TypeSignature([TInt],[TInt]))

        sig, match = op.check_stack_effect()
        assert sig == Stack([TInt])
        assert match == False