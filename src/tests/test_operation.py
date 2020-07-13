import unittest

from operation import TypeSignature, Operation, op_nop, Stack


import logging
root_log = logging.getLogger()
root_log.setLevel(logging.DEBUG)

from af_types.af_bool import *
from af_types.af_int import *
from af_types.af_any import *

class OperationTests(unittest.TestCase):

    def setUp(self) -> None:
        pass

    def test_simple_stack_effect(self) -> None:
        op = Operation("nop", op_nop, sig=TypeSignature([StackObject(stype=TInt)],[StackObject(stype=TInt)]))

        sig, match = op.check_stack_effect()
        assert sig == Stack([StackObject(stype=TInt)])
        assert match == True

        sig, match = op.check_stack_effect(Stack([StackObject(stype=TBool), StackObject(stype=TInt)]))
        assert sig == Stack([StackObject(stype=TBool), StackObject(stype=TInt)])
        assert match == True

    def test_any_stack_effect(self) -> None:
        op = Operation("nop", op_nop, sig=TypeSignature([StackObject(stype=TAny)],[StackObject(stype=TInt)]))

        check_out = Stack([StackObject(stype=TInt)])

        sig, match = op.check_stack_effect()
        assert sig == check_out
        assert match == True

        sig, match = op.check_stack_effect(Stack([StackObject(stype=TBool)]))
        assert sig == check_out
        assert match == True

        sig, match = op.check_stack_effect(Stack([StackObject(stype=TAny)]))
        assert sig == check_out
        assert match == True

        sig, match = op.check_stack_effect(Stack([StackObject(stype=TBool), StackObject(stype=TInt)]))
        assert sig == Stack([StackObject(stype=TBool), StackObject(stype=TInt)])
        assert match == True
