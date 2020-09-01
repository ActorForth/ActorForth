import unittest
import io
from copy import deepcopy

from continuation import Continuation, Stack
from compiler import *
from interpret import *

from aftype import StackObject

from af_types.af_debug import *

TBool = Type("Bool")
TInt = Type("Int")
TAny = Type("Any")

def op_gen(result, pop_count):
    def op_test(c):
        for i in range(pop_count):
            c.stack.pop()
        c.stack.push(result)
    return op_test

class TestPatternMatching(unittest.TestCase):

    def setUp(self) -> None:
        self.stack = Stack()
        self.cont = Continuation(self.stack)
        self.save_types = deepcopy(Type.types)
        self.save_ctors = deepcopy(Type.ctors)

        self.zero_pattern = StackObject(stype=TInt, value=0)
        self.one_pattern = StackObject(stype=TInt, value=1)
        self.any_pattern = StackObject(stype=TAny)
        self.true_pattern = StackObject(stype=TBool, value=True)
        self.false_pattern = StackObject(stype=TBool, value=False)
        self.int_pattern = StackObject(stype=TInt)

        op_debug(self.cont)
        op_on(self.cont)

    def tearDown(self) -> None:
        Type.types = deepcopy(self.save_types)
        Type.ctors = deepcopy(self.save_ctors)

    # def execute(self, code) -> Any:
    #     cont = interpret(self.cont, io.StringIO(code))
    #     return cont.stack.tos().value

    def test_curry_match_and_execute(self) -> None:
        words = [Operation("test", op_gen(StackObject(stype=TInt,value=99),1), sig = TypeSignature([self.zero_pattern],[self.int_pattern]))]
        curry, sig = match_and_execute_compiled_word(self.cont, words)
        
        self.cont.stack.push(StackObject(stype=TInt,value=0))
        curry(self.cont)

        assert self.cont.stack.pop().value == 99

        self.cont.stack.push(StackObject(stype=TInt,value=1))
        with self.assertRaises(Exception) as x:
            curry(self.cont)

        words.append( Operation("test", op_gen(StackObject(stype=TInt,value=101),1), sig = TypeSignature([self.one_pattern],[self.int_pattern])))

        curry(self.cont)

        assert self.cont.stack.pop().value == 101

        words.append( Operation("test", op_gen(StackObject(stype=TInt,value=76),1), sig = TypeSignature([self.any_pattern],[self.int_pattern])))       

        self.cont.stack.push(StackObject(stype=TInt,value=0))
        self.cont.stack.push(StackObject(stype=TInt,value=42))

        curry(self.cont)

        assert self.cont.stack.pop().value == 76

        curry(self.cont)
        
        assert self.cont.stack.pop().value == 99
