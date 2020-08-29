from itertools import tee
from dataclasses import dataclass
from typing import Iterator, Tuple
from copy import deepcopy
from datetime import datetime
from os import system

from . import *
from .af_int import *
from stack import *

checkpoints = Stack()

def op_checkpoint(c: AF_Continuation) -> None:
	types = deepcopy(Type.types)
	ctors = deepcopy(Type.ctors)
	when = datetime.now()
	checkpoints.push((types,ctors,when))
make_word_context('checkpoint', op_checkpoint, [], [])


def op_checkpoints(c: AF_Continuation) -> None:
	if checkpoints.depth() == 0:
		print("\nNo checkpoints saved.")
	else:
		points = (checkpoints.contents()[::-1])
		result = "\nCheckpoints:\n"
		for count, point in enumerate(points):
			result += "\t%s\t: %s\n" % (count+1,point[2])
		print(result)
	if c.prompt:
		print(c.prompt,end='',flush=True)
make_word_context('checkpoints', op_checkpoints, [], [])


def op_restore(c: AF_Continuation) -> None:
	assert checkpoints.depth(), "No checkpoints saved."
	checkpoint = checkpoints.pop()
	Type.types = checkpoint[0]
	Type.ctors = checkpoint[1]
	s = Stack()
	r = Stack()
	c.__init__(s, r)
make_word_context('restore', op_restore, [], [])


def op_system(c: AF_Continuation) -> None:
	cmd = c.stack.pop().value
	system(cmd)
	if c.prompt:
		print(c.prompt,end='',flush=True)
make_word_context('system', op_system, [TAtom], [])
