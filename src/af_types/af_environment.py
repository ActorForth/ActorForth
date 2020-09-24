from itertools import tee
from dataclasses import dataclass
from typing import Iterator, Tuple
from copy import deepcopy
from datetime import datetime
from os import system

from . import *
from .af_int import *
from stack import *
from af_types.af_branch import op_pcsave, op_pcreturn
from interpret import interpret
from continuation import Continuation


checkpoints = Stack()

def op_checkpoint(c: AF_Continuation) -> None:
	types = deepcopy(Type.types)
	ctors = deepcopy(Type.ctors)
	udts  = deepcopy(Type.udts)
	when = datetime.now()
	checkpoints.push((types,ctors,udts,when))
make_word_context('checkpoint', op_checkpoint, [], [])


def op_checkpoints(c: AF_Continuation) -> None:
	if checkpoints.depth() == 0:
		print("\nNo checkpoints saved.")
	else:
		points = (checkpoints.contents()[::-1])
		result = "\nCheckpoints:\n"
		for count, point in enumerate(points):
			ts = point[3].isoformat()[0:-7]
			result += "\t%s\t: %s\n" % (count+1,ts)
		print(result)
	if c.prompt:
		print(c.prompt,end='',flush=True)
make_word_context('checkpoints', op_checkpoints, [], [])


def op_restore(c: AF_Continuation) -> None:
	assert checkpoints.depth(), "No checkpoints saved."
	checkpoint = checkpoints.pop()
	Type.types = checkpoint[0]
	Type.ctors = checkpoint[1]
	Type.udts  = checkpoint[2]

	## TODO : This doesn't seem to be resetting our stacks.
	s = Stack()
	r = Stack()
	c = Continuation(stack=s, rstack=r)

	if checkpoints.depth() == 0:
		op_checkpoint(c)
make_word_context('restore', op_restore, [], [])


def op_system(c: AF_Continuation) -> None:
	cmd = c.stack.pop().value
	system(cmd)
	if c.prompt:
		print(c.prompt,end='',flush=True)
make_word_context('system', op_system, [TAtom], [])


def op_load(c: AF_Continuation) -> None:
	filename = c.stack.pop().value
	f = None
	for file in [filename, filename + '.a4', 'lib/' + filename, 'lib/' + filename + '.a4']:
		try:
			f = open(file)
			break
		except FileNotFoundError:
			pass
	if not f:
		print("No file or module called '%s' found." % filename)
	else:
		op_pcsave(c)
		c.execute(interpret(c, f, filename))
		op_pcreturn(c)
	if c.prompt:
		print(c.prompt,end='',flush=True)
make_word_context('load', op_load, [TAtom], [])
