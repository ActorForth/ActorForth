from itertools import tee
from dataclasses import dataclass
import binascii
from typing import Iterator, Tuple

from . import *
from .af_int import *
from .af_any import op_swap
from stack import *

"""
"data/rawbtctrans.hex" open # -> FStream
4 bytes 					# -> FStream, bytes(count=4, val=None, endian='big')
read 						# -> FStream, bytes(count=4, val=b'\x01\x00\x00\x00', endian='big')
							# Knows there's two characters per byte.
little						# -> FStream, bytes(count=4,  val=b'\x01\x00\x00\x00', endian='little')
int 						# -> FStream, int(1)
"""

TFStream = Type("FStream")
TBytes = Type("Bytes")
@dataclass
class CBytes:
	count : int
	val : Optional[str] = None
	endian : str = 'big'	


def op_open(c: AF_Continuation) -> None:
	filename = c.stack.tos().value	
	f = open(filename)
	c.stack.pop()
	c.stack.push(StackObject(value=f, stype=TFStream))
make_word_context('open', op_open, [TAtom], [TFStream])


def op_bytes(c: AF_Continuation) -> None:
	count = c.stack.pop().value
	c.stack.push(StackObject(value=CBytes(count), stype=TBytes))
make_word_context('bytes', op_bytes, [TInt], [TBytes])


def op_atom_bytes(c: AF_Continuation) -> None:
	op_int(c)
	op_bytes(c)
make_word_context('bytes', op_atom_bytes, [TAtom], [TBytes])	


def op_little(c: AF_Continuation) -> None:
	c.stack.tos().value.endian = 'little'
make_word_context('little', op_little, [TBytes], [TBytes])


def op_big(c: AF_Continuation) -> None:
	c.stack.tos().value.endian = 'big'
make_word_context('big', op_big, [TBytes], [TBytes])


def op_bytes_to_int(c: AF_Continuation) -> None:
	assert c.stack.tos().value.val is not None, "No content in Bytes object."
	b = c.stack.pop().value
	result = int.from_bytes(b.val, byteorder = b.endian)
	c.stack.push(StackObject(value=result, stype=TInt))
make_word_context('int', op_bytes_to_int, [TBytes], [TInt])


def op_read_bytes(c: AF_Continuation) -> None:
	count = c.stack.tos().value.count
	op_swap(c)
	f = c.stack.tos().value
	op_swap(c)
	c.stack.tos().value.val = binascii.unhexlify(f.read(count*2)) # Two characters per byte.
make_word_context('read', op_read_bytes, [TFStream, TBytes], [TFStream, TBytes])

