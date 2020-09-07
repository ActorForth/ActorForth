from itertools import tee
from dataclasses import dataclass
import binascii
from typing import Iterator, Tuple
from io import StringIO

from . import *
from .af_int import *
from .af_any import op_swap
from stack import *

"""
"data/rawbtctrans.hex" open # -> IStream
4 bytes 					# -> IStream, bytes(count=4, val=None, endian='big')
read 						# -> IStream, bytes(count=4, val=b'\x01\x00\x00\x00', endian='big')
							# Knows there's two characters per byte.
little						# -> IStream, bytes(count=4,  val=b'\x01\x00\x00\x00', endian='little')
int 						# -> IStream, int(1)
"""

TIStream = Type("IStream")
TBytes = Type("Bytes")
@dataclass
class CBytes:
	count : int
	val : Optional[str] = None
	endian : str = 'little'	


def op_istream(c: AF_Continuation) -> None:
	io = StringIO(c.stack.pop().value)
	c.stack.push(StackObject(value=io, stype=TIStream))
make_word_context('istream', op_istream, [TAtom], [TIStream])


def op_open(c: AF_Continuation) -> None:
	filename = c.stack.tos().value	
	f = open(filename)
	c.stack.pop()
	c.stack.push(StackObject(value=f, stype=TIStream))
make_word_context('open', op_open, [TAtom], [TIStream])


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
make_word_context('read', op_read_bytes, [TIStream, TBytes], [TIStream, TBytes])

