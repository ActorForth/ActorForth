Encoding for ActorForth

The canonical python implementation of ActorForth will not likely implement this native encoding mechanism but when ActorForth ultimately is implemented as its own CPU (and possibly self-hosted as a software VM) we want to use a more efficient and flexible ecoding standard for expressing large values and complex structures.

Goals:

1. Support unlimited range sizes and types.
2. Efficient representation in terms of size and cost of realizing.
3. Independent of platform as much as possible.

Basic Encoding:

	Inspired by Smalltalk State Representation Protocol.
	UTF-8 encoding has similar features. Also the Microcore forth CPU as apparently 
	inspired by Transmeta's CPU.

	Octets: 

		All information comes in 8-bit octets.
		The first bit determines if this octet completes the value or continues to the next octet.
		Only the following 7 bits hold data so each octet is capable of representing 128 discrete values. 
		If the first bit is set it means the following octet is part of this value and its 7 value bits should be appended to the prior octet's value bits.
		This continues until an octet without the leading bit is set which signals the end of the value represented by this octet stream.

		Octet streams will (probably) be expressed in big endian ordering.

	Format & Special Values:

		Octet streams must be represented by their minimal representation to be valid. Therefore the number 1 MUST be b0000 0001 - a single octet. b1000 0000 0000 0001 is an invalid two octet stream that appears to resolve to 1 but actually resolves to [ NULL, 1 ] (see below).

		Octet streams that begin with b1000 signify special values.

		b1000 0000 is a placeholder for no value aka NULL or nil.
		b1000 0111 <unsigned count> <value> is a placeholder for a repeating occurance of a single value octet stream. 
		<add more - subject to heavy revision>

