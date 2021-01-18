//
//	stack.cpp	- Stack implementation for ActorForth.
//

#include <iostream>

#include "type.hpp"
#include "stack.hpp"


StackSig make_sig(const Type& type)  
{
	return std::make_pair(type, std::make_optional<std::any>());
}


std::ostream& operator<<(std::ostream& out, const Signature& sig)
{
	out << "Sig|in=";
	//std::for_each(sig.in_seq.begin(), sig.in_seq.end(), [&out](const StackSig& s) { out << s << ","});
	return out;
}
