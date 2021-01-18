//
//	stack.cpp	- Stack implementation for ActorForth.
//

#include <iostream>
#include <variant>

#include "type.hpp"
#include "stack.hpp"


StackSig make_sig(const Type& type)  
{
	return std::make_pair(type, std::make_optional<std::any>());
}


std::ostream& operator<<(std::ostream& out, const Signature& sig)
{
	out << "Sig|in=";

	auto v = std::get_if<Stack<StackSig>::NonEmpty>(sig.in_seq._stack);
	if(not v) return out;

	std::for_each(v->_data.begin(), v->_data.end(), [&out](const StackSig& s) { out << s << ","});
	return out;
}
