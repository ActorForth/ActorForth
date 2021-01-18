//
//	stack.cpp	- Stack implementation for ActorForth.
//

#include <iostream>
#include <variant>
#include <utility>

#include "type.hpp"
#include "stack.hpp"


std::ostream& operator<<(std::ostream& out, const StackSig& sig) 
{ 
	out << "StackSig:" << sig.first; 
	return out; 
}


StackSig make_stacksig(const Type& type)  
{
	return std::make_pair(type, std::make_optional<std::any>());
}


std::ostream& operator<<(std::ostream& out, const Signature& sig)
{
	out << "Sig|in=";

	auto v = std::get_if<Stack<StackSig>::NonEmpty>(&(sig.in_seq._stack));
	if(not v)
	{
		out << "<empty>|";
		return out;
	}

	std::for_each(v->_data.begin(), v->_data.end(), [&out](const StackSig& s) { out << s << ","; } );
	return out;
}
