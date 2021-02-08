//
//	stack.cpp	- Stack implementation for ActorForth.
//

// #include <doctest/doctest.h>

#include <iostream>
#include <variant>
#include <utility>
#include <optional>

#include "stack.hpp"


template<> const std::vector<StackObject> Stack<StackObject>::AlwaysEmpty = std::vector<StackObject>();
template<> const std::vector<StackSig> Stack<StackSig>::AlwaysEmpty  = std::vector<StackSig>();


std::ostream& operator<<(std::ostream& out, const StackObject& obj)
{
	out << "<StackObject>{" << obj.type << ", val: " << obj.value << "}";	
	return out;
}

std::ostream& operator<<(std::ostream& out, const Signature& sig)
{
	out << "<Signature>{in=" << sig.in_seq << ", out=" << sig.out_seq << "}";
	return out;
}

// Confirms whether or not the inbound stack complies with this Signature.
// Only the last n stack entries are checked where n = in_seq.depth().
bool Signature::matches(const Stack<StackObject>& sobjects) const
{
	// If our input signature is empty then it's a match!
	if(in_seq.depth() == 0) return true;

	// Is sobject long enough?
	if(sobjects.depth() < in_seq.depth()) return false;
	
	auto o = sobjects.rbegin();
	for(auto s = in_seq.rbegin(); s != in_seq.rend();++s)
	{
		if(*s != *o++) return false;
	}

	return true;
}
	

bool Signature::matches(const Stack<StackSig>& sig) const
{
	// If our input signature is empty then it's a match!
	if(in_seq.depth() == 0) return true;

	// Is sobject long enough?
	if(sig.depth() < in_seq.depth()) return false;
	
	auto o = sig.rbegin();
	for(auto s = in_seq.rbegin(); s != in_seq.rend();++s)
	{
		if(*s != *o++) return false;
	}

	return true;
}


Signature::Signature(const std::vector<Type>& in, const std::vector<Type>& out)
{
	for(auto i=in.begin();i!=in.end();++i)
	{
		in_seq.push( StackSig::make_stacksig(*i) );
	}

	for(auto i=out.begin();i!=out.end();++i)
	{
		out_seq.push( StackSig::make_stacksig(*i) );
	}
}

std::ostream& Signature::display(std::ostream& o) const
{
	bool first = true;
	if(in_seq.depth()==0) o << "[]";
	for(auto i=in_seq.begin();i!=in_seq.end();++i)
	{
		if(!first) o << ", ";
		first = false;
		o << (*i).type.name;
		if((*i).maybe_value.has_value()) o << "{ " << (*i).maybe_value << " }";
	}

	o << " -> ";

	first = true;
	if(out_seq.depth()==0) o << "[]";
	for(auto i=out_seq.begin();i!=out_seq.end();++i)
	{
		if(!first) o << ", ";
		first = false;
		o << (*i).type.name;
		if((*i).maybe_value.has_value()) o << "{ " << (*i).maybe_value << " }";
	}

	return o;
}
