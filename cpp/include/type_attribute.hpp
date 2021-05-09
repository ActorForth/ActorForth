//
//	type_attribute.hpp	- Attribute declaration for ActorForth.
//

#pragma once


//#include "stack_sig.hpp"

using namespace ActorForth;
//{

namespace Types
{	

struct Attribute
{
	const std::string name;
	//const StackSig sig;
	const size_t pos;
};

//std::ostream& operator<<(std::ostream& out, const Attribute& attrib);

std::ostream& operator<<(std::ostream& out, const Attribute& attrib)
{
	out << "<Attribute>{ " << attrib.name ; // << ", " << attrib.sig 
	out << ", pos:" << attrib.pos << "}";
	return out;
}


} // eons Types

//} // eons ActorForth