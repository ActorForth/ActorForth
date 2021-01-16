//
//	type.cpp	- Type definition for ActorForth.
//

#include <sstream>
#include <stdexcept>

#include "type.hpp"

std::vector<Type> Type::Types;
std::map<const std::string, const Type::ID> Type::TypeIDs;

Type& Type::find_or_make( const std::string& n, const Operation::Handler& h )
{
	// TODO : automatically treat all types that begin with _ as generic Any types.
	auto search = TypeIDs.find(n);
	if (search != TypeIDs.end()) return Types[search->second];
	// BDM TODO : potential race condiction here. mutex required? too slow! 
	// 			  probably just need to preallocate the vector for max allowed types.
	auto t = Type(n, h);
	Types.push_back(t);
	return Types[t.id];
}

Type& Type::from_id( const ID& id ) 
{ 		
	if(id < Types.size()) return Types[id]; 
	std::stringstream err;
	if (Types.size())
	{
		err << "<exception: out_of_range> Request for TypeID : " << id << " is beyond largest type id of " << Types.size() - 1 << "!";
	}
	else
	{
		err << "<exception: out_of_range> No types exist!";
	}
	throw std::out_of_range(err.str());
}	

std::ostream& operator<<(std::ostream& out, const Type& type)
{
	out << "'" << type.name << "'|ID:" << type.id << "|";
	return out;
}
