//
//	type.cpp	- Type definition for ActorForth.
//

//#include <doctest/doctest.h>

#include <iostream>
#include <sstream>
#include <stdexcept>

#include "type.hpp"
#include "continuation.hpp"

#include "types/compiler.hpp"

using namespace ActorForth;

namespace Types
{

Type::Handler Type::default_handler = [](Continuation& c) { (*(c.op))(c); };

//
//	Any type is a special generic type that matches all other types.
//
//std::vector<Type> Type::Types(1, Type("Any"));
std::vector<Type> Type::Types = { {"Any"} };
std::map<const std::string, const Type::ID> Type::TypeIDs = { {"Any",0} };


Type& Type::find_or_make( const std::string& n, const Handler& handler )
{
	// TODO : automatically treat all types that begin with _ as generic Any types.
	auto search = TypeIDs.find(n);
	if (search != TypeIDs.end()) return Types[search->second];

	// BDM TODO : potential race condition here. mutex required? too slow! 
	// 			  probably just need to preallocate the vector for max allowed types.
	TypeIDs.insert( {n, Types.size()} );
	auto t = Type(n, handler);
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

Type& Type::from_name( const std::string& name )
{
	try
	{
		return Types[TypeIDs.at(name)];
	}	
	catch( const std::out_of_range& x )
	{
		std::stringstream err;
		err << "<exception: out_of_range> No such Type named '" << name << "!";
		throw std::out_of_range(err.str());
	}
}

std::ostream& operator<<(std::ostream& out, const Type& type)
{
	out << "<" << type.name << "|ID:" << type.id << "|>";
	return out;
}

void initialize(void) 
{ };
	const Type Any = Type::find_or_make("Any");
	const Type IType = Type::find_or_make("Type");
	const Type Int = Type::find_or_make("Int");
	const Type Bool = Type::find_or_make("Bool");
	const Type Atom = Type::find_or_make("Atom");
	const Type String = Type::find_or_make("String");



	const Type WordSpecInputSig = Type::find_or_make("WordSpecInputSig", _word_spec_input_interpret);
	const Type WordSpecOutputSig = Type::find_or_make("WordSpecOutputSig");
	const Type WordInputPattern = Type::find_or_make("WordInputPattern");
	const Type WordOutputPattern = Type::find_or_make("WordOutputPattern");
	const Type WordCodeCompile = Type::find_or_make("WordCodeCompile");
//};

}
