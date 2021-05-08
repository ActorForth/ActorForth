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


std::ostream& operator<<(std::ostream& out, const AnyValue& val)
{
	if(auto v = std::get_if<bool>(&val)) out << std::boolalpha << *v;
	else if(auto v = std::get_if<int>(&val)) out << *v;
	else if(auto v = std::get_if<unsigned>(&val)) out << *v;
	else if(auto v = std::get_if<std::string>(&val)) out << *v;
	else out << "UNKNOWN VALUE TYPE!";
	return out;
}


std::ostream& operator<<(std::ostream& out, const std::optional<AnyValue>& val)
{
	if(val.has_value()) out << val.value();
	else
		out << "<no value>";
	return out;
}

ProductInstance::ProductInstance(const Type& type) : type(type) 
{
	std::cout << "Initializing the " << type.attribs().size() << " attributes for a " << type.name << "." << std::endl;
	// Initialze default attribute variables based on Type.
	for(size_t i=0;i<type.attribs().size();++i)
	{
		const Attribute& a = type.attribs()[i];
		std::cout << "\tInitializing attribute #" << i << " " << a.name << "." << std::endl;
		attributes.push_back(AnyValue());
	}
	std::cout << "Initialization complete." << std::endl;
}


StackSig StackSig::make_stacksig(const Type& type)  
{
	// NOTE - turns out make_optional will construct the optional with a default
	//		  ctor of the first listed type! Not what we expected/wanted!
	//return StackSig( std::make_pair(type, std::make_optional<AnyValue>()) );
	return StackSig( type, std::optional<AnyValue>() );
}


std::ostream& operator<<(std::ostream& out, const Attribute& attrib)
{
	out << "<Attribute>{ " << attrib.name << ", " << attrib.sig << ", pos:" << attrib.pos << "}";
	return out;
}


std::ostream& operator<<(std::ostream& out, const StackSig& sig) 
{ 
	out << "<Spec>{" << sig.type << ", " << sig.maybe_value << "}";
	return out; 
}


bool StackSig::operator==(const StackSig& s) const
{
	// Generic Types always match.
	if(type.id == 0) return true;

	// Different Types fail.
	if(type.id != s.type.id) return false;

	// If both our signatures specifies a value check it as well.
	if(maybe_value.has_value() and s.maybe_value.has_value() and maybe_value.value() != s.maybe_value.value()) return false;

	return true;
}

//bool StackSig::operator==(const StackObject& o) const
bool operator==(const StackSig& s, const StackObject& o)
{
	//std::cout << "Comparing " << *this << " with " << o << "." << std::endl;
	
	// Generic Types always match.
	if(s.type.id == 0) return true;

	// Different Types fail.
	if(s.type.id != o.type.id) return false;

	// If our signature specifies a value check it as well.
	if(s.maybe_value.has_value() and s.maybe_value.value() != o.value) return false;

	return true;
}



Type::Handler Type::default_handler = [](Continuation& c) { (*(c.op))(c); };

//
//	Any type is a special generic type that matches all other types.
//
//std::vector<Type> Type::Types(1, Type("Any"));
std::vector<Type> Type::Types = { {"Any"} };
std::map<const std::string, const Type::ID> Type::TypeIDs = { {"Any",0} };


Type& Type::find_or_make( const std::string& n, const Handler& handler, const bool lock )
{
	// TODO : automatically treat all types that begin with _ as generic Any types.
	auto search = TypeIDs.find(n);
	if (search != TypeIDs.end()) return Types[search->second];

	// BDM TODO : potential race condition here. mutex required? too slow! 
	// 			  probably just need to preallocate the vector for max allowed types.
	TypeIDs.insert( {n, Types.size()} );
	auto t = Type(n, handler, lock);
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
		err << "<exception: out_of_range> No such Type named '" << name << "'!";
		throw std::out_of_range(err.str());
	}
}


void Type::add_attribute( const std::string& name, const StackSig& sig ) const
{
	if(attributes_locked)
	{
		std::stringstream s;
		s << "Type attributes are locked for Type: '" << Type::name << "'.";
		throw( std::logic_error(s.str()) );
	}
	if(find_attribute(name))
	{
			std::stringstream s;
			s << "'" << name << "' is already an attribute of Type: '" << Type::name << "'.";
			throw std::logic_error(s.str());
	}

	Attribute attrib = { name, sig, attributes.size() };
	// BDM HACK HACK - this is likely undefined behavior!!
	std::vector<Attribute>* v = const_cast<std::vector<Attribute>*>(&attributes);
	//attributes.push_back(attrib);
	v->push_back(attrib);
}

void Type::_list_valid_attributes(std::stringstream& out) const
{
	out << "\t";
	if(attributes.size()==0)
	{
		out << "There are no attributes for this type yet.";
		return;
	}
	for(auto a = attributes.begin(); a != attributes.end(); ++a)
	{
		if(a!=attributes.begin()) out << ", ";
		out << a->name;
	}
}

const Attribute& Type::attrib( const std::string& name ) const
{
	const Attribute* result = find_attribute(name);
	if(result) return *result;
	std::stringstream s;
	s << "Type::attrib['" << name << "'] is not a value attribute name for Type: '" << Type::name << "'.\n";
	_list_valid_attributes(s);
	throw std::out_of_range(s.str());
}

const Attribute* Type::find_attribute(const std::string& name) const
{
	for(auto a = attributes.begin(); a != attributes.end(); ++a)
	{
		if ( (*a).name == name ) return &(*a);
	}
	return (Attribute*)0;
}

std::ostream& operator<<(std::ostream& out, const Type& type)
{
	out << "<" << type.name << "|ID:" << type.id << "|";
	if(type.attributes.size())
	{
		out << "{";

		for(auto a = type.attributes.begin(); a != type.attributes.end(); ++a)
		{
			if(a!=type.attributes.begin()) out << ", ";
			out << "[" << a->pos << "] " << a->name << ":" << a->sig;
		}

		out << "};";
	}
	out << ">";
	return out;
}

bool ProductInstance::operator!=(const ProductInstance& p) const
{
	if(type.id != p.type.id) return true;
	for(size_t i=0;i<attributes.size();++i)
	{
		if (attributes[i] != p.attributes[i]) return true;
	}
	return false;
}


AnyValue& ProductInstance::operator[](const std::string& attrib_name)
{
	return attributes[type.attrib(attrib_name).pos];
}

const AnyValue& ProductInstance::operator[](const std::string& attrib_name) const
{
	return attributes[type.attrib(attrib_name).pos];
}



	const Type Any = Type::find_or_make("Any");
	const Type IType = Type::find_or_make("Type");
	const Type Int = Type::find_or_make("Int");
	const Type Bool = Type::find_or_make("Bool");
	const Type Atom = Type::find_or_make("Atom");
	const Type String = Type::find_or_make("String");

	// The false parameter keeps the type unlocked so we may add attributes to it later.
	const Type FSPosition = Type::find_or_make("FilePosition", Type::default_handler, false);
	

void initialize(void) 
{ 	std::cout << "Type::initialize starts." << std::endl;

	FSPosition.add_attribute("filename", {String,{}}); 
	FSPosition.add_attribute("linenumber", {Int,{}}); 
	FSPosition.add_attribute("column", {Int,{}}); 
	//FSPosition.lock_attributes();

	std::cout << "Type::initialize ends." << std::endl;
}


	const Type WordSpecInputSig = Type::find_or_make("WordSpecInputSig", _word_spec_input_interpret);
	//const Type WordSpecOutputSig = Type::find_or_make("WordSpecOutputSig");
	//const Type WordInputPattern = Type::find_or_make("WordInputPattern");
	//const Type WordOutputPattern = Type::find_or_make("WordOutputPattern");
	//const Type WordCodeCompile = Type::find_or_make("WordCodeCompile");
//};

}
