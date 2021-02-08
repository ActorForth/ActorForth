//
//	type.hpp	- Type declaration for ActorForth.
//

#pragma once

#include <functional>
#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <stdexcept>
#include <variant>

class Continuation;
struct StackObject;

namespace ActorForth
{

class Operation;

}

namespace Types
{


class Type
{
public:

	using Handler = std::function<void(Continuation&)>;
	static Handler default_handler;
	using ID = size_t;

	//static Type& find( const std::string& n );
	static Type& find_or_make( const std::string& n, const Handler& handler = default_handler );

	static Type& from_id( const ID& id );
	static Type& from_name( const std::string& name );

	bool operator==(const Type& t) const { return id == t.id; }

	//Type( const Type&& ) = default;
	
	Type( const Type& t ) = default;
	Type& operator=(const Type& t) 
	{
		if(name != t.name) throw std::logic_error("Can't re-assign Type instance names.");
		if(id != t.id) throw std::logic_error("Can't re-assign Type instance ids.");
		if(handler.target<void(Continuation&)>() != t.handler.target<void(Continuation&)>()) throw std::logic_error("Can't re-assign Type instance handlers.");

		return *this;
	}
	
	static size_t size() { return Types.size(); }

	const std::string name;
	const ID id;

protected:
	Type( const std::string& n, const Handler& h = default_handler ) : name(n), id(Types.size()), handler(h) 
	{ ; } //std::cout << "Type::ctor( n=" << n << ")" << std::endl; }

private:

	const Handler handler;

	// A Type name can only be instantiated once and its position in the Types vector is its ID.
	static std::vector<Type> Types;

	// TypeIDs maps the type name to the offset position of the Types vector.
	static std::map<const std::string, const Type::ID> TypeIDs;

	friend std::ostream& operator<<(std::ostream& out, const Type& type);
};



using AnyValue = std::variant< bool, int, unsigned, std::string >;

std::ostream& operator<<(std::ostream& out, const AnyValue& val);
std::ostream& operator<<(std::ostream& out, const std::optional<AnyValue>& val);	



struct StackSig
{
	// Note - Generic types will always ignore a specified value.
	StackSig( const Type& t, const std::optional<AnyValue>& x ) : type(t), maybe_value(x) {;}
	StackSig( const StackSig& s ) = default;

	// BDM StackSig( std::pair< Type,std::optional<AnyValue> >&& x ) : std::pair< Type,std::optional<AnyValue> >(x) {;}

	static StackSig make_stacksig(const Type& type);
	template<class T> static StackSig make_stacksig(const Type& type, const T& val ) 
	{
		return StackSig(type, std::make_optional< AnyValue >( val ));
	}

	bool operator==(const StackSig& o) const;
	//bool operator==(const StackObject& o) const;
	friend bool operator==(const StackSig& s, const StackObject& o);

	Type type;
	std::optional<AnyValue> maybe_value;
};

std::ostream& operator<<(std::ostream& out, const StackSig& sig);

/*
struct Signature
{
	Signature() = default;
	Signature(const Signature&) = default;
	Signature(const std::vector<Type>& in, const std::vector<Type>& out = {});
	Stack<StackSig> in_seq;
	Stack<StackSig> out_seq;

	bool matches(const Stack<StackObject>& sobjects) const;
	bool matches(const Stack<StackSig>& sig) const;

	std::ostream& display(std::ostream& o) const;

};

std::ostream& operator<<(std::ostream& out, const Signature& sig);
*/



//
//	Initialize built-in Types here. Order matters!
//
// #include "types/any.hpp"
void initialize(void); 

extern const Type Any;
extern const Type IType;
extern const Type Int;
extern const Type Bool;
extern const Type Atom;
extern const Type String;  // BDM : Should we just use Atoms?

extern const Type WordSpecInputSig;
extern const Type WordSpecOutputSig;
extern const Type WordInputPattern;
extern const Type WordOutputPattern;
extern const Type WordCodeCompile;

};