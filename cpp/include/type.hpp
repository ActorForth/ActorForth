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
// struct StackSig;

}




namespace Types
{


struct Attribute;


class Type
{
public:

	using Handler = std::function<void(Continuation&)>;
	static Handler default_handler;
	using ID = size_t;

	//static Type& find( const std::string& n );
	static Type& find_or_make( const std::string& n, const Handler& handler = default_handler, const bool lock = true );

	static Type& from_id( const ID& id );
	static Type& from_name( const std::string& name );

	bool operator==(const Type& t) const { return id == t.id; }

	//Type( const Type&& ) = default;

	Type( void ) = delete;	
	Type( const Type& t );
	~Type( void );
	
	Type& operator=(const Type& t);

	
	static size_t size() { return Types.size(); }
 
	void lock_attributes(void) { attributes_locked = true; }

	void add_attribute( const std::string& name ); // BDM refactor! , const ActorForth::StackSig& sig ) const;
	const std::vector<Attribute>& attribs(void) const {return attributes;}

	const Attribute& attrib( const std::string& name ) const;

	const std::string name;
	const ID id;

protected:
	Type( const std::string& n, const Handler& h = default_handler, const bool lock = true);

private:

	const Handler handler;

	// User defined attributes for this type.
	std::vector<Attribute> attributes;

	const Attribute* find_attribute(const std::string& name) const;

	// When locked, no attributes may be added/removed.
	bool attributes_locked;

	// A Type name can only be instantiated once and its position in the Types vector is its ID.
	static std::vector<Type> Types;

	// TypeIDs maps the type name to the offset position of the Types vector.
	static std::map<const std::string, const Type::ID> TypeIDs;

	friend std::ostream& operator<<(std::ostream& out, const Type& type);

	void _list_valid_attributes(std::stringstream& out) const;
};

struct ProductInstance;

using AnyValue = std::variant< bool, int, unsigned, std::string, ProductInstance>; 

struct ProductInstance
{
	ProductInstance() = delete;
	//~ProductInstance();
	ProductInstance(const Type& type);

	AnyValue& operator[](const std::string& attrib_name);
	const AnyValue& operator[](const std::string& attrib_name) const;
	Type type;
	std::vector<AnyValue> attributes;

	bool operator!=(const ProductInstance& p) const;
	//bool operator==(const ProductInstance& p) const;
};


std::ostream& operator<<(std::ostream& out, const AnyValue& val);
std::ostream& operator<<(std::ostream& out, const std::optional<AnyValue>& val);


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

/* BDM
extern const Type FSPosition;

extern const Type WordSpecInputSig;
extern const Type WordSpecOutputSig;
extern const Type WordInputPattern;
extern const Type WordOutputPattern;
extern const Type WordCodeCompile;
*/

} // eons Types
