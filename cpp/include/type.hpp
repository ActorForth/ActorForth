//
//	type.hpp	- Type declaration for ActorForth.
//

#pragma once

#include <functional>
#include <string>
#include <vector>
#include <map>
#include <iostream>

class Continuation;
class Operation;

class Type
{
public:

	using Handler = std::function<void(Continuation&)>;
	static Handler default_handler;
	using ID = size_t;

	//static Type& find( const std::string& n );
	static Type& find_or_make( const std::string& n, const Handler& h = default_handler );

	static Type& from_id( const ID& id );

	bool operator==(const Type& t) const { return id == t.id; }

	const std::string name;
	const ID id;

protected:
	Type( const std::string& n, const Handler& h = default_handler ) : name(n), id(Types.size()), handler(h) {;}

private:
	const Handler handler;

	// A Type name can only be instantiated once and its position in the Types vector is its ID.
	static std::vector<Type> Types;

	// TypeIDs maps the type name to the offset position of the Types vector.
	static std::map<const std::string, const Type::ID> TypeIDs;

	friend std::ostream& operator<<(std::ostream& out, const Type& type);
};

//
//	Initialize built-in Types here. Order matters!
//
// #include "types/any.hpp"