//
//	type.hpp	- Type declaration for ActorForth.
//

#pragma once

#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <functional>

#include "continuation.hpp"

class Type
{
public:

	using ID = size_t;
	static Type& find_or_make( const std::string& n );

	static Type& from_id( const ID& id );

protected:
	Type( const std::string& n ) : name(n), id(Types.size()) {;}

private:
	const std::string name;
	const ID id;

	// A Type name can only be instantiated once and its position in the Types vector is its ID.
	static std::vector<Type> Types;
	// TypeIDs maps the type name to the offset position of the Types vector.
	static std::map<const std::string, const Type::ID> TypeIDs;
	friend std::ostream& operator<<(std::ostream& out, const Type& type);
};
