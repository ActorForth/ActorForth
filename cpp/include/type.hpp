//
//	type.hpp	- Type definition for ActorForth.
//

#pragma once

#include <string>
#include <vector>
#include <map>
#include <iostream>

class Type
{
public:

	using ID = size_t;
	static Type& find_or_make( const std::string& n )
	{
		auto search = TypeIDs.find(n);
		if (search != Types.end()) return Types[search->second];
		// potential race condiction here. mutex required? too slow!
		auto t = Type(n);
		Types.push_back(t);
		return Types[t.id];
	}

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


std::ostream& operator<<(std::ostream& out, const Type& type);