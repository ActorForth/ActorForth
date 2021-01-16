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

	Type( const std::string&& n) : name(n), id(priorID++) {;}
	using ID = size_t;


private:
	const std::string name;
	const ID id;

	static ID priorID;
	friend std::ostream& operator<<(std::ostream& out, const Type& type);
};


std::ostream& operator<<(std::ostream& out, const Type& type);