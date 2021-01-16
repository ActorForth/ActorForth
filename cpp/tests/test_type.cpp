//
//	test_type.cpp	- Test code for Type class.
//

#include <iostream>

#include "type.hpp"

std::ostream& operator<<(std::ostream& out, const Type& type)
{
	out << "'" << type.name << "'|ID:" << type.id << "|";
	return out;
}

std::vector<Type> Type::Types;
std::map<const std::string, const Type::ID> Type::TypeIDs;

int main(void)
{
	Type A = Type::find_or_make("A Type");
	Type B = Type::find_or_make("B Type");

	std::cout << "Here is our A Type: " << A << "." << std::endl;
	std::cout << "Here is our B Type: " << B << "." << std::endl;

	return 0;
}
