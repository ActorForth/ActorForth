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

Type::ID Type::priorID;

int main(void)
{
	Type A("A Type");
	Type B("B Type");

	std::cout << "Here is our A Type: " << A << "." << std::endl;
	std::cout << "Here is our B Type: " << B << "." << std::endl;

	return 0;
}