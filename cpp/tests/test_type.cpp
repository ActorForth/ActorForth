//
//	test_type.cpp	- Test code for Type class.
//

#include <iostream>

#include "type.hpp"

int main(void)
{
	try
	{
		std::cout << "\nType ID 0 is " << Type::from_id(0) << std::endl;
	}
	catch (const std::out_of_range& err)
	{
		std::cout << err.what() << "\n" << std::endl;
	}

	Type A = Type::find_or_make("AType");
	Type B = Type::find_or_make("BType");

	std::cout << "Here is our AType: " << A << "." << std::endl;
	std::cout << "Here is our BType: " << B << "." << std::endl;

	std::cout << "\nType ID 0 is " << Type::from_id(0) << std::endl;
	std::cout << "Type ID 1 is " << Type::from_id(1) << std::endl;

	try
	{
		std::cout << "\nType ID 2 is " << Type::from_id(2) << std::endl;
	}
	catch (const std::out_of_range& err)
	{
		std::cout << err.what() << std::endl;
	}

	return 0;
}
