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
	// This can't happen any longer since the "Any" type is initialized globally for Type at compile time.
	catch (const std::out_of_range& err)
	{
		std::cout << err.what() << "\n" << std::endl;
	}

	Type A = Type::find_or_make("AType");
	Type B = Type::find_or_make("BType");
	Type AA = Type::find_or_make("AType");

	std::cout << "Here is our AType: " << A << "." << std::endl;
	std::cout << "Here is our AAType: " << AA << "." << std::endl;
	std::cout << "Here is our BType: " << B << ".\n" << std::endl;

	try
	{
		std::cout << "Type ID 0 is " << Type::from_id(0) << std::endl;
		std::cout << "Type ID 1 is " << Type::from_id(1) << std::endl;
		std::cout << "Type ID 2 is " << Type::from_id(2) << std::endl;
		std::cout << "Type ID 3 is " << Type::from_id(3) << std::endl;
	}
	catch (const std::out_of_range& err)
	{
		std::cout << err.what() << std::endl;
	}

	return 0;
}
