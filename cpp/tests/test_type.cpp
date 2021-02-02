//
//	test_type.cpp	- Test code for Type class.
//

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "type.hpp"

TEST_CASE("Registering and querying types.")
{
	SUBCASE("Type ID 0 is for the generic 'Any' Type.")
	{
		CHECK(Type::from_id(0).name == "Any");
	}

	Type::ID max_id = 0;

	SUBCASE("Registering New Types")
	{	
		Type A = Type::find_or_make("AType");
		max_id = A.id;
		CHECK(Type::from_id(max_id) == A);

		Type B = Type::find_or_make("BType");
		max_id = B.id;
		CHECK(Type::from_id(max_id) == B);

		SUBCASE("Name collissions return the prior type.")
		{
			Type AAgain = Type::find_or_make("AType");
			CHECK(Type::from_id(AAgain.id) == A);
		}

		SUBCASE("Getting a type beyond the number of registered types throws std::out_of_range.")
		{
			CHECK_THROWS_AS(Type::from_id(max_id + 1), const std::out_of_range);
		}
	}
}

TEST_CASE("main") 
{

	CHECK(Bool.name == "Bool");

	CHECK(Any.name == "Any");

	CHECK(Int.name == "Int");

}
