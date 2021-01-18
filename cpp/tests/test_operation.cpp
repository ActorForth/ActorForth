//
//	test_operation.cpp	- Test code for Operation class.
//

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <utility>	// make_pair
#include <any>
#include <optional>

#include "operation.hpp"


TEST_CASE("Signature Checks")
{
	Signature sig;

	SUBCASE("Default Signature is Empty.")
	{
		CHECK(sig.in_seq.depth() == 0);
		CHECK(sig.out_seq.depth() == 0);
	}

	SUBCASE("Adding Stack Signatures")
	{
		Type A = Type::find_or_make("AType");
		Type B = Type::find_or_make("BType");

		auto a = std::make_pair(A, std::make_optional<std::any>());
		//std::cout << a << std::endl;

		sig.in_seq.push( a );
		//sig.in_seq.push_back(B);

		//CHECK(sig.in_seq.depth() == 1);
	}
}