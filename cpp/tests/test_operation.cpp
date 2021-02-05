//
//	test_operation.cpp	- Test code for Operation class.
//

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "continuation.hpp"
#include "operation.hpp"
#include "types/any.hpp"
#include "types/int.hpp"

using namespace std;

extern Operation* t;

TEST_CASE("Any Type Operations")
{
	(void)t;

	Continuation cont;

	SUBCASE("op_interpret")
	{
		const string code = "17";
		Parser p("op_interpret", code);


		for(auto n: p.tokens())
		{
			// Pass control to the interpreter.
			//cont.op = op_interpret;
			cont.token = n;
			cont.execute( cont );
		}	
		/*

		CHECK(cont.stack.depth()==1);
		CHECK(cont.stack.tos().type == Int);
		CHECK(get<int>(cont.stack.tos().value)==17);
		*/
	}
}
