//
//	test_operation.cpp	- Test code for Operation class.
//

#include "continuation.hpp"
#include "parser.hpp"
#include "operation.hpp"
#include "type.hpp"
//#include "types/any.hpp"
//#include "types/int.hpp"

#define DOCTEST_CONFIG_IMPLEMENT
#include <doctest/doctest.h>

using namespace std;
using namespace Types;
using namespace ActorForth;

extern Operation* t;

namespace ActorForth
{

extern ActorForth::Operation* const op_interpret;

}

void execute_me(const string& code, Continuation& cont)
{
	Parser p("op unit test", code);
	for(auto n: p.tokens())
		{
			// Pass control to the interpreter.
			cont.op = op_interpret;
			cont.token = n;
			cont.execute( cont );
		}	
}

TEST_CASE("Any Type Operations")
{
	//(void)t;

	Continuation cont;

	SUBCASE("op_interpret")
	{
		execute_me("17", cont);
		
		CHECK(cont.stack.depth()==1);
		CHECK(cont.stack.tos().type == Int);
		CHECK(get<int>(cont.stack.tos().value)==17);
	}

	SUBCASE("swap same & different types")
	{
		execute_me("15 true", cont);
		CHECK(cont.stack.depth()==2);
		CHECK(cont.stack.tos().type == Bool);
		CHECK(get<bool>(cont.stack.tos().value)==true);

		execute_me("swap", cont);
		CHECK(cont.stack.depth()==2);
		CHECK(cont.stack.tos().type == Int);
		CHECK(get<int>(cont.stack.tos().value)==15);

		execute_me("drop false swap", cont);
		CHECK(cont.stack.depth()==2);
		CHECK(cont.stack.tos().type == Bool);
		CHECK(get<bool>(cont.stack.tos().value)==true);

		execute_me("drop", cont);
		CHECK(cont.stack.depth()==1);
		CHECK(cont.stack.tos().type == Bool);
		CHECK(get<bool>(cont.stack.tos().value)==false);
	}

	SUBCASE("dup")
	{
		execute_me("test dup", cont);

		CHECK(cont.stack.depth()==2);
		CHECK(cont.stack.tos().type == Atom);
		CHECK(get<std::string>(cont.stack.tos().value)=="test");

		SUBCASE("drop")
		{
			execute_me("drop", cont);

			CHECK(cont.stack.depth()==1);
			CHECK(cont.stack.tos().type == Atom);
			CHECK(get<std::string>(cont.stack.tos().value)=="test");

			SUBCASE("swap")
			{
				execute_me("next",cont);
				CHECK(cont.stack.depth()==2);
				CHECK(cont.stack.tos().type == Atom);
				CHECK(get<std::string>(cont.stack.tos().value)=="next");

				execute_me("swap", cont);
				CHECK(cont.stack.depth()==2);
				CHECK(cont.stack.tos().type == Atom);
				CHECK(get<std::string>(cont.stack.tos().value)=="test");

				SUBCASE("2dup")
				{
					execute_me("2dup", cont);
					CHECK(cont.stack.depth()==4);
					CHECK(cont.stack.tos().type == Atom);
					CHECK(get<std::string>(cont.stack.tos().value)=="test");

					execute_me("drop", cont);
					CHECK(cont.stack.depth()==3);
					CHECK(cont.stack.tos().type == Atom);
					CHECK(get<std::string>(cont.stack.tos().value)=="next");

					execute_me("drop", cont);
					CHECK(cont.stack.depth()==2);
					CHECK(cont.stack.tos().type == Atom);
					CHECK(get<std::string>(cont.stack.tos().value)=="test");

					execute_me("drop", cont);
					CHECK(cont.stack.depth()==1);
					CHECK(cont.stack.tos().type == Atom);
					CHECK(get<std::string>(cont.stack.tos().value)=="next");

					SUBCASE("=")
					{
						execute_me("different =", cont);
						CHECK(cont.stack.depth()==1);
						CHECK(cont.stack.tos().type == Atom);
						CHECK(get<std::string>(cont.stack.tos().value)=="different");
					}
				}
			}
		}
	}
}

TEST_CASE("Int Type Operations")
{
	Continuation cont;

	SUBCASE("+")
	{
		execute_me("17 3 +", cont);

		CHECK(cont.stack.depth()==1);
		CHECK(cont.stack.tos().type == Int);
		CHECK(get<int>(cont.stack.tos().value)==20);

		SUBCASE("-")
		{
			execute_me("5 -", cont);
			CHECK(cont.stack.depth()==1);
			CHECK(cont.stack.tos().type == Int);
			CHECK(get<int>(cont.stack.tos().value)==15);

			SUBCASE("*")
			{
				execute_me("dup *", cont);
				CHECK(cont.stack.depth()==1);
				CHECK(cont.stack.tos().type == Int);
				CHECK(get<int>(cont.stack.tos().value)==225);

				SUBCASE("/")
				{
					execute_me("5 /", cont);
					CHECK(cont.stack.depth()==2);
					CHECK(cont.stack.tos().type == Int);
					CHECK(get<int>(cont.stack.tos().value)==45);

					execute_me("drop", cont);
					CHECK(cont.stack.depth()==1);
					CHECK(cont.stack.tos().type == Int);
					CHECK(get<int>(cont.stack.tos().value)==0);
				}
			}
		}
	}
}

int main(int argc, char *argv[])
{
	using namespace ActorForth;
	Types::initialize();

	cout << "test_operation" << endl;

	if (!op_interpret or !op_nop) { std::cout << "ERROR primitive ops not ready!" << std::endl; exit(-1); }
	//if(argc > 1 and std::string(argv[1]) == "--help") return -1;

	doctest::Context context;

	context.setOption("order-by", "name");            // sort the test cases by their name

    context.applyCommandLine(argc, argv);

    int res = context.run(); // run

    if(context.shouldExit()) // important - query flags (and --exit) rely on the user doing this
        return res;          // propagate the result of the tests
}
