//
//	test_stack.cpp	- Test code for Stack class.
//

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <iostream>

#include "stack.hpp"

TEST_CASE("Stack")
{
	Stack<char> stack;
	SUBCASE("Empty stack has no depth.")
	{
		CHECK(stack.depth() == 0);
	}

	SUBCASE("Empty stack has no tos.")
	{
		CHECK_THROWS_AS(stack.tos(), const Stack<char>::Underflow);
	}

	SUBCASE("Empty stack can't pop.")
	{
		CHECK_THROWS_AS(stack.pop(), const Stack<char>::Underflow);
	}

	SUBCASE("Putting things on the stack.")
	{
		stack.push('a');
		CHECK(stack.tos() == 'a');
		stack.push('b');
		CHECK(stack.tos() == 'b');
		CHECK(stack.depth() == 2);

		SUBCASE("Taking things off the stack.")
		{
			stack.pop();
			CHECK(stack.tos() == 'a');
			CHECK(stack.depth() == 1);
			stack.pop();
			CHECK(stack.depth() == 0);
			CHECK_THROWS_AS(stack.tos(), const Stack<char>::Underflow);
			CHECK_THROWS_AS(stack.pop(), const Stack<char>::Underflow);
		}

		SUBCASE("Altering content on the stack.")
		{
			stack.tos() = 'c';
			CHECK(stack.tos() == 'c');
			stack.pop();
			CHECK(stack.tos() == 'a');
			stack.pop();
			CHECK_THROWS_AS(stack.tos() = 'd';, const Stack<char>::Underflow);
		}
	}
}

TEST_CASE("Signature Checks")
{
	Signature sig;

	SUBCASE("Default Signature is Empty.")
	{
		CHECK(sig.in_seq.depth() == 0);
		CHECK(sig.out_seq.depth() == 0);
	}

	SUBCASE("Adding Stack Sigs")
	{
		Type A = Type::find_or_make("AType");
		Type B = Type::find_or_make("BType");

		//auto a = std::make_pair(A, std::make_optional<std::any>());
		auto a = make_stacksig(A);
		std::cout << "Stack Sig: " << a << std::endl;

		sig.in_seq.push( a );
		CHECK(sig.in_seq.depth() == 1);

		sig.in_seq.push(make_stacksig(B));

		std::cout << "Signature with In but not Out: " << sig << std::endl;

		sig.out_seq.push(make_stacksig(B));
		sig.out_seq.push(make_stacksig(A));

		std::cout << "Signature with In & Out: " << sig << std::endl;

		// Empty Signature output.
		std::cout << "Empty Signature: " << Signature() << std::endl;
	}
}
