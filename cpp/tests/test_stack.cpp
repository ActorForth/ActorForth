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
		Type Any = Type::find_or_make("Any");

		//auto a = std::make_pair(A, std::make_optional<std::any>());
		auto a = StackSig::make_stacksig(A);
		std::cout << "Stack Sig: " << a << std::endl;

		sig.in_seq.push( a );
		CHECK(sig.in_seq.depth() == 1);

		sig.in_seq.push(StackSig::make_stacksig(B));

		std::cout << "Signature with In but not Out: " << sig << std::endl;

		sig.out_seq.push(StackSig::make_stacksig(B));
		sig.out_seq.push(StackSig::make_stacksig(A));

		std::cout << "Signature with In & Out: " << sig << std::endl;

		// Empty Signature output.
		auto empty_sig = Signature();
		std::cout << "Empty Signature: " << empty_sig << std::endl;

		SUBCASE("Matching Signatures")
		{
			auto alt_longer_match_sig = Signature();
			alt_longer_match_sig.in_seq.push(StackSig::make_stacksig(A));
			alt_longer_match_sig.in_seq.push(StackSig::make_stacksig(A));
			alt_longer_match_sig.in_seq.push(StackSig::make_stacksig(B));

			auto alt_mismatch_sig = Signature();
			alt_mismatch_sig.in_seq.push(StackSig::make_stacksig(A));
			alt_mismatch_sig.in_seq.push(StackSig::make_stacksig(A));
			alt_mismatch_sig.in_seq.push(StackSig::make_stacksig(A));

			CHECK(sig.matches(sig.in_seq) == true);
			CHECK(sig.matches(empty_sig.in_seq) == false);

			CHECK(sig.matches(alt_longer_match_sig.in_seq) == true);

			CHECK(sig.matches(alt_mismatch_sig.in_seq) == false);

			CHECK(empty_sig.matches(sig.in_seq) == true);

			SUBCASE("Matching Generic Signatures.")
			{
				// Replace the top object in sig with a generic signature.
				sig.in_seq.pop();
				sig.in_seq.push(StackSig::make_stacksig(Any));

				// Now it should match the prior mismatched one.
				CHECK(sig.matches(alt_mismatch_sig.in_seq) == true);
			}

			SUBCASE("Matching filtered value signatures.")
			{
				sig.in_seq.tos().second = 1;
				CHECK(sig.matches(alt_longer_match_sig.in_seq) == true);

				alt_longer_match_sig.in_seq.tos().second = 2;
				CHECK(sig.matches(alt_longer_match_sig.in_seq) == false);
			}
		}

		SUBCASE("Matching StackObjects")
		{
			Stack<StackObject> match_stack;
			Stack<StackObject> mis_match_stack;

			match_stack.push( StackObject::make_stackobj(A, 1) );
			match_stack.push( StackObject::make_stackobj(B, 1) );		

			mis_match_stack.push( StackObject::make_stackobj(B, 1) );	
			mis_match_stack.push( StackObject::make_stackobj(B, 1) );


			Stack<StackObject> short_stack;
			short_stack.push( StackObject::make_stackobj(A, 1) );

			std::cout << "sig = " << sig << std::endl;
			//std::cout << "match_stack = " << match_stack << std::endl;
			CHECK(sig.matches(match_stack) == true);

			CHECK(empty_sig.matches(match_stack) == true);

			CHECK(sig.matches(mis_match_stack) == false);

			CHECK(sig.matches(short_stack) == false);

			SUBCASE("Matching Generic Signatures.")
			{
				// Replace the top object in sig with a generic signature.
				sig.in_seq.pop();
				sig.in_seq.push(StackSig::make_stacksig(Any));

				std::cout << "Trying generic signature match of " << sig.in_seq << " against " << mis_match_stack << "." << std::endl;

				// Now it should match the prior mismatched one.
				CHECK(sig.matches(mis_match_stack) == true);
			}

			SUBCASE("Matching filtered value signatures.")
			{
				sig.in_seq.tos().second = 1;
				CHECK(sig.matches(match_stack) == true);

				match_stack.tos().second = 2;
				CHECK(sig.matches(match_stack) == false);
			}
		}
	}
}
