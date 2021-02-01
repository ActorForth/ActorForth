//
//	continuation.hpp	- Continuation declaration for ActorForth.
//

#pragma once

#include <vector>

#include <parser.hpp>
#include <operation.hpp>
#include <stack.hpp>

class Continuation
{
public:

//private:	
	Stack<StackObject> stack;	// data stack
	Stack<StackObject> rstack;	// return stack

	Operation* op;
	Parser::Token token;

	void execute( Continuation& cont ) { (*op)(cont); }
};
