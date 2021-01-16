//
//	operation.cpp	-	Definition for Operation in ActorForth.
//

#include <operation.hpp>
#include <continuation.hpp>

Operation::Handler Operation::default_handler = [](Continuation& c) { c.op(c); };
