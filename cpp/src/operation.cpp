//
//	operation.cpp	-	Declarations for Operation in ActorForth.
//

#include <operation.hpp>
#include <continuation.hpp>

Operation::Handler Operation::default_handler = [](Continuation& c) { c.op(c); };