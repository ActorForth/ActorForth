//
// 	repl.hpp	- top level read/eval/print loop for ActorForth.
//

#pragma once

#include "continuation.hpp"
#include "parser.hpp"
#include "operation.hpp"

int main(int argc, char *argv[]);

namespace ActorForth
{

extern ActorForth::Operation* const op_interpret;

}