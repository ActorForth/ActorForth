//
// 	repl.hpp	- top level read/eval/print loop for ActorForth.
//

#pragma once

#include "continuation.hpp"
#include "parser.hpp"
#include "operation.hpp"

int main(int argc, char *argv[]);

extern Operation* const op_interpret;