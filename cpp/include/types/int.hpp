//
//	int.hpp	-	Integer type for ActorForth.
//

#pragma once

#include "type.hpp"
#include "operation.hpp"

namespace ActorForth
{
extern Operation* const op_atom_int;
extern Operation* const op_string_int;
extern Operation* const op_int_plus;
extern Operation* const op_int_minus;
extern Operation* const op_int_multiply;
extern Operation* const op_int_divide;
}
