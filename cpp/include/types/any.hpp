#pragma once

#include "type.hpp"

namespace ActorForth
{

extern Operation* const op_interpret;	

extern Operation* const op_print;
extern Operation* const op_empty_print;

extern Operation* const op_stack;

extern Operation* const op_depth;

extern Operation* const op_type_words;

extern Operation* const op_words;

extern Operation* const op_types;

extern Operation* const op_dup;

extern Operation* const op_drop;

extern Operation* const op_swap;

extern Operation* const op_2dup;

extern Operation* const op_assign;

}