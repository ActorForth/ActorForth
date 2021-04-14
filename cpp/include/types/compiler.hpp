//
//	compiler.hpp -	types & functions for ActorForth compiler implementation.
//

#pragma once

#include "type.hpp"
#include "continuation.hpp"
#include "operation.hpp"

namespace ActorForth
{

struct WordSpecification
{
	std::string name;
	Parser::Token token;
	Signature sig;
	//Type::Handler handler; Sticking with the default only for now.
};

// Starts compilation of a new word.
void _word_spec_start( Continuation& c );

Operation* const op_word_spec_start = ActorForth::Operation::add(":", {}, {{Atom},{WordSpecInputSig}}, _word_spec_start, true);


struct WordImplementation
{
	WordSpecification spec;
	std::vector<const Operation*> words;
};

// Executes a compiled word.
void _word_spec_input_interpret( Continuation& c );

//Operation* const op_word_spec_interpret = ActorForth::Operation::add(":", {}, {{Atom},{WordSpec}}, _word_spec_interpret, true);



}