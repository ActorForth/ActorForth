//
//	compiler.cpp -	types & functions for ActorForth compiler implementation.
//

#include "types/compiler.hpp"

namespace ActorForth
{

// Starts compilation of a new word.
void _word_spec_start( Continuation& c )
{
	const std::string name = std::get<std::string>(c.stack.tos().value);
	WordSpecification new_word_spec = { name, c.token, Signature() };
	c.stack.pop();
	// BDM - get UDT first. c.stack.push( StackObject( WordSpecInputSig, new_word_spec ) );
}

// Executes a compiled word.
void _word_spec_input_interpret( Continuation& c )
{
	StackObject atom( c.stack.tos() );
	const std::string name = std::get<std::string>(atom.value);

	// Is this a Type?
	try
	{
		Type& type = Type::from_name( name );
		(void)type;
		//WordSpecification spec;

		return;
	}
	catch( const std::out_of_range& ) {}

	// If not a Type, we'll allow an immediate execution of a word that
	// has no stack effect.
	Operation* word = Operation::find( name, Stack<StackObject>() );
	(void)word;
	// Did we get one and, if so, does it not have any return stack signature?

}



	
}