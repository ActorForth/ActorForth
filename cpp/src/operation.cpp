//
//	operation.cpp	-	Definition for Operation in ActorForth.
//
#include <variant>

#include <operation.hpp>
#include <continuation.hpp>

namespace ActorForth
{

// Holds the global vocabularies of all Operations for each Type.
std::map<Type::ID,std::vector<Operation*>> Operation::TypeOps;

// Holds all of the Constructors for various Types.
std::map<Type::ID,std::vector<const Operation*>> Operation::TypeCtors;

// add -	Adds a new Operation to the appropriate vocabularies based on the
//			stack signature. Also automatically detects and registers constuctors
//			operations that have the same name as the Type except lower case and
//			their only return value is an instance of that Type.
//			Returns 0 if a conflicting Operation already exists.
Operation* Operation::add(const std::string& name, const Parser::Token& token, const Signature& sig, const Type::Handler& h, const bool force_global )
{
	//std::cout << "Operation::add( name = '" << name << "', token = " << token << ", sig = " << sig << "." << std::endl;

	// Which Type vocabulary does it belong?
	Type::ID type_id = 0; // Default to Global 'Any' vocabulary.
	try
	{
		// If we're not forcing global 'Any' scope then see if there's a stack context
		// which will indicate another Type's vocabulary to store this Operation into.
		if(not force_global) type_id = sig.in_seq.tos().type.id;
	}
	catch( Stack<StackSig>::Underflow& x ) 
	{
		std::cout << "HERE'S OUR Stack<StackSig> ERROR!" << std::endl;
	} // Empty stack means Global 'Any' vocabulary.
	/*
	catch( std::exception& x )
	{
		std::cout << "HERE'S AN UNKNOWN EXCEPTION!" << std::endl;
	}
	*/

	// Check to see the operation doesn't already exist first.
	if(Operation::find(name, sig.in_seq)) return 0;

	// Once created an Operation is never deleted.
	Operation* new_op = new Operation(name, token, sig, h);

	// Insert Operation into type vocabulary.
	TypeOps[type_id].push_back(new_op);

	//std::cout << "Just added operator " << *new_op << std::endl;

	//std::cout << Type::from_id(type_id) << " has " << Operation::TypeOps[type_id].size() << " words." << std::endl;

	// BDM TODO : Detect whether or not this is also a Constructor.
	return new_op;
}


std::ostream& operator<<(std::ostream& out, const Operation& op)
{
	out << "Op: '" << op.name << "' @ " << op.token << " with " << op.sig;
	return out;
}


Operation* const op_nop = Operation::add("nop", {}, Signature(), [](Continuation&) {;}, true);



}

#include "types/any.hpp"
#include "types/int.hpp"

Operation* t[] = 
	{ 
		// Any repl words.
		ActorForth::op_interpret,
		ActorForth::op_print, 
		ActorForth::op_empty_print, 
		ActorForth::op_stack,
		ActorForth::op_depth,
		ActorForth::op_type_words,
		ActorForth::op_words,
		ActorForth::op_types,

		// Any basic stack manipulation words.
		ActorForth::op_dup,
		ActorForth::op_drop,
		ActorForth::op_swap,
		ActorForth::op_2dup,
		ActorForth::op_assign,
		
		// Int words.
		ActorForth::op_atom_int,
		ActorForth::op_string_int,
		ActorForth::op_int_plus,
		ActorForth::op_int_minus,
		ActorForth::op_int_multiply,
		ActorForth::op_int_divide,
	};

