//
//	operation.cpp	-	Definition for Operation in ActorForth.
//

#include <operation.hpp>
#include <continuation.hpp>

// Holds the global vocabularies of all Operations for each Type.
std::map<Type::ID,std::vector<Operation*>> Operation::TypeOps;

// Holds all of the Constructors for various Types.
std::map<Type::ID,std::vector<Operation*>> Operation::TypeCtors;

// add -	Adds a new Operation to the appropriate vocabularies based on the
//			stack signature. Also automatically detects and registers constuctors
//			operations that have the same name as the Type except lower case and
//			their only return value is an instance of that Type.
bool Operation::add(const std::string& name, const Parser::Token& token, const Signature& sig, const Type::Handler& h, const bool force_global )
{
	// TODO : check to see the operation doesn't already exist first.

	// Once created an Operation is never deleted.
	Operation* new_op = new Operation(name, token, sig, h);

	// Which Type vocabulary does it belong?
	Type::ID type = 0; // Default to Global 'Any' vocabulary.
	try
	{
		if(not force_global) type = sig.in_seq.tos().first.id;
	}
	catch( const Stack<StackSig>::Underflow& x ) {;} // Empty stack means Global 'Any' vocabulary.

	// Insert Operation into type vocabulary.
	TypeOps[type].push_back(new_op);
	return true;
}

// find -	Returns an Operation given a name based on the context of a stack and
//			the Operation's type signature if such exists. Operations with the
//			longest type signature have priority.
Operation* Operation::find(const std::string& op_name, const Stack<StackObject>& stack)
{
	return (Operation*)0;
}

