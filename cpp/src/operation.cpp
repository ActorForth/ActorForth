//
//	operation.cpp	-	Definition for Operation in ActorForth.
//

#include <operation.hpp>
#include <continuation.hpp>

// Holds the global vocabularies of all Operations for each Type.
std::map<Type::ID,std::vector<Operation*>> Operation::TypeOps;

// Holds all of the Constructors for various Types.
std::map<Type::ID,std::vector<const Operation*>> Operation::TypeCtors;

// add -	Adds a new Operation to the appropriate vocabularies based on the
//			stack signature. Also automatically detects and registers constuctors
//			operations that have the same name as the Type except lower case and
//			their only return value is an instance of that Type.
Operation* Operation::add(const std::string& name, const Parser::Token& token, const Signature& sig, const Type::Handler& h, const bool force_global )
{
	// TODO : check to see the operation doesn't already exist first.

	// Once created an Operation is never deleted.
	Operation* new_op = new Operation(name, token, sig, h);

	// Which Type vocabulary does it belong?
	Type::ID type = 0; // Default to Global 'Any' vocabulary.
	try
	{
		// If we're not forcing global 'Any' scope then see if there's a stack context
		// which will indicate another Type's vocabulary to store this Operation into.
		if(not force_global) type = sig.in_seq.tos().first.id;
	}
	catch( const Stack<StackSig>::Underflow& x ) {;} // Empty stack means Global 'Any' vocabulary.

	// Insert Operation into type vocabulary.
	TypeOps[type].push_back(new_op);

	// TODO : Detect whether or not this is also a Constructor.
	return new_op;
}


Operation* _search_vocabulary(const std::string& op_name, const std::vector<Operation*>& list)
{
	std::vector<Operation*> results;

	// Search in reverse order for all Operations with name, 'op_name'.
	std::for_each(list.rbegin(), list.rend(), [op_name, results](Operation* op) {if(op->name == op_name) results.push_back(op);} );

	return (Operation*) 0;	
}

// find -	Returns an Operation given a name based on the context of a stack and
//			the Operation's type signature if such exists. Operations with the
//			longest type signature have priority.
Operation* Operation::find(const std::string& op_name, const Stack<StackObject>& stack)
{
	Operation* op = 0;

	// Default to global 'Any' Type vocabulary.
	Type::ID type = 0;
	try
	{
		// Does the top of our stack indicate a Type vocabulary we should check first/
		type = stack.tos().first.id;
	}
	catch( const Stack<StackObject>::Underflow& x ) {;} // Empty stack means Global 'Any' vocabulary.




	return op;
}

