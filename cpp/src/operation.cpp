//
//	operation.cpp	-	Definition for Operation in ActorForth.
//

#include <doctest/doctest.h>

#include <variant>

#include <operation.hpp>
#include <continuation.hpp>

// Holds the global vocabularies of all Operations for each Type.
std::map<Type::ID,std::vector<Operation*>> Operation::TypeOps;

// Holds all of the Constructors for various Types.
std::map<Type::ID,std::vector<const Operation*>> Operation::TypeCtors;

// TODO : Implement these for real!!
bool Signature::matches(const Stack<StackObject>& sobject) const 
{ 
	// If the Signature is longer than the size of the Stack then we're not a match.
	if(in_seq.depth() > sobject.depth()) return false;

	return false; 
}

bool Signature::matches(const Stack<StackSig>& sig) const 
{ 
	// Input Signature's must be of identical length.
	if(in_seq.depth() != sig.depth()) return false;

	return false; 
}

// add -	Adds a new Operation to the appropriate vocabularies based on the
//			stack signature. Also automatically detects and registers constuctors
//			operations that have the same name as the Type except lower case and
//			their only return value is an instance of that Type.
//			Returns 0 if a conflicting Operation already exists.
Operation* Operation::add(const std::string& name, const Parser::Token& token, const Signature& sig, const Type::Handler& h, const bool force_global )
{
	// Which Type vocabulary does it belong?
	Type::ID type = 0; // Default to Global 'Any' vocabulary.
	try
	{
		// If we're not forcing global 'Any' scope then see if there's a stack context
		// which will indicate another Type's vocabulary to store this Operation into.
		if(not force_global) type = sig.in_seq.tos().first.id;
	}
	catch( const Stack<StackSig>::Underflow& x ) {;} // Empty stack means Global 'Any' vocabulary.

	// Check to see the operation doesn't already exist first.
	if(Operation::find(name, sig.in_seq)) return 0;

	// Once created an Operation is never deleted.
	Operation* new_op = new Operation(name, token, sig, h);

	// Insert Operation into type vocabulary.
	TypeOps[type].push_back(new_op);

	// TODO : Detect whether or not this is also a Constructor.
	return new_op;
}

template<class T> Operation* _search_vocabulary(const std::string& op_name, const Stack<T>& stack, const std::vector<Operation*>& list)
{
	Operation* result = 0;
	std::vector<Operation*> results;

	// Search in reverse order for all Operations with name, 'op_name'.
	std::for_each(list.rbegin(), list.rend(), [&op_name, &result, &stack](Operation* op) 
	{
		if(op->name != op_name) return;

		// Do we have a signature match with the stack?
		if(not op->sig.matches(stack)) return;

		// Is this the longest match we've found?
		if(result and op->sig.in_seq.depth() > result->sig.in_seq.depth()) result = op;
	} );

	return result;	
}

// find -	Returns an Operation given a name based on the context of a stack and
//			the Operation's type signature if such exists. Operations with the
//			longest type signature have priority.
template<class T> Operation* Operation::find(const std::string& op_name, const Stack<T>& stack)
{
	// Default to global 'Any' Type vocabulary.
	Type::ID type = 0;
	try
	{
		// Does the top of our stack indicate a Type vocabulary we should check first/
		type = stack.tos().first.id;
	}
	catch( const Stack<StackObject>::Underflow& x ) {;} // Empty stack means Global 'Any' vocabulary.

	Operation* op = _search_vocabulary(op_name, stack, TypeOps[type]);

	// Was this result from the global vocabulary? If so we're done.
	if(type == 0) return op;

	// Check and see if we have a better alternative in the global 'Any' vocabulary.
	Operation* global_op = _search_vocabulary(op_name, stack, TypeOps[0]);

	// If there is only a global result then return it.
	if(not op and global_op) return global_op;

	// If there is both a specialized op and global op return the one with longest 
	// type signature, prioritizing the specialized one if equal.
	if(op and global_op) return (op->sig.in_seq.depth() < global_op->sig.in_seq.depth()) ? global_op : op;

	// Seems we didn't find anything.
	return op;
}


Operation& op_nop = *Operation::add("nop",{},Signature(), [](Continuation&) {;});