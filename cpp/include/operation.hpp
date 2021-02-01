//
//	operation.hpp	-	Declarations for Operation in ActorForth.
//

#pragma once

#include <functional>
#include <vector>
#include <string>
#include <optional>
#include <utility> // std::pair
#include <any>
#include <iostream>
#include <map>

#include "parser.hpp"
#include "type.hpp"
#include "stack.hpp"

class Continuation;

class Operation
{
public:

	// Calls the actual Operation hanlder to execute Operation.
	void operator()(Continuation& c) const {handler(c);}

	// add -	Adds a new Operation to the appropriate vocabularies based on the
	//			stack signature. Also automatically detects and registers constuctors
	//			operations that have the same name as the Type except lower case and
	//			their only return value is an instance of that Type.
	static Operation* add(const std::string& name, const Parser::Token& token, const Signature& sig, const Type::Handler& h = Type::default_handler, const bool force_global = false);

	// find -	Returns an Operation given a name based on the context of a stack and
	//			the Operation's type signature if such exists. Operations with the
	//			longest type signature have priority.
	template<class T> static Operation* find(const std::string& op_name, const Stack<T>& stack)
	{
		// Default to global 'Any' Type vocabulary.
		Type::ID type = 0;
		try
		{
			// Does the top of our stack indicate a Type vocabulary we should check first/
			type = stack.tos().first.id;
		}
		catch( const Stack<StackSig>::Underflow& x ) {;} // Empty stack means Global 'Any' vocabulary.
		catch( const Stack<StackObject>::Underflow& x ) {;}
		//catch( std::exception& x ) {;}

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

	const std::string name;
	const Parser::Token token;
	Signature sig;

protected:

		Operation(const std::string& name, const Parser::Token& token, const Signature& sig, const Type::Handler& h = Type::default_handler ) 
			: name(name), token(token), sig(sig), handler(h) {;}

		Operation(const Operation&) = delete;
		Operation& operator=(const Operation&) = delete;

private:

	Type::Handler handler;

	// For user defined Operations only. Default handler ignores.
	std::vector<const Operation*> words;

	// Holds the global vocabularies of all Operations for each Type.
	static std::map<Type::ID,std::vector<Operation*>> TypeOps;

	// Holds all of the Constructors for various Types.
	static std::map<Type::ID,std::vector<const Operation*>> TypeCtors;

	template<class T> static Operation* _search_vocabulary(const std::string& op_name, const Stack<T>& stack, const std::vector<Operation*>& list)
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
};

extern Operation* const op_nop;
