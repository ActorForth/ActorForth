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
	template<class T> static Operation* find(const std::string& op_name, const Stack<T>& stack);

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
};

extern Operation& op_nop;