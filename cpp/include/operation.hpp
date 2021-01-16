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

#include "parser.hpp"
#include "type.hpp"
#include "stack.hpp"

class Continuation;

using StackSig = std::pair< Type,std::optional<std::any> >;
using StackObject = std::pair< Type,std::any >;

struct Signature
{
	Stack<StackSig> in_seq;
	Stack<StackSig> out_seq;
};

class Operation
{
public:


	Operation(const std::string name, Type::Handler h = Type::default_handler) : name(name), handler(h) {;}

	// How do we const this?
	void operator()(Continuation& c) {handler(c);}

private:
	const std::string name;
	Type::Handler handler;
	std::vector<const Operation> words;
	const Parser::Token token;
	Signature sig;
};
