//
//	operation.hpp	-	Declarations for Operation in ActorForth.
//

#pragma once

#include <functional>
#include <vector>
#include <string>

#include "parser.hpp"

class Continuation;

class Operation
{
public:
	using Handler = std::function<void(Continuation&)>;
	static Handler default_handler;

	Operation(const std::string name, Handler h = Operation::default_handler) : name(name), handler(h) {;}

	// How do we const this?
	void operator()(Continuation& c) {handler(c);}

private:
	const std::string name;
	Handler handler;
	std::vector<const Operation> words;
	const Parser::Token token;
	// TODO: TypeSignature goes here!!!
};
