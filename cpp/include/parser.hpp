//
// parser.hpp	- parser implementation for ActorForth
//

#pragma once

#include <ctype.h>

#include <iostream>
#include <variant>
#include <optional>
#include <vector>
#include <utility> // std::pair
#include <string>
#include <fstream>
#include <sstream>

#include <experimental/coroutine>

#include "generator.hpp"

using felspar::makham::generator;

class Parser
{
public:
	Parser(const std::string filename );

	struct FilePosition
	{
		FilePosition() : filename("=Unknown="), linenumber(1), column(1) {;}
		FilePosition(const std::string& name) : filename(name), linenumber(1), column(1) {;}
		void update(const char c);

		std::string filename;
		unsigned linenumber;
		unsigned column;
	};

	struct Token
	{
		Token() {;}
		Token(const char c, const FilePosition& pos) 
			{ value.push_back(c); location = pos; }
		std::string value;
		FilePosition location;
	};

	struct Whitespace;
	struct Characters;
	struct String;
	struct Comment;

	using State = std::variant<Whitespace, Characters, String, Comment>;
	using StateMaybeToken = std::pair<State, std::optional<Token> >;

	struct Whitespace 
	{		
		StateMaybeToken consume(const char c, const FilePosition& pos);
	};

	struct Characters
	{	
		Characters(char c, const FilePosition& pos)
			{ token.value.push_back(c); token.location = pos; }

		StateMaybeToken consume(const char c, const FilePosition& pos);

		Token token;
	};

	struct String
	{
		String(const FilePosition& pos) { token.location = pos; }
		StateMaybeToken consume( const char c, const FilePosition& pos);

		Token token;
	};

	struct Comment
	{
		StateMaybeToken consume(const char c, const FilePosition& pos);
	};

	generator<Token> tokens()
	{
		//if(! input) std::cerr << "Input file not valid." << std::endl; return;
		State state = Whitespace();
		char c = input.get();
		do
		{
			std::optional< Token > maybe_token;
			std::tie(state, maybe_token) = std::visit([&](auto&& sarg) { return sarg.consume(c, location); }, state);

			if (maybe_token.has_value()) co_yield( maybe_token.value() );
			location.update(c);

			c = input.get();

		} while (not input.eof());
	}

private:
	//std::variant<std::ifstream, std::istringstream> input;
	std::ifstream input;

	FilePosition location;
};

std::ostream& operator<<(std::ostream& out, const Parser::Token& token);
