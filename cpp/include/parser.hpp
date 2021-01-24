//
// 	parser.hpp	- Parser declaration for ActorForth.
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
	
	Parser(std::istream&& is);
	Parser(const std::string filename );
	~Parser() {;}

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
		Token() = default;
		Token(const char c, const FilePosition& pos) 
			{ value.push_back(c); location = pos; }
		std::string value;
		FilePosition location;
	};

	generator<Token> tokens();

private:

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

	union
	{ 
		//_Stream(std::ifstream&& s) : f(s) {;}
		std::istream i; 
		std::ifstream f; 
	};
	std::istream* input;

	FilePosition location;
};

std::ostream& operator<<(std::ostream& out, const Parser::Token& token);
