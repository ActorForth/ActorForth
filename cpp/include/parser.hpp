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


#include "type.hpp"

using felspar::makham::generator;

class Parser
{
public:
	
	Parser(void);
	Parser(const std::string filename );
	Parser(const std::string filename, const std::string content);

	~Parser() {;}

	/*
	struct FilePosition
	{
		FilePosition() : filename("=Unknown="), linenumber(1), column(1) {;}
		FilePosition(const std::string& name) : filename(name), linenumber(1), column(1) {;}
		void update(const char c);

		std::string filename;
		unsigned linenumber;
		unsigned column;
	};
	*/

	void update_pos(const char c);

	struct Token
	{
		Token() : location(Types::FSPosition) {;}
		Token(const Types::ProductInstance& pos) : location(pos) {;}
		Token(const char c, const Types::ProductInstance& pos) : location(pos)
			{ value.push_back(c); }
		std::string value;
		Types::ProductInstance location;
	};

	// consult good() to see if potential tokens are available.
	bool good(void) const { return input->good(); }
 
	bool is_stdin(void) const { return input == &std::cin; }

	// Returns tokens one by one until eof or additionally, for cin inputs, until a linefeed is reached.
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
		StateMaybeToken consume(const char c, const Types::ProductInstance& pos);
	};

	struct Characters
	{	
		Characters(char c, const Types::ProductInstance& pos) : token(c,pos) {;}

		StateMaybeToken consume(const char c, const Types::ProductInstance& pos);

		Token token;
	};

	struct String
	{
		String(const Types::ProductInstance& pos) : token(pos) {;}
		StateMaybeToken consume( const char c, const Types::ProductInstance& pos);

		Token token;
	};

	struct Comment
	{
		StateMaybeToken consume(const char c, const Types::ProductInstance& pos);
	};

	// s or f may or may not be active according to the ctor called.
	// DO NOT USE THEM. Only reference the input pointer.
	std::stringstream s;
	std::ifstream f;
	std::istream* input;

	Types::ProductInstance location;
};

std::ostream& operator<<(std::ostream& out, const Parser::Token& token);
