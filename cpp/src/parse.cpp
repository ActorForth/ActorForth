//
// parse.cpp	- parser implementation for ActorForth
//

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
	Parser(const std::string filename ) 
		: input( std::ifstream(filename, std::ios::binary) ),
		  location(filename)
		{ 
			try 
			{
        		input.exceptions(input.failbit);        		
    		} 
    		catch (const std::ios_base::failure& e)
    		{
        		std::cout 	<< "Caught an ios_base::failure.\n"
                  			<< "Explanatory string: " << e.what() << '\n'
                  			<< "Error code: " << e.code() << '\n';
				throw;                  			
    		}
    		input.exceptions(input.badbit);
		}

	struct FilePosition
	{
		FilePosition() : filename("=Unknown="), linenumber(1), column(1) {;}
		FilePosition(const std::string& name) : filename(name), linenumber(1), column(1) {;}
		void update(const char c)
		{
			switch(c)
			{
				case '\n' :
					linenumber +=1;
					column = 1;
					break;
				case '\t' :
					column += 4;
					break;
				default:
					column += 1;
			}
		}
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
		StateMaybeToken consume(const char c, const FilePosition& pos)
		{
			if(isspace(c)) return { *this, {} };
			if(c=='.' or c==';' or c==':') return { Whitespace(), Token(c,pos) };
			if(c=='"') return { String(pos), {} };
			if(c=='#') return { Comment(), {} };
			return { Characters(c, pos), {} };
		}
	};

	struct Characters
	{	
		Characters(char c, const FilePosition& pos)
			{ token.value.push_back(c); token.location = pos; }

		StateMaybeToken consume(const char c, const FilePosition& pos)
		{
			if(isspace(c)) return { Whitespace(), token };
			if(c=='"') return { String(pos), token };
			if(c=='.' or c==';' or c==':') return { Characters(c, pos), token };
			if(c=='#') return { Comment(), token };
			token.value.push_back(c);
			return { *this, {} };
		}

		Token token;
	};

	struct String
	{
		String(const FilePosition& pos) { token.location = pos; }
		StateMaybeToken consume( const char c, const FilePosition& pos)
		{
			if(c=='"') return { Whitespace(), token };
			token.value.push_back(c);
			return { *this, {} };
		}

		Token token;
	};

	struct Comment
	{
		StateMaybeToken consume(const char c, const FilePosition& pos)
		{
			if(c=='\n') return { Whitespace(), {} };
			return { *this, {} };
		}
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

std::ostream& operator<<(std::ostream& out, const Parser::Token& token)
{
	out << "'" << token.value << "'" << "\t\t\t[ file : " << token.location.filename 
	    << ", line: " << token.location.linenumber << ", col: " << token.location.column << " ]";
	return out;
}

int main()
{
	//std::string const name = "../../../develop/cpp/data/SampleTextFile_1000kb.txt"; // "tests/data/parseme.a4";
	std::string const name = "tests/data/parseme.a4";
	Parser codetext(name); 

	for(auto n: codetext.tokens())
	{
		//std::cout << codetext.get();
		std::cout << n << std::endl;
	}

	return 0;
}
