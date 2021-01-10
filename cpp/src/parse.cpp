//
// parse.cpp	- parser implementation for ActorForth
//

#include <iostream>
#include <variant>
#include <vector>
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
	{;}

	char get() {return input.get();}
	operator bool() const {return static_cast<bool>(input);}

	struct FilePosition
	{
		FilePosition() : filename("=Unknown="), linenumber(1), column(1) {;}
		FilePosition(const std::string& name) : filename(name), linenumber(1), column(1) {;}
		std::string filename;
		unsigned linenumber;
		unsigned column;
	};

	struct Token
	{
		std::string value;
		FilePosition location;
	};

	generator<Token> tokens()
	{
		while(true)
		{
			if (not input) break;
			char n = input.get();
			if (n=='\n')
			{
				Token result = {"\\n", location};
				co_yield result;
				location.linenumber += 1;
				location.column = 1;
			}
			else
			{
				std::stringstream s;
				s << n;
				Token result = { s.str(), location};
				co_yield result;
				location.column += 1;
			}

		}
	}

private:
	//std::variant<std::ifstream, std::istringstream> input;
	std::ifstream input;



	FilePosition location;
};

std::ostream& operator<<(std::ostream& out, const Parser::Token& token)
{
	out << token.value << " [ line: " << token.location.linenumber << ", col: " << token.location.column << " ]";
	return out;
}

int main()
{
	const std::string name = "tests/data/parseme.a4";
	Parser codetext(name); 

	for(auto n: codetext.tokens())
	{
		//std::cout << codetext.get();
		std::cout << n << std::endl;
	}

	return 0;
}