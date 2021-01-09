//
// parse.cpp	- parser implementation for ActorForth
//

#include <iostream>
#include <variant>
#include <vector>
#include <string>
#include <fstream>

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
		const std::string filename;
		unsigned linenumber;
		unsigned column;
	};

	struct Token
	{
		const std::string value;
		const FilePosition location;
	};

	generator<Token> tokens()
	{
		while(true)
		{
			if (not input) break;
			char n = input.get();
			if (n=='\n')
			{
				location.column = 1;
				Token result = {"\n", location};
				co_yield result;
				location.linenumber += 1;
			}

		}
	}

private:
	//std::variant<std::ifstream, std::istringstream> input;
	std::ifstream input;



	FilePosition location;
};

int main()
{
	const std::string name = "tests/data/parseme.a4";
	Parser codetext(name); 

	while(codetext)
	{
		std::cout << codetext.get();
	}

	return 0;
}