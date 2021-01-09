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
	Parser(const std::string filename ) : input( std::ifstream(filename, std::ios::binary) )
	{;}

	char get() {return input.get();}
	operator bool() const {return static_cast<bool>(input);}


private:
	//std::variant<std::ifstream, std::istringstream> input;
	std::ifstream input;
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