//
// parse.cpp	- parser implementation for ActorForth
//

#include <iostream>
#include <vector>
#include <string>
#include <fstream>

#include <experimental/coroutine>

#include "generator.hpp"

using felspar::makham::generator;

int main()
{
	std::ifstream codetext("tests/data/parseme.a4", std::ios::binary);
	while(codetext)
	{
		std::cout << static_cast<char>(codetext.get());
	}

	return 0;
}