//
// 	repl.cpp	- top level read/eval/print loop for ActorForth.
//

#include <iostream>

#include "repl.hpp"

int main(int argc, char *argv[])
{
	for(int i=0;i<argc;++i)
	{
		std::cout << "argv[" << i << "] = " << argv[i] << std::endl;
	}

	return 0;
}