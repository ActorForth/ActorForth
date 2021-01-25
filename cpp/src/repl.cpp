//
// 	repl.cpp	- top level read/eval/print loop for ActorForth.
//

#include <iostream>
#include <filesystem>

#include "repl.hpp"


int help(char *argv[])
{
	using namespace std;
	cout << "help" << endl;
	cout << "\n" << argv[0] << " <optional: ActorForth File Path/Name> or \n";
	cout << setw(strlen(argv[0])) << "" << " --help for instructions.\n";
	return 0;
}

Parser open_file(int argc, char *argv[])
{
	// Open a file if we can find it.
	if( argc > 1)
	{
		if(std::filesystem::exists(argv[1]))
		{
			return Parser(argv[1]);
		}
		else
		{
			std::cout << "\n ERROR - file '" << argv[1] << "' not found." << std::endl;
			exit(-1);
		}
	}

	std::cout << "ActorForth ready. ^D to exit." << std::endl;
	
	// Defaults to reading from std::cin.
	return Parser();
}

int main(int argc, char *argv[])
{
	if(argc > 1 and std::string(argv[1]) == "--help") return help(argv);

	Stack<StackObject> dstack;
	Stack<StackObject> rstack;
	Continuation cont = { dstack, rstack, op_nop };

	Parser input = open_file(argc,argv);

	while(input.good())
	{
		std::cout << "ok ";

		for(auto n: input.tokens())
		{
			//std::cout << codetext.get();
			//std::cout << n.value << " ";
		}		
	}

	std::cout << "\nend of line..." << std::endl;

	return 0;
}