//
// 	repl.cpp	- top level read/eval/print loop for ActorForth.
//

#include <iostream>

#include "repl.hpp"


int help(char *argv[])
{
	using namespace std;
	cout << "\n" << argv[0] << " <ActorForth File Path/Name> or \n";
	cout << setw(strlen(argv[0])) << "" << " --help for instructions.\n";
}

int main(int argc, char *argv[])
{
	for(int i=0;i<argc;++i)
	{
		std::cout << "argv[" << i << "] = " << argv[i] << std::endl;
	}

	std::string filename="stdin";
	std::istream input = std::cin;

	if(string(argv[1] == "--help")) return help(argv);

	


	return 0;
}