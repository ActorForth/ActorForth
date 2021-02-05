//
// 	repl.cpp	- top level read/eval/print loop for ActorForth.
//

#include <iostream>
#include <filesystem>

#include "repl.hpp"

#include "type.hpp"
using namespace Types;
using namespace ActorForth;


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
	if( argc > 1 and std::filesystem::exists(argv[1])) return Parser(argv[1]);
	if( argc > 1) {
			std::cout << "\n ERROR - file '" << argv[1] << "' not found." << std::endl;
			exit(-1); }
	std::cout << "ActorForth ready. ^D to exit." << std::endl;
	
	// Defaults to reading from std::cin.
	return Parser();
}


int main(int argc, char *argv[])
{
	using namespace ActorForth;
	Types::initialize();

	//std::cout << "Startup" << std::endl;
	//std::cout << "op_interpret got address " << (void*) op_interpret << std::endl;

	if (!op_interpret or !op_nop) { std::cout << "ERROR primitive ops not ready!" << std::endl; exit(-1); }
	if(argc > 1 and std::string(argv[1]) == "--help") return help(argv);

	Stack<StackObject> dstack;
	Stack<StackObject> rstack;
	Continuation cont = { dstack, rstack, op_nop, Parser::Token() };

	Parser input = open_file(argc,argv);
	
	/*
	std::cout << "Any    = " << Types::Any << std::endl;
	std::cout << "Int    = " << Types::Int << std::endl;
	std::cout << "Bool   = " << Types::Bool << std::endl;
	std::cout << "Atom   = " << Types::Atom << std::endl;
	std::cout << "String = " << Types::String << std::endl;
	*/
	
	while(input.good())
	{
		// For stdin we write an acknowledgement prompt as a Forthwright should expect.
		if(input.is_stdin()) std::cout << "ok ";

		for(auto n: input.tokens())
		{
			// Pass control to the interpreter.
			cont.op = op_interpret;
			cont.token = n;
			cont.execute( cont );
		}		
	}

	int result = 0;

	// If there's an int at tos then we'll make that our return value.
	if(cont.stack.depth() and cont.stack.tos().type == Int)
	{
		result = std::get<int>(cont.stack.tos().value);
	}

	std::cout << "\nStack remaining : ";
	size_t count = 0;
	if(not cont.stack.depth()) std::cout << "<empty>" << std::endl;
	while(cont.stack.depth())
	{
		if(not count) std::cout << "\n\tTOS: \t"; else std::cout << "\t\t";
		const StackObject& so = cont.stack.tos();
		std::cout << std::setw(2) << std::dec << count++ << "\t" << so << std::endl;
		cont.stack.pop();		
	}
	std::cout << "\nend of line..." << std::endl;

	return result;
}
