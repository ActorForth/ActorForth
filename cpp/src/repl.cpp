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
	if( argc > 1 and std::filesystem::exists(argv[1])) return Parser(argv[1]);
	if( argc > 1) {
			std::cout << "\n ERROR - file '" << argv[1] << "' not found." << std::endl;
			exit(-1); }
	std::cout << "ActorForth ready. ^D to exit." << std::endl;
	
	// Defaults to reading from std::cin.
	return Parser();
}

void _interpret( Continuation& c )
{
	// BDM - 	call interpret inside the interpreter immediately goes into an infinite loop.
	// TODO -	consider putting the tokens stream in the Continuation and allowing
	//			devs to update the token stream and/or write their own interpreter word
	//			to act as a little DSL.
	std::cout << "Interpreting : " << c.token << std::endl;

	const std::string& word = c.token.value;

	// First check if it's an integer.
	try
	{
		// return if successful.
		int i = std::stoi(word);
		c.stack.push( StackObject( Int, i ) );
		std::cout << "\tFound a Int : " << c.stack.tos() << "." << std::endl;
		return;
	}	
	catch( const std::invalid_argument& ) {;}
	catch( const std::out_of_range& ) {;}

	// Next check if it's a boolean.
	if(word == "true" or word == "false") 
	{
		c.stack.push( StackObject( { Bool, (word == "true") ? true : false} ) );
		std::cout << "\tFound a Bool : " << c.stack.tos() << "." << std::endl;
		return;
	}

	// Finally check if it's an existing value word.	
	Operation* op = Operation::find(word, c.stack);
	if(op)
	{
		std::cout << "\tFound an Operation : " << *op << ". EXECUTING!" << std::endl;
		c.op = op;
		c.execute( c );
		return;
	}

	// Otherwise create an Atom.
	c.stack.push( StackObject( Atom, word ) );
	std::cout << "\tFound an Atom : " << c.stack.tos() << "." << std::endl;
}

int main(int argc, char *argv[])
{
	//std::cout << "Startup" << std::endl;
	Operation* const op_interpret = Operation::add("interpret", {}, Signature(), _interpret, true);
	//std::cout << "op_interpret got address " << (void*) op_interpret << std::endl;

	if (!op_interpret or !op_nop) { std::cout << "ERROR primitive ops not ready!" << std::endl; exit(-1); }
	if(argc > 1 and std::string(argv[1]) == "--help") return help(argv);

	Stack<StackObject> dstack;
	Stack<StackObject> rstack;
	Continuation cont = { dstack, rstack, op_nop, Parser::Token() };

	Parser input = open_file(argc,argv);

	std::cout << "Any    = " << Any << std::endl;
	std::cout << "Int    = " << Int << std::endl;
	std::cout << "Bool   = " << Bool << std::endl;
	std::cout << "Atom   = " << Atom << std::endl;
	std::cout << "String = " << String << std::endl;

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

	return 0;
}
