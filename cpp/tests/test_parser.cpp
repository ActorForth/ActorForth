//
// test_parser.cpp
//


#include "parser.hpp"

int main()
{
	//std::string const name = "../../../develop/cpp/data/SampleTextFile_1000kb.txt"; 
	//std::string const name = "../../../develop/cpp/data/orig_SampleTextFile_1000b.txt";
	std::string const name = "tests/data/parseme.a4";

	Parser codetext(name); 

	for(auto n: codetext.tokens())
	{
		//std::cout << codetext.get();
		std::cout << n << std::endl;
	}

	/*
	std::string s = "This is some text from a stringstream (istream).";
	
	std::cout << "Here's the string we'll parse: '" << s << "'" << std::endl;
	Parser parse_istream("no file name", s);

	std::cout << "Now parse the tokens." << std::endl;
	for(auto n: parse_istream.tokens())
	{
		//std::cout << codetext.get();
		std::cout << n << std::endl;
	}
	*/

	std::cout << "Will try to read from cin if anything was passed in." << std::endl;

	Parser stdinput;
	for(auto n: stdinput.tokens())
	{
		//std::cout << codetext.get();
		std::cout << n << std::endl;
	}

	std::cout << "Done reading from cin." << std::endl;

	return 0;
}
