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

	std::string s = "This is some text from a stringstream (istream).";

	Parser parse_istream(std::stringstream(s);

	for(auto n: parse_istream.tokens())
	{
		//std::cout << codetext.get();
		std::cout << n << std::endl;
	}

	return 0;
}
