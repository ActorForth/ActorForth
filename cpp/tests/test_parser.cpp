//
// test_parser.cpp
//
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "parser.hpp"

TEST_CASE("Parser")
{
	//std::string const name = "../../../develop/cpp/data/SampleTextFile_1000kb.txt"; 
	//std::string const name = "../../../develop/cpp/data/orig_SampleTextFile_1000b.txt";
	std::string const name = "tests/data/parseme.a4";

	Parser codetext(name); 

	SUBCASE("File parser is ready.")
	{
		CHECK(codetext.good() == true);
	}

	for(auto n: codetext.tokens())
	{
		//std::cout << codetext.get();
		std::cout << n << std::endl;
	}

	SUBCASE("File parser is exhausted.")
	{
		CHECK(codetext.good() == false);
	}

	
	std::string s = "This is some text from a stringstream (istream).";
	
	std::cout << "Here's the string we'll parse: '" << s << "'" << std::endl;
	Parser parse_istream("no file name", s);

	SUBCASE("String parser is ready.")
	{
		CHECK(parse_istream.good() == true);
	}

	std::cout << "Now parse the tokens." << std::endl;
	for(auto n: parse_istream.tokens())
	{
		//std::cout << codetext.get();
		std::cout << n << std::endl;
	}

	SUBCASE("String parser is exhausted.")
	{
		CHECK(parse_istream.good() == false);
	}
	

	std::cout << "Will try to read from cin if anything was passed in." << std::endl;

	Parser stdinput;
	for(auto n: stdinput.tokens())
	{
		//std::cout << codetext.get();
		std::cout << n << std::endl;
	}

	std::cout << "Done reading from cin." << std::endl;
}


