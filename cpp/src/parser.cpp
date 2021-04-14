//
// parser.cpp	- parser implementation for ActorForth
//

#include "parser.hpp"

Parser::Parser(void) : input(&std::cin), location(Types::FSPosition) 
{
	std::string attrib = "filename";
	Types::AnyValue& x = location[attrib];
	std::string& f = std::get<std::string>(x);
	f = std::string("=Unknown=");
	//std::get<std::string&>(location["filename"])= std::string("=Unknown=");
	std::get<int>(location["linenumber"])=1;
	std::get<int>(location["column"])=1;
}

Parser::Parser(const std::string filename ) 
	: 	f( std::ifstream(filename, std::ios::binary) ),
		input(0),
		location(Types::FSPosition) 
		//location(std::move(filename))
{ 
	std::cout << "Parser(filename = " << filename << ")." << std::endl;

	std::cout << "Setting location filename." << std::endl;
	Types::AnyValue fn = filename;
	
	std::cout << "fn = " << fn << std::endl;
	location["filename"] = fn;

	std::cout << "Setting linenumber position." << std::endl;
	location["linenumber"]=1;
	std::cout << "Setting column position." << std::endl;
	location["column"]=1;
	try 
	{
  		f.exceptions(f.failbit);
	} 
	catch (const std::ios_base::failure& e)
	{
   		std::cout 	<< "Caught an ios_base::failure.\n"
           			<< "Explanatory string: " << e.what() << '\n'
           			<< "Error code: " << e.code() << '\n';
		throw;                  			
    }
    f.exceptions(f.badbit);
    input = &f;

    std::cout << "Parser ctor return." << std::endl;
}


Parser::Parser(const std::string filename, const std::string content)
	: 	s(content),
		location(Types::FSPosition) 
		//input(0),
		// location(std::move(filename))
{
	std::get<std::string>(location["filename"])=filename;
	std::get<int>(location["linenumber"])=1;
	std::get<int>(location["column"])=1;
	//s = std::stringstream();
	//std::cout << "Parser sstream ctor." << std::endl;
	//s << content ;
	//std::cout << "Streamed in the content." << std::endl;
	input = &s;
	//std::cout << "Input pointer set." << std::endl;
}


void Parser::update_pos(const char c)
{
	int linenumber = std::get<int>(location["linenumber"]);
	int column = std::get<int>(location["column"]);
	switch(c)
	{
		case '\n' :
			std::get<int>(location["linenumber"])=linenumber + 1;
			std::get<int>(location["column"]) = 1;
			break;
		case '\t' :
			std::get<int>(location["column"]) = column + 4;
			break;
		default:
			std::get<int>(location["column"]) = column + 1;
	}
}

Parser::StateMaybeToken Parser::Whitespace::consume(const char c, const Types::ProductInstance& pos)
{
	if(isspace(c)) return { *this, {} };
	if(c=='.' or c==';' or c==':') return { Whitespace(), Token(c,pos) };
	if(c=='"') return { String(pos), {} };
	if(c=='#') return { Comment(), {} };
	return { Characters(c, pos), {} };
}

Parser::StateMaybeToken Parser::Characters::consume(const char c, const Types::ProductInstance& pos)
{
	//std::cout << "Characters::consume with '" << c << "'" << std::endl;
	if(isspace(c)) return { Whitespace(), token };
	if(c=='"') return { String(pos), token };
	if(c=='.' or c==';' or c==':') return { Characters(c, pos), token };
	if(c=='#') return { Comment(), token };
	token.value.push_back(c);
	return { *this, {} };
}

Parser::StateMaybeToken Parser::String::consume( const char c, const Types::ProductInstance& pos)
{			
	if(c=='"') return { Whitespace(), token };
	token.value.push_back(c);
	return { *this, {} };
}

Parser::StateMaybeToken Parser::Comment::consume(const char c, const Types::ProductInstance& pos)
{
	if(c=='\n') return { Whitespace(), {} };
	return { *this, {} };
}

generator<Parser::Token> Parser::tokens()
{
	//if(! input) std::cerr << "Input file not valid." << std::endl; return;
	State state = Whitespace();
	char c;

	using namespace std;
	//cout << "Tokens read char '" << c << "'." << endl;

	std::optional< Token > maybe_token;
	while(input->get(c))
	{
		//std::optional< Token > maybe_token;
		std::tie(state, maybe_token) = std::visit([&](auto&& sarg) { return sarg.consume(c, location); }, state);

		if (maybe_token.has_value()) co_yield( maybe_token.value() );
		update_pos(c);

		// If we're reading from std::cin we'll only pull in one line at a time
		// unless we're already inside a string.
		if(is_stdin() and c == '\n' and not std::holds_alternative<String>(state)) break;
		//cout << "Tokens read char '" << c << "'." << endl;
	}
	// Anything left over? Throw in a line feed to find out.
	// BDM NOTE - open strings still won't be completed. What to do?
	std::tie(state, maybe_token) = std::visit([&](auto&& sarg) { return sarg.consume('\n', location); }, state);
	if (maybe_token.has_value()) co_yield( maybe_token.value() );
}

std::ostream& operator<<(std::ostream& out, const Parser::Token& token)
{
	out << "'" << token.value << "'" << " [ file : " << std::get<std::string>(token.location["filename"]) 
	    << ", line: " << std::get<int>(token.location["linenumber"]) << ", col: " << std::get<int>(token.location["column"]) << " ]";
	return out;
}
