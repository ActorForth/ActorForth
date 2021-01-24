//
// parser.cpp	- parser implementation for ActorForth
//

#include "parser.hpp"

Parser::Parser(std::istream&& is) 
{
	i = is;
	input = &i;
}

Parser::Parser(const std::string filename ) 
	: 	f( std::ifstream(filename, std::ios::binary) ),
		input(0),
		location(filename)
{ 
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
}

void Parser::FilePosition::update(const char c)
{
	switch(c)
	{
		case '\n' :
			linenumber +=1;
			column = 1;
			break;
		case '\t' :
			column += 4;
			break;
		default:
			column += 1;
	}
}

Parser::StateMaybeToken Parser::Whitespace::consume(const char c, const FilePosition& pos)
{
	if(isspace(c)) return { *this, {} };
	if(c=='.' or c==';' or c==':') return { Whitespace(), Token(c,pos) };
	if(c=='"') return { String(pos), {} };
	if(c=='#') return { Comment(), {} };
	return { Characters(c, pos), {} };
}

Parser::StateMaybeToken Parser::Characters::consume(const char c, const FilePosition& pos)
{
	if(isspace(c)) return { Whitespace(), token };
	if(c=='"') return { String(pos), token };
	if(c=='.' or c==';' or c==':') return { Characters(c, pos), token };
	if(c=='#') return { Comment(), token };
	token.value.push_back(c);
	return { *this, {} };
}

Parser::StateMaybeToken Parser::String::consume( const char c, const FilePosition& pos)
{			
	if(c=='"') return { Whitespace(), token };
	token.value.push_back(c);
	return { *this, {} };
}

Parser::StateMaybeToken Parser::Comment::consume(const char c, const FilePosition& pos)
{
	if(c=='\n') return { Whitespace(), {} };
	return { *this, {} };
}

generator<Parser::Token> Parser::tokens()
{
	//if(! input) std::cerr << "Input file not valid." << std::endl; return;
	State state = Whitespace();
	char c = input->get();
	do
	{
		std::optional< Token > maybe_token;
		std::tie(state, maybe_token) = std::visit([&](auto&& sarg) { return sarg.consume(c, location); }, state);

		if (maybe_token.has_value()) co_yield( maybe_token.value() );
		location.update(c);

		c = input->get();

	} while (not input->eof());
}

std::ostream& operator<<(std::ostream& out, const Parser::Token& token)
{
	out << "'" << token.value << "'" << "\t\t\t[ file : " << token.location.filename 
	    << ", line: " << token.location.linenumber << ", col: " << token.location.column << " ]";
	return out;
}
