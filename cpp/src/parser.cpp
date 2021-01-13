//
// parser.cpp	- parser implementation for ActorForth
//

#include "parser.hpp"

Parser::Parser(const std::string filename ) 
	: 	input( std::ifstream(filename, std::ios::binary) ),
		location(filename)
{ 
	try 
	{
  		input.exceptions(input.failbit);        		
	} 
	catch (const std::ios_base::failure& e)
	{
   		std::cout 	<< "Caught an ios_base::failure.\n"
           			<< "Explanatory string: " << e.what() << '\n'
           			<< "Error code: " << e.code() << '\n';
		throw;                  			
    }
    input.exceptions(input.badbit);
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

std::ostream& operator<<(std::ostream& out, const Parser::Token& token)
{
	out << "'" << token.value << "'" << "\t\t\t[ file : " << token.location.filename 
	    << ", line: " << token.location.linenumber << ", col: " << token.location.column << " ]";
	return out;
}
