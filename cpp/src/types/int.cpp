//
//	int.cpp -	Operational implementations of Int type in ActorForth.
//

#include "continuation.hpp"
#include "types/int.hpp"

namespace ActorForth
{

void _string_int_ctor( Continuation& c )
{
	const std::string& word = std::get<std::string>(c.stack.tos().value);
	int i = std::stoi(word);

	// BDM HACK - 	C++ accepts a lot of stuff with text as numerics.
	//				This needs to be improved to be more "correct"
	//				so words like 2dup don't get treated as numbers.
	if(word.size()==std::to_string(i).size())
	{
		c.stack.pop();
		c.stack.push( StackObject( Int, i ) );
		//std::cout << "\tFound a Int : " << c.stack.tos() << "." << std::endl;
		return;
	}
	throw std::invalid_argument("");
}

Operation* const op_atom_int = Operation::add("int", {}, { {Atom}, {Int} }, _string_int_ctor, true);
Operation* const op_string_int = Operation::add("int", {}, { {String}, {Int} }, _string_int_ctor, true);



}
