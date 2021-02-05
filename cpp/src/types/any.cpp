//
//	any.cpp	-	Operation implementations for Any type. 
//				This is the GLOBAL dictionary.
//

#include "continuation.hpp"
#include "types/any.hpp"

namespace ActorForth
{

void _interpret( Continuation& c )
{
	// BDM - 	call interpret inside the interpreter immediately goes into an infinite loop.
	// TODO -	consider putting the tokens stream in the Continuation and allowing
	//			devs to update the token stream and/or write their own interpreter word
	//			to act as a little DSL.
	//std::cout << "Interpreting : " << c.token << std::endl;

	const std::string& word = c.token.value;

	// Initially ever token starts as an Atom.
	c.stack.push( StackObject( Atom, word ) );

	//std::cout << "tos = " << c.stack.tos() << std::endl;

	// First check if it's an integer.
	try
	{
		// return if successful.
		int i = std::stoi(word);
		//int i = std::stoi(std::get<std::string>(c.stack.tos().value));
		//std::cout << "int i = " << i << std::endl;

		// BDM HACK - 	C++ accepts a lot of stuff with text as numerics.
		//				This needs to be improved to be more "correct"
		//				so words like 2dup don't get treated as numbers.
		if(word.size()==std::to_string(i).size())
		{		
			//std::cout << "updating the stack with the int." << std::endl;	
			c.stack.pop();
			c.stack.push( StackObject( Int, i ) );
			//std::cout << "\tFound a Int : " << c.stack.tos() << "." << std::endl;
			return;
		}
	}	
	catch( const std::invalid_argument& ) {;}
	catch( const std::out_of_range& ) {;}

	// Next check if it's a boolean.
	if(word == "true" or word == "false") 
	{
		c.stack.pop();
		c.stack.push( StackObject( { Bool, (word == "true") ? true : false} ) );
		//std::cout << "\tFound a Bool : " << c.stack.tos() << "." << std::endl;
		return;
	}

	// Finally check if it's an existing value word.	
	// BDM TODO - should probably make find a regular operator
	//			  that knows to get the Atom at tos() out of the way.
	auto tmp = c.stack.tos(); c.stack.pop();
	ActorForth::Operation* op = ActorForth::Operation::find(word, c.stack);
	if(op)
	{
		//std::cout << "\tFound an Operation : " << *op << ". EXECUTING!" << std::endl;
		c.op = op;
		c.execute( c );
		return;
	}

	// Otherwise create an Atom.
	c.stack.push(tmp);
	std::cout << "?\n";
	//std::cout << "\tFound an Atom : " << c.stack.tos() << "." << std::endl;
}

Operation* const op_interpret = ActorForth::Operation::add("interpret", {}, Signature(), _interpret, true);


void _print(Continuation& c)
{
	const StackObject& o = c.stack.tos();
	std::cout << o.type << ", " << o.value;
	c.stack.pop();
}

Signature s = { {Any}, {} };

Operation* const op_print = Operation::add("print", {}, { {Any}, {} }, _print, true);
Operation* const op_empty_print = Operation::add("print", {}, {{},{}}, [](Continuation&) {std::cout<<"<empty stack>\n";}, true);


void _stack(Continuation& c)
{
	size_t count = 0;
	if(not c.stack.depth()) std::cout << "<empty>" << std::endl;
	for(auto item=c.stack.rbegin();item!=c.stack.rend();++item)
	{
		const StackObject& o = *item;
		if(not count) std::cout << "TOS: \t"; else std::cout << "\t";
		std::cout << std::setw(2) << std::dec << count++ << " :\t" << o.value << "\t: " << o.type.name << std::endl;
	}
}

Operation* const op_stack = Operation::add("/s", {}, {{},{}}, _stack, true);

Operation* const op_depth = Operation::add("/d", {}, {{},{}}, [](Continuation& c) { c.stack.push( { Int, static_cast<int>(c.stack.depth()) } ); }, true );

// BDM TODO - 	move this under Type.cpp once we get the ctor declared for 
//				putting a Type on the stack in the first place. Then can 
//				refactor _print_words in terms of this one.
void _print_type_words( Continuation& c )
{
	const Type& t = c.stack.tos().type;
	const std::vector<Operation*>& ops = Operation::TypeOps[t.id];
	std::cout << "\n\t" << t.name << ":";
	for( auto op=ops.rbegin(); op!=ops.rend(); ++op)
	{
		const Signature& s = (**op).sig;
		std::cout << "\n\t\t" << (**op).name << " :" << s.in_seq << " -> " << s.out_seq;
		//++count;
	}
	c.stack.pop();
}


void _print_words( Continuation& c )
{
	(void)c;
	size_t count = 0;
	std::cout << "ActorForth Dictionaries:" << std::endl;
	for( size_t id = 0; id < Type::size(); ++id )
	{
		const Type& t = Type::from_id(id);
		const std::vector<Operation*>& ops = Operation::TypeOps[id];
		
		if(ops.size()==0) continue;

		std::cout << "\n\t" << t.name << ":";
		for( auto op=ops.rbegin(); op!=ops.rend(); ++op)
		{
			const Signature& s = (**op).sig;
			std::cout << "\n\t\t" << (**op).name << " : ";
			s.display(std::cout);
			++count;
		}
	}
	std::cout << "\n" << count << " total words." << std::endl;
}

Operation* const op_type_words = Operation::add("words", {}, {{IType},{}}, _print_type_words, true);
Operation* const op_type_words_short = Operation::add("/w", {}, {{IType},{}}, _print_type_words, true);
Operation* const op_words = Operation::add("words", {}, {{},{}}, _print_words, true);
Operation* const op_words_short = Operation::add("/w", {}, {{},{}}, _print_words, true);

void _print_types( Continuation& c )
{
	(void)c;
	std::cout << "ActorForth Types:\n";
	for( size_t id = 0; id < Type::size(); ++id )
	{
		std::cout << "\t" << Type::from_id(id) << "\n";
	}
}

Operation* const op_types = Operation::add("types", {}, {{},{}}, _print_types, true);
Operation* const op_types_short = Operation::add("/t", {}, {{},{}}, _print_types, true);


Operation* const op_dup = Operation::add("dup", {}, {{Any},{Any, Any}}, [](Continuation& c) {c.stack.push(c.stack.tos());}, true );
Operation* const op_drop = Operation::add("drop", {}, {{Any},{}}, [](Continuation& c) {c.stack.pop();}, true );

// BDM TODO : use C++ swap as an efficient alternative.
Operation* const op_swap = Operation::add("swap", {}, {{Any, Any},{Any, Any}}, [](Continuation& c) {const StackObject o = c.stack.tos(); c.stack.pop(); const StackObject j = c.stack.tos(); c.stack.pop(); c.stack.push(o); c.stack.push(j);}, true );
Operation* const op_2dup = Operation::add("2dup", {}, {{Any, Any},{Any, Any, Any, Any}}, [](Continuation& c) {const StackObject o = c.stack.tos(); (*op_swap)(c); const StackObject j = c.stack.tos(); (*op_swap)(c); c.stack.push(j); c.stack.push(o); }, true );

// BDM TODO 	- Need to confirm the Types are identical (convertible via ctors?) before allowing an assignment.
Operation* const op_assign = Operation::add("=", {}, {{Any, Any},{Any}}, [](Continuation& c) {StackObject o = c.stack.tos(); c.stack.pop(); c.stack.pop(); c.stack.push(o);}, true );

}