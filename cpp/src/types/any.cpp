//
//	any.cpp	-	Operation implementations for Any type. 
//				This is the GLOBAL dictionary.
//

#include "continuation.hpp"
#include "types/any.hpp"

void _print(Continuation& c)
{
	std::cout << c.stack.tos();
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
		if(not count) std::cout << "TOS: \t"; else std::cout << "\t";
		std::cout << std::setw(2) << std::dec << count++ << "\t" << *item << std::endl;
	}
}

Operation* const op_stack = Operation::add("/s", {}, {{},{}}, _stack, true);

Operation* const op_depth = Operation::add("/d", {}, {{},{}}, [](Continuation& c) { c.stack.push( { Int, static_cast<int>(c.stack.depth()) } ); }, true );

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
			std::cout << "\n\t\t" << **op;
			++count;
		}
	}
	std::cout << "\n" << count << " total words." << std::endl;
}

Operation* const op_words = Operation::add("words", {}, {{},{}}, _print_words, true);
