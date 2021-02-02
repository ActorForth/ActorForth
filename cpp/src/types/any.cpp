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