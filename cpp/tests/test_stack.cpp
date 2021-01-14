//
//	test_stack.cpp	- Test code for Stack class.
//

#include <iostream>

#include "stack.hpp"

int main(void)
{
	std::vector<int> v(1,12);
	std::vector<int> w;

	std::cout << "My vector<int> : ";
	for( auto n: v) std::cout << n << std::endl;
	std::cout << "It's capacity is " << v.capacity() << std::endl;
	std::cout << "The other vector's capacity is " << w.capacity() << std::endl;

	Stack<char> stack;
	std::cout << "Made a stack!" << std::endl;
	//std::cout << "It's capacity is " << stack._stack.capacity() << std::endl;
	return 0;
}
