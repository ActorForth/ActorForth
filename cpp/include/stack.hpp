//
//	stack.hpp	- Stack declaration for ActorForth.
//

#pragma once

#include <stdexcept>
#include <variant>
#include <utility> // std::pair
#include <optional>
#include <vector>

template <class T> class Stack
{
public:
	struct Underflow : public std::out_of_range {;};

	struct MaybeEmpty;
	using StackResult = std::pair<MaybeEmpty, T&>;
	struct NonEmpty;
	struct Empty
	{
		T& tos(void) const { throw Underflow(); }	
		T& pop(void) const { throw Underflow(); }
		NonEmpty push( const T& value) { return NonEmpty(value); }
	};

	struct NonEmpty
	{
		NonEmpty( const T& value ) {;}
	};

	using MaybeEmpty = std::variant<Empty, NonEmpty>;

	Stack(void) : _stack(Empty()) {;} // { _stack = Empty();}


private:
	MaybeEmpty _stack;
};