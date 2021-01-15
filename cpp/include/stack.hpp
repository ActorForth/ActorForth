//
//	stack.hpp	- Stack definition for ActorForth.
//

#pragma once

#include <stdexcept>
#include <variant>
#include <vector>

template <class T> class Stack
{
public:

	struct Underflow : public std::out_of_range {;};

	Stack(void) : _stack(Empty()) {;} 
	Stack(const Stack&) = default;

private:

	struct Empty;
	struct NonEmpty;
	using MaybeEmpty = std::variant<Empty, NonEmpty>;

	struct Empty
	{
		T& tos(void) const { throw Underflow(); }	
		MaybeEmpty pop(void) { throw Underflow(); }
		MaybeEmpty push( const T& value ) { return NonEmpty(value); }
		MaybeEmpty push( const T&& value ) { return NonEmpty(value); }
	};

	struct NonEmpty
	{
		NonEmpty( const T& value ) : _data(1,value) {;}
		NonEmpty( const T&& value ) { _data.emplace_back(value); }
		T& tos(void) const { return _data.back(); }	
		MaybeEmpty pop(void) const 
		{ 
			_data.pop_back(); 
			if(_data.empty()) return Empty();
			return *this;
		}
		MaybeEmpty push( const T& value ) { _data.emplace_back(value); return *this; }
		MaybeEmpty push( const T&& value ) { _data.emplace_back(value); return *this; }
		std::vector<T> _data;
	};


	MaybeEmpty _stack;
};
