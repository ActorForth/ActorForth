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

	struct Empty;
	struct NonEmpty;
	using MaybeEmpty = std::variant<Empty, NonEmpty>;

	//using StackResult = std::pair<MaybeEmpty, T&>;

	struct Empty
	{
		T& tos(void) const { throw Underflow(); }	
		MaybeEmpty pop(void) { throw Underflow(); }
		MaybeEmpty push( const T& value )
		{ 
			auto r = NonEmpty(value); 
			return { r, r.tos() };
		}
		MaybeEmpty push( const T&& value )
		{ 
			auto r = NonEmpty(value); 
			return { r, r.tos() };
		}
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

	

	Stack(void) : _stack(Empty()) {;} // { _stack = Empty();}
	Stack(const Stack&) = default;


//private:
	MaybeEmpty _stack;
};