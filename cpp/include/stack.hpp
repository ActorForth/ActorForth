//
//	stack.hpp	- Stack definition for ActorForth.
//

#pragma once

#include <stdexcept>
#include <variant>
#include <vector>
#include <optional>
#include <utility> // std::pair
#include <any>


template <class T> class Stack
{
	struct Empty;
	struct NonEmpty;
	using MaybeEmpty = std::variant<Empty, NonEmpty>;
public:

	struct Underflow : public std::out_of_range 
	{
		Underflow() : std::out_of_range("<exception: Stack Underflow> Empty stack!") {;}
	};

	Stack(void) : _stack(Empty()) {;} 
	Stack(const Stack&) = default;

	T& tos(void) { return std::visit([&](auto& sarg) { return sarg.tos(); }, _stack); }
	const T& tos(void) const { return std::visit([&](auto& sarg) { return sarg.tos(); }, _stack); }

	Stack<T>::MaybeEmpty push( const T& value ) { return std::visit([&](auto& sarg) { return sarg.push(value); }, _stack); }

	size_t depth(void) const { return (std::get_if<NonEmpty>(&_stack)) ? std::get<NonEmpty>(_stack)._data.size() : 0; }

private:

	struct Empty
	{
		T& tos(void) { throw Underflow(); }	
		const T& tos(void) const { throw Underflow(); }	
		MaybeEmpty pop(void) { throw Underflow(); }
		MaybeEmpty push( const T& value ) { return NonEmpty(value); }
		MaybeEmpty push( const T&& value ) { return NonEmpty(value); }
	};

	struct NonEmpty
	{
		NonEmpty( const T& value ) : _data(1,value) {;}
		// NonEmpty( const T&& value ) { _data.emplace_back(value); }
		T& tos(void) { return _data.back(); }	
		const T& tos(void) const { return _data.back(); }
		
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


using StackSig = std::pair< Type,std::optional<std::any> >;
using StackObject = std::pair< Type,std::any >;

struct Signature
{
	Stack<StackSig> in_seq;
	Stack<StackSig> out_seq;

	bool matches(const Stack<StackObject>& sobject) const;
	bool matches(const Stack<StackSig>& sig) const;
};

//std::ostream& operator<<(std::ostream& out, const Signature& sig);
