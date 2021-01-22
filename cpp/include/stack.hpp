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

#include "type.hpp"

struct Signature;

template <class T> class Stack
{
//public:
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

	T& tos(void) { return std::visit([&](auto& sarg) -> T& { return sarg.tos(); }, _stack); }
	const T& tos(void) const { return std::visit([&](const auto & sarg) -> const T& { return sarg.tos(); }, _stack); }	

	void pop(void) { _stack = std::visit([](auto& sarg) { return sarg.pop(); }, _stack); }

	void push( const T& value ) { _stack = std::visit([&](auto& sarg) { return sarg.push(value); }, _stack); }

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
		NonEmpty( const T&& value ) { _data.emplace_back(value); }
		T& tos(void) { return _data.back(); }	
		const T& tos(void) const { return _data.back(); }
		
		MaybeEmpty pop(void) 
		{ 
			_data.pop_back(); 
			if(_data.empty()) return Empty();
			return *this;
		}
		MaybeEmpty push( const T& value ) { _data.push_back(value); return *this; }
		MaybeEmpty push( const T&& value ) { _data.emplace_back(value); return *this; }
		std::vector<T> _data;
	};


	MaybeEmpty _stack;

	friend std::ostream& operator<<(std::ostream& out, const Signature& sig);
};

using StackSig = std::pair< Type,std::optional<std::any> >;
std::ostream& operator<<(std::ostream& out, const StackSig& sig);

StackSig make_stacksig(const Type& type);
template <class T> StackSig make_stacksig(const Type& type, T& val ) 
{
	return std::make_pair(type, std::make_optional< std::any >( std::make_any<T>(val) ));
}
using StackObject = std::pair< Type,std::any >;

struct Signature
{
	Stack<StackSig> in_seq;
	Stack<StackSig> out_seq;

	bool matches(const Stack<StackObject>& sobject) const;
	bool matches(const Stack<StackSig>& sig) const;
};

std::ostream& operator<<(std::ostream& out, const Signature& sig);
