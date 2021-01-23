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
	auto rbegin(void) const 
	{ 
		auto result = std::get_if<NonEmpty>(&_stack);
		if (result) return result->_data.rbegin();
		throw Underflow();
	}

	auto rend(void) const 
	{ 
		auto result = std::get_if<NonEmpty>(&_stack);
		if (result) return result->_data.rend();
		throw Underflow();
	}

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

using AnyValue = std::variant< bool, int, unsigned, std::string >;

struct StackObject : std::pair< Type, AnyValue >
{

};

struct StackSig : public std::pair< Type,std::optional<AnyValue> >
{
	// Note - Generic types will always ignore a specified value.
	StackSig( std::pair< Type,std::optional<AnyValue> >& x ) : std::pair< Type,std::optional<AnyValue> >(x) {;}
	StackSig( std::pair< Type,std::optional<AnyValue> >&& x ) : std::pair< Type,std::optional<AnyValue> >(x) {;}

	static StackSig make_stacksig(const Type& type);

	bool operator==(const StackObject& o) const;
};

std::ostream& operator<<(std::ostream& out, const StackSig& sig);

template <class T> StackSig make_stacksig(const Type& type, T& val ) 
{
	return std::make_pair(type, std::make_optional< AnyValue >( std::make_any<T>(val) ));
}



struct Signature
{
	Stack<StackSig> in_seq;
	Stack<StackSig> out_seq;

	bool matches(const Stack<StackObject>& sobjects) const;
	bool matches(const Stack<StackSig>& sig) const;
};

std::ostream& operator<<(std::ostream& out, const Signature& sig);
