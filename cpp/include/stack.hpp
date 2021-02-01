//
//	stack.hpp	- Stack definition for ActorForth.
//

#pragma once

#include <stdexcept>
#include <variant>
#include <vector>
#include <optional>
#include <utility> // std::pair

#include "type.hpp"

struct Signature;

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

	T& tos(void) { return std::visit([&](auto& sarg) -> T& { return sarg.tos(); }, _stack); }
	const T& tos(void) const { return std::visit([&](const auto & sarg) -> const T& { return sarg.tos(); }, _stack); }	

	void pop(void) 
	{ 
		_stack = std::visit([](auto& sarg) { return sarg.pop(); }, _stack); 
		/*
		MaybeEmpty m = std::visit([](auto& sarg) { return sarg.pop(); }, _stack);
		if(_stack.index() != m.index()) _stack = m;
		*/
	}

	void push( const T& value ) 
	{ 
		_stack = std::visit([&](auto& sarg) { return sarg.push(value); }, _stack); 
		/*
		MaybeEmpty m = std::visit([&value](auto& sarg) { return sarg.push(value); }, _stack);
		if(_stack.index() != m.index())
		{
			std::cout << "Updating Stack State!" << std::endl;
			_stack = m;
		}
		std::cout << "tos() = " << tos() << std::endl;
		*/
	}

	size_t depth(void) const { return (std::get_if<NonEmpty>(&_stack)) ? std::get<NonEmpty>(_stack)._data.size() : 0; }

	auto rbegin(void) const 
	{ 
		auto result = std::get_if<NonEmpty>(&_stack);
		return (result) ? result->_data.rbegin() : AlwaysEmpty.rbegin();
	}

	auto rend(void) const 
	{ 
		auto result = std::get_if<NonEmpty>(&_stack);
		return (result) ? result->_data.rend() : AlwaysEmpty.rend();
	}

private:

	struct Empty
	{
		T& tos(void) { throw Underflow(); }	
		const T& tos(void) const { throw Underflow(); }	
		MaybeEmpty pop(void) { throw Underflow(); }
		MaybeEmpty push( const T& value ) 
		{ 
			//std::cout << "Pushing " << value << " onto empty stack." << std::endl;
			return NonEmpty(value); 
		}
		//MaybeEmpty push( T&& value ) { return NonEmpty(value); }
	};

	struct NonEmpty
	{
		NonEmpty( const T& value ) : _data(1,value) 
		{
			/*
			_data.push_back(value);
			std::cout << "NonEmpty ctor receiving " << value << " now has " << _data.back() << std::endl;
			*/
		}
		//NonEmpty( const T&& value ) { _data.emplace_back(value); }
		T& tos(void) { return _data.back(); }	
		const T& tos(void) const { return _data.back(); }
		
		MaybeEmpty pop(void) 
		{ 
			_data.pop_back(); 
			if(_data.empty()) return Empty();
			return *this;
		}
		MaybeEmpty push( const T& value ) 
		{ 
			_data.push_back(value); 
			// std::cout << "NonEmpty push receiving " << value << std::endl;
			return *this;
		}
		//MaybeEmpty push( T&& value ) { _data.emplace_back(value); return *this; }
		std::vector<T> _data;
	};


	MaybeEmpty _stack;

	// This is used to guarantee that even empty Stacks don't throw
	// exceptions when rbegin and rend are called. They'll just be
	// const iterators to an empty vector.
	static const std::vector<T> AlwaysEmpty;

	friend inline std::ostream& operator<<(std::ostream& out, const Stack<T>& stack)
	{
		out << "Stack: [";

		auto v = std::get_if<Stack<T>::NonEmpty>(&(stack._stack));
		if(not v)
		{
			out << "<empty>";
		}
		else
		{
			std::for_each(v->_data.begin(), v->_data.end(), [&out](const T& t) { out << t << ","; } );	
		}
		out << "]";

		return out;
	}

	friend std::ostream& operator<<(std::ostream& out, const Signature& sig);
};

//template<class T> inline std::ostream& operator<<(std::ostream& out, const Stack<T>& stack)


using AnyValue = std::variant< bool, int, unsigned, std::string >;

std::ostream& operator<<(std::ostream& out, const AnyValue& val);
std::ostream& operator<<(std::ostream& out, const std::optional<AnyValue>& val);

struct StackObject
{
	// BDM StackObject( std::pair< Type, AnyValue >&& x ) : std::pair< Type, AnyValue >(x) {;}
	StackObject( const Type& t, const AnyValue& x ) : first(t), second(x) {;}

	template<class T> static StackObject make_stackobj(const Type& type, const T& val )
	{
		return StackObject( type, val );		
	}

	Type first;
	AnyValue second;
};

std::ostream& operator<<(std::ostream& out, const StackObject& obj);

struct StackSig
{
	// Note - Generic types will always ignore a specified value.
	StackSig( const Type& t, const std::optional<AnyValue>& x ) : first(t), second(x) {;}
	StackSig( const StackSig& s ) = default;

	/*
	StackSig& operator=(const StackSig& s) 
	{
		first = s.first;
		second = s.second;
		return *this;
	}
	*/
	// BDM StackSig( std::pair< Type,std::optional<AnyValue> >&& x ) : std::pair< Type,std::optional<AnyValue> >(x) {;}

	static StackSig make_stacksig(const Type& type);
	template<class T> static StackSig make_stacksig(const Type& type, const T& val ) 
	{
		return StackSig(type, std::make_optional< AnyValue >( val ));
	}

	bool operator==(const StackSig& o) const;
	bool operator==(const StackObject& o) const;

	Type first;
	std::optional<AnyValue> second;
};

std::ostream& operator<<(std::ostream& out, const StackSig& sig);

struct Signature
{
	Stack<StackSig> in_seq;
	Stack<StackSig> out_seq;

	bool matches(const Stack<StackObject>& sobjects) const;
	bool matches(const Stack<StackSig>& sig) const;
};

std::ostream& operator<<(std::ostream& out, const Signature& sig);
