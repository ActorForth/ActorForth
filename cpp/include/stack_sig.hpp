//
//	stack_sig.hpp	- StackSig declaration for ActorForth.
//

#pragma once

#include "type.hpp"

namespace ActorForth
{

struct StackSig
{
	// Note - Generic types will always ignore a specified value.
	StackSig( const Types::Type& t, const std::optional<Types::AnyValue>& x ) : type(t), maybe_value(x) {;}
	StackSig( const StackSig& s ) = default;

	// BDM StackSig( std::pair< Type,std::optional<AnyValue> >&& x ) : std::pair< Type,std::optional<AnyValue> >(x) {;}

	static StackSig make_stacksig(const Types::Type& type);
	template<class T> static StackSig make_stacksig(const Types::Type& type, const T& val ) 
	{
		return StackSig(type, std::make_optional< Types::AnyValue >( val ));
	}

	bool operator==(const StackSig& o) const;
	//bool operator==(const StackObject& o) const;
	friend bool operator==(const StackSig& s, const StackObject& o);

	Types::Type type;
	std::optional<Types::AnyValue> maybe_value;
};


std::ostream& operator<<(std::ostream& out, const StackSig& sig);


} // eons ActorForth