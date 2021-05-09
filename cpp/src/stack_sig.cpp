

StackSig StackSig::make_stacksig(const Type& type)  
{
	// NOTE - turns out make_optional will construct the optional with a default
	//		  ctor of the first listed type! Not what we expected/wanted!
	//return StackSig( std::make_pair(type, std::make_optional<AnyValue>()) );
	return StackSig( type, std::optional<AnyValue>() );
}

std::ostream& operator<<(std::ostream& out, const StackSig& sig) 
{ 
	out << "<Spec>{" << sig.type << ", " << sig.maybe_value << "}";
	return out; 
}


bool StackSig::operator==(const StackSig& s) const
{
	// Generic Types always match.
	if(type.id == 0) return true;

	// Different Types fail.
	if(type.id != s.type.id) return false;

	// If both our signatures specifies a value check it as well.
	if(maybe_value.has_value() and s.maybe_value.has_value() and maybe_value.value() != s.maybe_value.value()) return false;

	return true;
}

//bool StackSig::operator==(const StackObject& o) const
bool operator==(const StackSig& s, const StackObject& o)
{
	//std::cout << "Comparing " << *this << " with " << o << "." << std::endl;
	
	// Generic Types always match.
	if(s.type.id == 0) return true;

	// Different Types fail.
	if(s.type.id != o.type.id) return false;

	// If our signature specifies a value check it as well.
	if(s.maybe_value.has_value() and s.maybe_value.value() != o.value) return false;

	return true;
}

