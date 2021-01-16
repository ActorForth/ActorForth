//
//	operation.cpp	-	Definition for Operation in ActorForth.
//

#include <operation.hpp>
#include <continuation.hpp>

// Holds the global vocabularies of all Operations for each Type.
std::map<Type::ID,std::vector<Operation>> Operation::TypeOps;

// Holds all of the Constructors for various Types.
std::map<Type::ID,std::vector<Operation>> Operation::TypeCtors;

