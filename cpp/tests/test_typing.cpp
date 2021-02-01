#include <string>
#include <iostream>
#include <variant>

using namespace std;

using Object = std::variant< bool, int, string >;

template <class T> struct Type
{
	Type(const string& n) : name(n) {;}
	T& value( Object& o) { return std::get<T>(o); }

	const std::string name;
};

int main()
{
	Object ob = true;
	Type<bool> tb("Boolean");

	Object oi = 42;
	Type<int> ti("Integer");

	Object os = "A string.";
	Type<string> ts("String");

	cout << "Here's a " << tb.name << " : " << tb.value(ob) << endl;
	cout << "Here's a " << ti.name << " : " << ti.value(oi) << endl;
	cout << "Here's a " << ts.name << " : " << ts.value(os) << endl;
	cout << endl;

	return 0;
}
