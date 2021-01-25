#include <iostream>

using namespace std;

int main()
{
	char name[20], address[20];
	std::istream* stream;

	stream = &std::cin;
	
	cout << "Name: ";

	//cin.getline(name, 20);
	char c;
	stream->get(c);
	int pos = 0;
	while(not isspace(c))
	{ 
		name[pos++] = c;
		stream->get(c);
	}
	name[pos]=0;
	
	cout << "Address: ";
	stream->getline(address, 20);
	
	
	cout << endl << "You entered " << endl;
	cout << "Name = " << name << endl;
	cout << "Address = " << address << endl;
	
	return 0;
}
