
#include <iostream>

int main(int argc, const char **args)
{
	if (argc < 2)
	{
		return 0;
	}

	std::cout << "Building module '" << args[1] << "'..." << std::endl;

	return 0;
}
