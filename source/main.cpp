
#include <iostream>
#include <filesystem>
#include <fstream>

#include "Base.h"
#include "Lexer.h"

static void CompileSourceFile(const String &filename)
{
	if (std::filesystem::is_directory(filename))
	{
		for (const auto &file : std::filesystem::directory_iterator(filename))
		{
			CompileSourceFile(file.path());
		}

		return;
	}

	if (filename.length() < 7 && filename.substr(filename.length() - 7, 7) != ".strict")
	{
		return;
	}

	std::cout << "\tBuilding source file '" << filename << "'..." << std::endl;

	std::ifstream fileStream(filename);
	String fileContent = String(std::istreambuf_iterator<char>(fileStream), std::istreambuf_iterator<char>());
	Ref<TokenStream> tokens = TokenStream::Create(fileContent);

	do
	{
		std::cout << "\t\t" << ToString(tokens->Next().type) << std::endl;
	} while (tokens->HasNext());
}

int main(int argc, const char **args)
{
	if (argc < 2)
	{
		return 0;
	}

	std::cout << "Building module '" << args[1] << "'..." << std::endl;

	CompileSourceFile(String(args[1]));

	return 0;
}
