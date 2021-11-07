
#include <iostream>
#include <filesystem>
#include <fstream>

#include "Base.h"
#include "Lexer.h"
#include "BuildContext.h"

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
		Token token = tokens->Next();

		std::cout << "\t\t" << ToString(token.type);

		switch (token.type)
		{
		case TokenType::STRING_LITERAL:
			std::cout << " \"" << token.data.stringData << "\"";
			break;
		case TokenType::INT_LITERAL:
			std::cout << " " << token.data.intData;
			break;
		case TokenType::UINT_LITERAL:
			std::cout << " " << token.data.uintData;
			break;
		case TokenType::FLOAT_LITERAL:
			std::cout << " " << token.data.floatData;
			break;
		case TokenType::IDENTIFIER:
			std::cout << " " << token.data.stringData;
			break;
		}

		std::cout << std::endl;
	} while (tokens->HasNext());
}

int main(int argc, const char **args)
{
	if (argc < 2)
	{
		return 0;
	}

	std::cout << "Building module '" << args[1] << "'..." << std::endl;

	const Array<String> modulePath = {std::filesystem::current_path()};
	const String cachePath = String(std::filesystem::current_path()) + "/cache";

	const TargetFlags target = TargetFlags::BIT64 | TargetFlags::LINUX | TargetFlags::X86;

	BuildContext context(modulePath, cachePath, target);
	context.AddModule(args[1]);
	context.Build();

	return 0;
}
