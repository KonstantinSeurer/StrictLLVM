
#include "ASTParser.h"

#include <iostream>

Ref<Unit> ParseUnit(TokenStream lexer)
{
	Ref<Unit> unit = Allocate<Unit>();

	while (lexer.HasNext())
	{
		const Token &token = lexer.Next();

		if (token.type != TokenType::USING)
		{
			continue;
		}

		String dependency;
		while (lexer.HasNext())
		{
			const Token &dependencyToken = lexer.Next();

			if (dependencyToken.type == TokenType::EQUALS || dependencyToken.type == TokenType::SEMICOLON)
			{
				break;
			}

			if (dependencyToken.type == TokenType::IDENTIFIER)
			{
				dependency += dependencyToken.data.stringData;
			}
			else if (dependencyToken.type == TokenType::PERIOD)
			{
				dependency += '.';
			}
			else
			{
				std::cerr << "Unexpected identifier " << ToString(dependencyToken.type) << "!" << std::endl;
				return nullptr;
			}
		}

		unit->dependencyNames.push_back(dependency);
	}

	return unit;
}
