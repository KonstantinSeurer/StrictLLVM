
#include "Unit.h"

#include <iostream>

Unit::Unit(const JSON &structureJSON)
	: ASTItem(ASTItemType::UNIT)
{
	name = structureJSON["name"];

	for (const JSON &dependencyName : structureJSON["dependencies"])
	{
		dependencyNames.push_back(String(dependencyName));
	}
}
/*
Bool Unit::ParseStructure()
{
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
				return false;
			}
		}

		dependencyNames.push_back(dependency);
	}

	return true;
}

Bool Unit::ParseImplementation()
{
	return true;
}

Bool Unit::Link()
{
	return true;
}
*/

JSON Unit::GetStructureJSON() const
{
	JSON result;
	JSON dependenciesJSON(JSON::value_t::array);

	for (UInt64 dependencyIndex = 0; dependencyIndex < dependencyNames.size(); dependencyIndex++)
	{
		dependenciesJSON[dependencyIndex] = dependencyNames[dependencyIndex];
	}

	result["dependencies"] = dependenciesJSON;
	result["name"] = name;

	return result;
}
