
#include "ASTParser.h"

#include <iostream>

static String ParseUsing(ErrorStream &err, Lexer &lexer)
{
	String dependency;

	while (lexer.HasNext())
	{
		const Token &token = lexer.Next();

		if (token.type == TokenType::EQUALS || token.type == TokenType::SEMICOLON)
		{
			break;
		}

		if (token.type == TokenType::IDENTIFIER)
		{
			dependency += token.data.stringData;
		}
		else if (token.type == TokenType::PERIOD)
		{
			dependency += '.';
		}
		else
		{
			err.PrintError(token, String("Unexpected token ") + ToString(token.type) + "!");
			return nullptr;
		}
	}

	return dependency;
}

static HashMap<TokenType, DeclarationFlags> declarationFlags = {
	{TokenType::PRIVATE, DeclarationFlags::PRIVATE},
	{TokenType::PROTECTED, DeclarationFlags::PROTECTED},
	{TokenType::INTERNAL, DeclarationFlags::INTERNAL},
	{TokenType::PUBLIC, DeclarationFlags::PUBLIC},
	{TokenType::MUT, DeclarationFlags::MUT},
	{TokenType::IMPURE, DeclarationFlags::IMPURE}};

static HashSet<TokenType> unitDeclarationTypeSet = {TokenType::ERROR, TokenType::CLASS, TokenType::SINGLETON, TokenType::TYPE};

#define ASSERT_TOKEN(err, lexer, tokenType)                                                   \
	if (lexer.Get().type != tokenType)                                                        \
	{                                                                                         \
		err.PrintError(lexer.Get(), String("Unexpected token ") + ToString(tokenType) + "!"); \
		return nullptr;                                                                       \
	}

#define IFERR_RETURN(err)      \
	if (err.HasErrorOccured()) \
	{                          \
		return nullptr;        \
	}

static Ref<UnitDeclaration> ParseErrorDeclaration(ErrorStream &err, Lexer &lexer, const String &unitName)
{
	Ref<ErrorDeclaration> result = Allocate<ErrorDeclaration>();

	ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER)
	if (lexer.Get().data.stringData != unitName)
	{
		err.PrintError(lexer.Get(), "The declared name must match the unit name!");
		return nullptr;
	}
	lexer.Next();

	if (lexer.Get().type == TokenType::EQUALS)
	{
		lexer.Next();

		result->hasValue = true;

		const Token &valueToken = lexer.Next();
		if (valueToken.type == TokenType::INT_LITERAL)
		{
			result->value = valueToken.data.intData;
		}
		else if (valueToken.type == TokenType::UINT_LITERAL)
		{
			result->value = valueToken.data.uintData;
		}
		else
		{
			err.PrintError(lexer.Get(), String("Unexpected token ") + ToString(valueToken.type) + "!");
			return nullptr;
		}

		ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON)
		lexer.Next();
	}
	else if (lexer.Get().type == TokenType::SEMICOLON)
	{
		result->hasValue = false;
		lexer.Next();
	}
	else
	{
		err.PrintError(lexer.Get(), String("Unexpected token ") + ToString(lexer.Get().type) + "!");
		return nullptr;
	}

	return result;
}

static Ref<DataType> ParseDataType(ErrorStream &err, Lexer &lexer)
{
	Ref<DataType> result = Allocate<DataType>();

	return result;
}

static Ref<Template> ParseTemplate(ErrorStream &err, Lexer &lexer)
{
	Ref<Template> result = Allocate<Template>();

	// TODO: implement, this implementation only skips the template
	while (lexer.HasNext())
	{
		const Token &token = lexer.Next();

		if (token.type == TokenType::GREATER)
		{
			break;
		}
	}

	return result;
}

static DeclarationFlags ParseDeclarationFlags(Lexer &lexer)
{
	DeclarationFlags flags = DeclarationFlags::PRIVATE;

	while (lexer.HasNext())
	{
		const Token &token = lexer.Get();

		if (declarationFlags.find(token.type) != declarationFlags.end())
		{
			flags = (DeclarationFlags)((UInt64)flags | (UInt64)declarationFlags.at(token.type));
			lexer.Next();
		}
		else
		{
			break;
		}
	}

	return flags;
}

static Ref<MemberDeclaration> ParseMemberDeclaration(ErrorStream &err, Lexer &lexer, const String &unitName)
{
	Ref<MemberDeclaration> result = Allocate<MemberDeclaration>(ASTItemType::MEMBER_VARIABLE_DECLARATION);
	result->flags = ParseDeclarationFlags(lexer);

	bool isConstructor = false;

	if (lexer.Get().type == TokenType::IDENTIFIER && lexer.Get().data.stringData == unitName)
	{
		lexer.Next();

		isConstructor = true;
	}
	else
	{
		result->dataType = ParseDataType(err, lexer);
		IFERR_RETURN(err)
	}

	return result;
}

static Ref<UnitDeclaration> ParseClassDeclaration(ErrorStream &err, Lexer &lexer, bool singleton, const String &unitName)
{
	Ref<ClassDeclaration> result = Allocate<ClassDeclaration>();
	result->isSingleton = singleton;

	ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER)
	if (lexer.Get().data.stringData != unitName)
	{
		err.PrintError(lexer.Get(), "The declared name must match the unit name!");
		lexer.Next();
	}

	if (lexer.Get().type == TokenType::LESS)
	{
		result->typeTemplate = ParseTemplate(err, lexer);
		IFERR_RETURN(err)
	}

	if (lexer.Get().type == TokenType::COLON)
	{
		lexer.Next();
		// TODO: implement inheritance
	}

	ASSERT_TOKEN(err, lexer, TokenType::CURLY_OB)
	lexer.Next();

	while (lexer.HasNext())
	{
		const Token &token = lexer.Next();

		if (token.type == TokenType::CURLY_CB)
		{
			break;
		}

		result->members.push_back(ParseMemberDeclaration(err, lexer, unitName));
	}

	return result;
}

static Ref<UnitDeclaration> ParseTypeDeclaration(ErrorStream &err, Lexer &lexer, const String &unitName)
{
	Ref<TypeDeclaration> result = Allocate<TypeDeclaration>();

	return result;
}

static Ref<UnitDeclaration> ParseUnitDeclaration(ErrorStream &err, Lexer &lexer, const String &unitName)
{
	DeclarationFlags flags = ParseDeclarationFlags(lexer);

	const Token &token = lexer.Next();

	if (unitDeclarationTypeSet.find(token.type) == unitDeclarationTypeSet.end())
	{
		err.PrintError(token, "Expected declaration type!");
		return nullptr;
	}

	Ref<UnitDeclaration> declaration;

	switch (token.type)
	{
	case TokenType::ERROR:
		declaration = ParseErrorDeclaration(err, lexer, unitName);
		break;
	case TokenType::CLASS:
		declaration = ParseClassDeclaration(err, lexer, false, unitName);
		break;
	case TokenType::SINGLETON:
		declaration = ParseClassDeclaration(err, lexer, true, unitName);
		break;
	case TokenType::TYPE:
		declaration = ParseTypeDeclaration(err, lexer, unitName);
		break;
	default:
		return nullptr;
	}

	declaration->flags = flags;

	IFERR_RETURN(err)

	return declaration;
}

Ref<Unit> ParseUnit(ErrorStream &err, Lexer lexer, const String &name)
{
	Ref<Unit> unit = Allocate<Unit>();

	while (lexer.HasNext())
	{
		const Token &token = lexer.Get();

		if (token.type == TokenType::USING)
		{
			lexer.Next();
			unit->dependencyNames.push_back(ParseUsing(err, lexer));
			IFERR_RETURN(err)
			continue;
		}

		if (declarationFlags.find(token.type) == declarationFlags.end())
		{
			err.PrintError(token, "Expected visibility flag!");
			return nullptr;
		}

		if (unit->declaredType)
		{
			err.PrintError(token, "Declaring multiple types in the same unit is not allowed!");
			return nullptr;
		}

		unit->declaredType = ParseUnitDeclaration(err, lexer, name);
		IFERR_RETURN(err)
	}

	return unit;
}
