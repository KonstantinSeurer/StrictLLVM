
#include "ASTParser.h"
#include "Declaration.h"

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

static HashMap<TokenType, VisibilityFlags> visibilityFlags = {
	{TokenType::PRIVATE, VisibilityFlags::PRIVATE},
	{TokenType::PROTECTED, VisibilityFlags::PROTECTED},
	{TokenType::INTERNAL, VisibilityFlags::INTERNAL},
	{TokenType::PUBLIC, VisibilityFlags::PUBLIC}};

static HashMap<TokenType, DeclarationFlags> declarationFlags = {
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

static Ref<UnitDeclaration> ParseErrorDeclaration(ErrorStream &err, Lexer &lexer)
{
	Ref<ErrorDeclaration> result = Allocate<ErrorDeclaration>();

	ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER)
	lexer.Next(); // TODO: Assert that the identifier matches the unit name

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

static Ref<UnitDeclaration> ParseClassDeclaration(ErrorStream &err, Lexer &lexer, DeclarationFlags flags, bool singleton)
{
	Ref<ClassDeclaration> result = Allocate<ClassDeclaration>();

	return result;
}

static Ref<UnitDeclaration> ParseTypeDeclaration(ErrorStream &err, Lexer &lexer)
{
	Ref<TypeDeclaration> result = Allocate<TypeDeclaration>();

	return result;
}

static Ref<UnitDeclaration> ParseUnitDeclaration(ErrorStream &err, Lexer &lexer)
{
	VisibilityFlags visibility = visibilityFlags.at(lexer.Next().type);
	DeclarationFlags flags = DeclarationFlags::NONE;

	while (lexer.HasNext())
	{
		const Token &token = lexer.Next();

		if (declarationFlags.find(token.type) != declarationFlags.end())
		{
			flags = (DeclarationFlags)((UInt64)flags | (UInt64)declarationFlags.at(token.type));
			continue;
		}

		if (unitDeclarationTypeSet.find(token.type) == unitDeclarationTypeSet.end())
		{
			err.PrintError(token, "Expected declaration type!");
			return nullptr;
		}

		Ref<UnitDeclaration> declaration;

		switch (token.type)
		{
		case TokenType::ERROR:
			declaration = ParseErrorDeclaration(err, lexer);
			break;
		case TokenType::CLASS:
			declaration = ParseClassDeclaration(err, lexer, flags, false);
			break;
		case TokenType::SINGLETON:
			declaration = ParseClassDeclaration(err, lexer, flags, true);
			break;
		case TokenType::TYPE:
			declaration = ParseTypeDeclaration(err, lexer);
			break;
		default:
			return nullptr;
		}

		IFERR_RETURN(err)

		declaration->visibility = visibility;

		return declaration;
	}

	return nullptr;
}

Ref<Unit> ParseUnit(ErrorStream &err, Lexer lexer)
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

		if (visibilityFlags.find(token.type) == visibilityFlags.end())
		{
			err.PrintError(token, "Expected visibility flag!");
			return nullptr;
		}

		if (unit->declaredType)
		{
			err.PrintError(token, "Declaring multiple types in the same unit is not allowed!");
			return nullptr;
		}

		unit->declaredType = ParseUnitDeclaration(err, lexer);
		IFERR_RETURN(err)
	}

	return unit;
}
