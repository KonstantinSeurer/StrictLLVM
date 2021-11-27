
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

static Ref<UnitDeclaration> ParseErrorDeclaration(ErrorStream &err, Lexer &lexer)
{
	return nullptr;
}

static Ref<UnitDeclaration> ParseClassDeclaration(ErrorStream &err, Lexer &lexer, DeclarationFlags flags, bool singleton)
{
	return nullptr;
}

static Ref<UnitDeclaration> ParseTypeDeclaration(ErrorStream &err, Lexer &lexer)
{
	return nullptr;
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
		const Token &token = lexer.Next();

		if (token.type == TokenType::USING)
		{
			unit->dependencyNames.push_back(ParseUsing(err, lexer));
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
	}

	return unit;
}
