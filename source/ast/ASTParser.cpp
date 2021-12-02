
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

#define ASSERT_TOKEN(err, lexer, tokenType, returnValue)                                                                                  \
	if (lexer.Get().type != tokenType)                                                                                                    \
	{                                                                                                                                     \
		err.PrintError(lexer.Get(), String("Unexpected token ") + ToString(lexer.Get().type) + " expected " + ToString(tokenType) + "!"); \
		return returnValue;                                                                                                               \
	}

#define IFERR_RETURN(err)      \
	if (err.HasErrorOccured()) \
	{                          \
		return nullptr;        \
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

static Ref<UnitDeclaration> ParseErrorDeclaration(ErrorStream &err, Lexer &lexer, const String &unitName)
{
	Ref<ErrorDeclaration> result = Allocate<ErrorDeclaration>();

	ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
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

		ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
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

static Ref<Template> ParseTemplate(ErrorStream &err, Lexer &lexer);

static Ref<DataType> ParseDataType(ErrorStream &err, Lexer &lexer)
{
	Ref<DataType> result;

	DeclarationFlags flags = ParseDeclarationFlags(lexer);

	if (lexer.Get().type == TokenType::ROUND_OB)
	{
		lexer.Next();

		result = ParseDataType(err, lexer);
		IFERR_RETURN(err)

		result->flags = (DeclarationFlags)((UInt64)result->flags | (UInt64)flags);
	}
	else
	{
		if (lexer.Get().type == TokenType::TYPE)
		{
			lexer.Next();

			result = Allocate<DataType>();
			result->dataTypeType = DataTypeType::TYPE;
		}
		else
		{
			Ref<ValueType> valueResult = Allocate<ValueType>();
			valueResult->flags = flags;

			ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
			valueResult->name = lexer.Next().data.stringData;

			if (lexer.Get().type == TokenType::LESS)
			{
				lexer.Next();

				valueResult->typeTemplate = ParseTemplate(err, lexer);
				IFERR_RETURN(err)
			}

			result = valueResult;
		}
	}

	while (lexer.HasNext())
	{
		if (lexer.Get().type == TokenType::ROUND_OB)
		{
			lexer.Next();
			break;
		}

		Ref<PointerType> pointerResult = Allocate<PointerType>();
		pointerResult->value = result;

		switch (lexer.Get().type)
		{
		case TokenType::STAR:
		{
			pointerResult->dataTypeType = DataTypeType::POINTER;
			break;
		}
		case TokenType::AND:
		{
			pointerResult->dataTypeType = DataTypeType::REFERENCE;
			break;
		}
		case TokenType::SQUARE_OB:
		{
			pointerResult->dataTypeType = DataTypeType::ARRAY;
			lexer.Next();

			// TODO: parse size expression

			ASSERT_TOKEN(err, lexer, TokenType::SQUARE_CB, nullptr)
			break;
		}
		default:
			return result;
		}

		lexer.Next();

		result = pointerResult;
	}

	return result;
}

static Ref<Template> ParseTemplate(ErrorStream &err, Lexer &lexer)
{
	Ref<Template> result = Allocate<Template>();

	while (lexer.HasNext())
	{
		Ref<VariableDeclaration> argument = Allocate<VariableDeclaration>(ASTItemType::VARIABLE_DECLARATION);

		argument->dataType = ParseDataType(err, lexer);
		IFERR_RETURN(err)

		ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
		argument->name = lexer.Next().data.stringData;

		result->arguments.push_back(argument);

		if (lexer.Get().type == TokenType::GREATER)
		{
			lexer.Next();
			break;
		}

		if (lexer.Get().type != TokenType::COMMA)
		{
			err.PrintError(lexer.Get(), "Template argument have to be seperated by a COMMA!");
			return nullptr;
		}
	}

	return result;
}

static Ref<VariableDeclaration> ParseMemberDeclaration(ErrorStream &err, Lexer &lexer, const String &unitName)
{
	Ref<VariableDeclaration> result = Allocate<VariableDeclaration>(ASTItemType::NONE);
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

static void ParseSuperTypeList(ErrorStream &err, Lexer &lexer, Array<Ref<ValueType>> &target, TokenType endCharacter)
{
	while (lexer.HasNext())
	{
		const Token &startToken = lexer.Get();

		Ref<DataType> superType = ParseDataType(err, lexer);

		if (superType->dataTypeType != DataTypeType::VALUE)
		{
			err.PrintError(startToken, "A super type must be a value type!");
			return;
		}

		target.push_back(std::dynamic_pointer_cast<ValueType>(superType));

		if (lexer.Get().type == endCharacter)
		{
			return;
		}

		ASSERT_TOKEN(err, lexer, TokenType::COMMA, )
		lexer.Next();
	}
}

static Ref<UnitDeclaration> ParseClassDeclaration(ErrorStream &err, Lexer &lexer, bool singleton, const String &unitName)
{
	Ref<ClassDeclaration> result = Allocate<ClassDeclaration>();
	result->isSingleton = singleton;

	ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
	if (lexer.Get().data.stringData != unitName)
	{
		err.PrintError(lexer.Get(), "The declared name must match the unit name!");
	}
	lexer.Next();

	if (lexer.Get().type == TokenType::LESS)
	{
		lexer.Next();

		result->typeTemplate = ParseTemplate(err, lexer);
		IFERR_RETURN(err)
	}

	if (lexer.Get().type == TokenType::COLON)
	{
		lexer.Next();

		ParseSuperTypeList(err, lexer, result->superTypes, TokenType::CURLY_OB);
		IFERR_RETURN(err)
	}

	ASSERT_TOKEN(err, lexer, TokenType::CURLY_OB, nullptr)
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
	IFERR_RETURN(err)

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