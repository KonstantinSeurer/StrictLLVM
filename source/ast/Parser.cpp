
#include "Parser.h"

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

#define ASSERT_TOKEN(err, lexer, tokenType, returnValue)                                                                                  \
	if (lexer.Get().type != tokenType)                                                                                                    \
	{                                                                                                                                     \
		err.PrintError(lexer.Get(), String("Unexpected token ") + ToString(lexer.Get().type) + " expected " + ToString(tokenType) + "!"); \
		return returnValue;                                                                                                               \
	}

#define IFERR_RETURN(err, returnValue) \
	if (err.HasErrorOccured())         \
	{                                  \
		return returnValue;            \
	}

static HashMap<TokenType, DeclarationFlags> declarationFlags = {
	{TokenType::PRIVATE, DeclarationFlags::PRIVATE},
	{TokenType::PROTECTED, DeclarationFlags::PROTECTED},
	{TokenType::INTERNAL, DeclarationFlags::INTERNAL},
	{TokenType::PUBLIC, DeclarationFlags::PUBLIC},
	{TokenType::MUT, DeclarationFlags::MUT},
	{TokenType::IMPURE, DeclarationFlags::IMPURE},
	{TokenType::VIRTUAL, DeclarationFlags::VIRTUAL}};

static DeclarationFlags ParseDeclarationFlags(Lexer &lexer)
{
	DeclarationFlags flags = DeclarationFlags::PRIVATE;

	while (lexer.HasNext())
	{
		const Token &token = lexer.Get();

		if (declarationFlags.find(token.type) != declarationFlags.end())
		{
			flags = flags | declarationFlags.at(token.type);
			lexer.Next();
		}
		else
		{
			break;
		}
	}

	return flags;
}

static HashMap<TokenType, DeclarationFlags> visibilityFlags = {
	{TokenType::PRIVATE, DeclarationFlags::PRIVATE},
	{TokenType::PROTECTED, DeclarationFlags::PROTECTED},
	{TokenType::INTERNAL, DeclarationFlags::INTERNAL},
	{TokenType::PUBLIC, DeclarationFlags::PUBLIC}};

static DeclarationFlags ParseVisibilityFlags(Lexer &lexer)
{
	DeclarationFlags flags = DeclarationFlags::PRIVATE;

	while (lexer.HasNext())
	{
		const Token &token = lexer.Get();

		if (visibilityFlags.find(token.type) != visibilityFlags.end())
		{
			flags = flags | visibilityFlags.at(token.type);
			lexer.Next();
		}
		else
		{
			break;
		}
	}

	return flags;
}

static Lexer ScanAndSkipNested(ErrorStream &err, Lexer &lexer, TokenType openToken, TokenType closeToken)
{
	const Int64 startPosition = lexer.GetPosition();

	UInt32 level = 0;
	while (lexer.HasNext())
	{
		const TokenType token = lexer.Next().type;

		if (token == openToken)
		{
			level++;
		}

		if (token == closeToken)
		{
			if (level == 0)
			{
				err.PrintError(String("Unexpected token ") + ToString(closeToken) + "!");
				return Lexer();
			}

			level--;

			if (level == 0)
			{
				return Lexer(lexer, startPosition, lexer.GetPosition() - startPosition);
			}
		}
	}

	err.PrintError(String("Expected token ") + ToString(closeToken) + " before end of file!");
	return Lexer();
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

static HashMap<TokenType, Ref<PrimitiveType>> primitiveTypes;
static_block
{
	primitiveTypes[TokenType::VOID] = Allocate<PrimitiveType>(TokenType::VOID);
	primitiveTypes[TokenType::BOOL] = Allocate<PrimitiveType>(TokenType::BOOL);
	primitiveTypes[TokenType::INT8] = Allocate<PrimitiveType>(TokenType::INT8);
	primitiveTypes[TokenType::UINT8] = Allocate<PrimitiveType>(TokenType::UINT8);
	primitiveTypes[TokenType::INT16] = Allocate<PrimitiveType>(TokenType::INT16);
	primitiveTypes[TokenType::UINT16] = Allocate<PrimitiveType>(TokenType::UINT16);
	primitiveTypes[TokenType::INT32] = Allocate<PrimitiveType>(TokenType::INT32);
	primitiveTypes[TokenType::UINT32] = Allocate<PrimitiveType>(TokenType::UINT32);
	primitiveTypes[TokenType::INT64] = Allocate<PrimitiveType>(TokenType::INT64);
	primitiveTypes[TokenType::UINT64] = Allocate<PrimitiveType>(TokenType::UINT64);
	primitiveTypes[TokenType::FLOAT32] = Allocate<PrimitiveType>(TokenType::FLOAT32);
	primitiveTypes[TokenType::FLOAT64] = Allocate<PrimitiveType>(TokenType::FLOAT64);
};

static Ref<DataType> ParseDataType(ErrorStream &err, Lexer &lexer);

static Ref<Template> ParseTemplate(ErrorStream &err, Lexer &lexer)
{
	Ref<Template> result = Allocate<Template>();

	while (lexer.HasNext())
	{
		if (lexer.Get().type == TokenType::GREATER)
		{
			lexer.Next();
			return result;
		}

		lexer.Push();
		err.Try();

		Ref<ASTItem> argument = ParseDataType(err, lexer);

		if (err.Catch())
		{
			lexer.Revert();
			lexer.Push();
			err.Try();

			// TODO: parse expression

			if (err.Catch())
			{
				lexer.Revert();
				err.PrintError("Expected data type or expression!");
				return nullptr;
			}
		}
		else
		{
			lexer.Pop();
		}

		result->arguments.push_back(argument);

		if (lexer.Get().type == TokenType::GREATER)
		{
			lexer.Next();
			return result;
		}

		ASSERT_TOKEN(err, lexer, TokenType::COMMA, nullptr)
		lexer.Next();
	}

	err.PrintError("Expected token GREATER before end of file!");
	return nullptr;
}

static Ref<DataType> ParseDataType(ErrorStream &err, Lexer &lexer)
{
	Ref<DataType> result;

	DeclarationFlags flags = ParseDeclarationFlags(lexer);

	if (lexer.Get().type == TokenType::ROUND_OB)
	{
		lexer.Next();

		result = ParseDataType(err, lexer);
		IFERR_RETURN(err, nullptr)

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
		else if (lexer.Get().type == TokenType::IDENTIFIER)
		{
			Ref<ObjectType> objectResult = Allocate<ObjectType>();
			objectResult->flags = flags;
			objectResult->name = lexer.Next().data.stringData;

			if (lexer.Get().type == TokenType::LESS)
			{
				lexer.Next();

				objectResult->typeTemplate = ParseTemplate(err, lexer);
				IFERR_RETURN(err, nullptr)
			}

			result = objectResult;
		}
		else
		{
			const TokenType primitiveType = lexer.Get().type;

			if (primitiveTypes.find(primitiveType) == primitiveTypes.end())
			{
				err.PrintError(lexer.Get(), String("Unexpected token ") + ToString(primitiveType) + " expected primitive type!");
				return nullptr;
			}

			result = primitiveTypes.at(primitiveType);
			lexer.Next();
		}
	}

	while (lexer.HasNext())
	{
		if (lexer.Get().type == TokenType::ROUND_CB)
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

static void ParseParameterList(ErrorStream &err, Lexer &lexer, Array<Ref<VariableDeclaration>> &target, TokenType endToken)
{
	while (lexer.HasNext())
	{
		if (lexer.Get().type == endToken)
		{
			lexer.Next();
			return;
		}

		Ref<VariableDeclaration> argument = Allocate<VariableDeclaration>();

		argument->dataType = ParseDataType(err, lexer);
		IFERR_RETURN(err, )

		ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, )
		argument->name = lexer.Next().data.stringData;

		target.push_back(argument);

		if (lexer.Get().type == endToken)
		{
			lexer.Next();
			return;
		}

		ASSERT_TOKEN(err, lexer, TokenType::COMMA, )
		lexer.Next();
	}
}

static Ref<TemplateDeclaration> ParseTemplateDeclaration(ErrorStream &err, Lexer &lexer)
{
	Ref<TemplateDeclaration> result = Allocate<TemplateDeclaration>();

	ParseParameterList(err, lexer, result->parameters, TokenType::GREATER);
	IFERR_RETURN(err, nullptr)

	return result;
}

static Ref<MethodDeclaration> ParseAccessor(ErrorStream &err, Lexer &lexer)
{
	Ref<MethodDeclaration> result = Allocate<MethodDeclaration>();
	result->flags = ParseDeclarationFlags(lexer);

	result->dataType = ParseDataType(err, lexer);
	IFERR_RETURN(err, nullptr)

	if (lexer.Get().type == TokenType::GET)
	{
		lexer.Next();

		result->methodType = MethodType::GETTER;
	}
	else if (lexer.Get().type == TokenType::SET)
	{
		lexer.Next();

		result->methodType = MethodType::SETTER;
	}
	else
	{
		err.PrintError(lexer.Get(), "Unexpected token " + ToString(lexer.Get().type) + "! Expected either get or set.");
		return nullptr;
	}

	ASSERT_TOKEN(err, lexer, TokenType::ROUND_OB, nullptr)
	lexer.Next();

	ParseParameterList(err, lexer, result->parameters, TokenType::ROUND_CB);
	IFERR_RETURN(err, nullptr)

	if (lexer.Get().type == TokenType::CURLY_OB)
	{
		result->tempBody = ScanAndSkipNested(err, lexer, TokenType::CURLY_OB, TokenType::CURLY_CB);
		IFERR_RETURN(err, nullptr)
	}
	else
	{
		ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
		lexer.Next();
	}

	return result;
}

static Ref<Expression> ParseExpression(ErrorStream &err, Lexer &lexer)
{
	switch (lexer.Get().type)
	{
	default:
		break;
	}

	return nullptr;
}

static Ref<Statement> ParseStatement(ErrorStream &err, Lexer &lexer);

static Ref<BlockStatement> ParseBlockStatement(ErrorStream &err, Lexer &lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::CURLY_OB, nullptr)
	lexer.Next();

	Ref<BlockStatement> result = Allocate<BlockStatement>();

	while (lexer.HasNext())
	{
		if (lexer.Get().type == TokenType::CURLY_CB)
		{
			lexer.Next();
			break;
		}

		result->statements.push_back(ParseStatement(err, lexer));
		IFERR_RETURN(err, nullptr)
	}

	return result;
}

static Ref<IfStatement> ParseIfStatement(ErrorStream &err, Lexer &lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::IF, nullptr)
	lexer.Next();

	Ref<IfStatement> result = Allocate<IfStatement>();

	result->condition = ParseExpression(err, lexer);
	IFERR_RETURN(err, nullptr)

	result->thenStatement = ParseStatement(err, lexer);
	IFERR_RETURN(err, nullptr)

	if (lexer.Get().type == TokenType::ELSE)
	{
		result->elseStatement = ParseStatement(err, lexer);
		IFERR_RETURN(err, nullptr)
	}

	return result;
}

static Ref<ForStatement> ParseForStatement(ErrorStream &err, Lexer &lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::FOR, nullptr)
	lexer.Next();

	Ref<ForStatement> result = Allocate<ForStatement>();

	ASSERT_TOKEN(err, lexer, TokenType::ROUND_OB, nullptr)
	lexer.Next();

	result->startStatement = ParseStatement(err, lexer);
	IFERR_RETURN(err, nullptr)

	result->condition = ParseExpression(err, lexer);
	IFERR_RETURN(err, nullptr)

	ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
	lexer.Next();

	result->incrementExpression = ParseExpression(err, lexer);
	IFERR_RETURN(err, nullptr)

	ASSERT_TOKEN(err, lexer, TokenType::ROUND_CB, nullptr)
	lexer.Next();

	result->bodyStatement = ParseStatement(err, lexer);
	IFERR_RETURN(err, nullptr)

	return result;
}

static Ref<WhileStatement> ParseWhileStatement(ErrorStream &err, Lexer &lexer)
{
	Ref<WhileStatement> result = Allocate<WhileStatement>();

	if (lexer.Get().type == TokenType::DO)
	{
		lexer.Next();

		result->checkAfterBody = true;

		result->bodyStatement = ParseBlockStatement(err, lexer);
		IFERR_RETURN(err, nullptr)

		ASSERT_TOKEN(err, lexer, TokenType::WHILE, nullptr)
		lexer.Next();

		result->condition = ParseExpression(err, lexer);
		IFERR_RETURN(err, nullptr)

		ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
		lexer.Next();
	}
	else if (lexer.Get().type == TokenType::WHILE)
	{
		lexer.Next();

		result->checkAfterBody = false;

		result->condition = ParseExpression(err, lexer);
		IFERR_RETURN(err, nullptr)

		result->bodyStatement = ParseBlockStatement(err, lexer);
		IFERR_RETURN(err, nullptr)
	}
	else
	{
		err.PrintError(lexer.Get(), "Unexpected token " + ToString(lexer.Get().type) + "! Expected either do or while!");
		return nullptr;
	}

	return result;
}

static Ref<ReturnStatement> ParseReturnStatement(ErrorStream &err, Lexer &lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::RETURN, nullptr)
	lexer.Next();

	Ref<ReturnStatement> result = Allocate<ReturnStatement>();

	if (lexer.Get().type == TokenType::SEMICOLON)
	{
		lexer.Next();
	}
	else
	{
		result->expression = ParseExpression(err, lexer);
		IFERR_RETURN(err, nullptr)

		ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
		lexer.Next();
	}

	return result;
}

static Ref<Statement> ParseTokenStatement(ErrorStream &err, Lexer &lexer, StatementType type)
{
	lexer.Next();

	Ref<Statement> result = Allocate<Statement>(type);

	ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
	lexer.Next();

	return result;
}

static Ref<ExpressionStatement> ParseExpressionStatement(ErrorStream &err, Lexer &lexer)
{
	Ref<ExpressionStatement> result = Allocate<ExpressionStatement>();

	result->expression = ParseExpression(err, lexer);
	IFERR_RETURN(err, nullptr)

	ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
	lexer.Next();

	return result;
}

static Ref<VariableDeclarationStatement> ParseVariableDeclarationStatement(ErrorStream &err, Lexer &lexer)
{
	Ref<VariableDeclarationStatement> result = Allocate<VariableDeclarationStatement>();
	result->declaration = Allocate<VariableDeclaration>();

	result->declaration->dataType = ParseDataType(err, lexer);
	IFERR_RETURN(err, nullptr)

	ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
	result->declaration->name = lexer.Next().data.stringData;

	if (lexer.Get().type == TokenType::EQUALS)
	{
		lexer.Next();

		result->value = ParseExpression(err, lexer);
		IFERR_RETURN(err, nullptr)
	}

	ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
	lexer.Next();

	return result;
}

static Ref<Statement> ParseStatement(ErrorStream &err, Lexer &lexer)
{
	switch (lexer.Get().type)
	{
	case TokenType::SEMICOLON:
		lexer.Next();
		return Allocate<Statement>(StatementType::NOP);
	case TokenType::CURLY_OB:
		return ParseBlockStatement(err, lexer);
	case TokenType::IF:
		return ParseIfStatement(err, lexer);
	case TokenType::FOR:
		return ParseForStatement(err, lexer);
	case TokenType::DO:
	case TokenType::WHILE:
		return ParseWhileStatement(err, lexer);
	case TokenType::RETURN:
		return ParseReturnStatement(err, lexer);
	case TokenType::BREAK:
		return ParseTokenStatement(err, lexer, StatementType::BREAK);
	case TokenType::CONTINUE:
		return ParseTokenStatement(err, lexer, StatementType::CONTINUE);
	default:
	{
		lexer.Push();
		err.Try();

		Ref<Statement> result = ParseExpressionStatement(err, lexer);

		if (err.Catch())
		{
			lexer.Revert();

			result = ParseVariableDeclarationStatement(err, lexer);
			IFERR_RETURN(err, nullptr)
		}
		else
		{
			lexer.Pop();
		}

		return result;
	}
	}
	return nullptr;
}

struct pair_hash
{
	template <class T1, class T2>
	std::size_t operator()(const std::pair<T1, T2> &pair) const
	{
		return std::hash<T1>()(pair.first) ^ std::hash<T2>()(pair.second);
	}
};

static HashMap<Pair<TokenType, TokenType>, OperatorType, pair_hash> operatorTypes = {
	// Non mutating binary operators
	{{TokenType::PLUS, TokenType::ROUND_OB}, OperatorType::PLUS},
	{{TokenType::MINUS, TokenType::ROUND_OB}, OperatorType::MINUS},
	{{TokenType::STAR, TokenType::ROUND_OB}, OperatorType::MULTIPLY},
	{{TokenType::SLASH, TokenType::ROUND_OB}, OperatorType::DIVIDE},
	{{TokenType::AND, TokenType::ROUND_OB}, OperatorType::AND},
	{{TokenType::OR, TokenType::ROUND_OB}, OperatorType::OR},
	{{TokenType::POWER, TokenType::ROUND_OB}, OperatorType::XOR},
	{{TokenType::LESS, TokenType::LESS}, OperatorType::SHIFT_LEFT},
	{{TokenType::GREATER, TokenType::GREATER}, OperatorType::SHIFT_RIGHT},
	{{TokenType::GREATER, TokenType::ROUND_OB}, OperatorType::GREATER},
	{{TokenType::LESS, TokenType::ROUND_OB}, OperatorType::LESS},
	{{TokenType::EQUALS, TokenType::EQUALS}, OperatorType::EQUAL},
	{{TokenType::NOT, TokenType::EQUALS}, OperatorType::NOT_EQUAL},
	{{TokenType::GREATER, TokenType::EQUALS}, OperatorType::GREATER_EQUAL},
	{{TokenType::LESS, TokenType::EQUALS}, OperatorType::LESS_EQUAL},
	// Misc binary operators
	{{TokenType::SQUARE_OB, TokenType::SQUARE_CB}, OperatorType::ARRAY_ACCESS},
	// Mutating binary operators
	{{TokenType::PLUS, TokenType::EQUALS}, OperatorType::PLUS_EQUAL},
	{{TokenType::MINUS, TokenType::EQUALS}, OperatorType::MINUS_EQUAL},
	{{TokenType::STAR, TokenType::EQUALS}, OperatorType::MULTIPLY_EQUAL},
	{{TokenType::SLASH, TokenType::EQUALS}, OperatorType::DIVIDE_EQUAL},
	{{TokenType::AND, TokenType::EQUALS}, OperatorType::AND_EQUAL},
	{{TokenType::OR, TokenType::EQUALS}, OperatorType::OR_EQUAL},
	{{TokenType::POWER, TokenType::EQUALS}, OperatorType::XOR_EQUAL},
	// Non mutating unary operators
	// Don't implement NEGATIVE since the difference betweeen NEGATIVE and MINUS depends on the parameter count.
	{{TokenType::NOT, TokenType::ROUND_OB}, OperatorType::NOT},
	{{TokenType::TILDE, TokenType::ROUND_OB}, OperatorType::INVERSE},
	// Mutating unary operators
	{{TokenType::PLUS, TokenType::PLUS}, OperatorType::INCREMENT},
	{{TokenType::MINUS, TokenType::MINUS}, OperatorType::DECREMENT}};

static HashSet<OperatorType> binaryOperatorSet = {
	// Non mutating binary operators
	OperatorType::PLUS,
	OperatorType::MINUS,
	OperatorType::MULTIPLY,
	OperatorType::DIVIDE,
	OperatorType::AND,
	OperatorType::OR,
	OperatorType::XOR,
	OperatorType::SHIFT_LEFT,
	OperatorType::SHIFT_RIGHT,
	OperatorType::GREATER,
	OperatorType::LESS,
	OperatorType::EQUAL,
	OperatorType::NOT_EQUAL,
	OperatorType::GREATER_EQUAL,
	OperatorType::LESS_EQUAL,
	// Misc binary operators
	OperatorType::ARRAY_ACCESS,
	// Mutating binary operators
	OperatorType::PLUS_EQUAL,
	OperatorType::MINUS_EQUAL,
	OperatorType::MULTIPLY_EQUAL,
	OperatorType::DIVIDE_EQUAL,
	OperatorType::AND_EQUAL,
	OperatorType::OR_EQUAL,
	OperatorType::XOR_EQUAL};

static bool IsBinaryOperator(OperatorType type)
{
	return binaryOperatorSet.find(type) != binaryOperatorSet.end();
}

static Ref<VariableDeclaration> ParseMemberDeclaration(ErrorStream &err, Lexer &lexer, const String &unitName)
{
	DeclarationFlags flags = ParseVisibilityFlags(lexer);

	bool isConstructor = false;
	bool isDestructor = false;
	bool isInternal = false;
	bool isInline = false;
	bool isOperator = false;
	OperatorType operatorType;

	Ref<DataType> dataType;
	String name;

	if (lexer.Get().type == TokenType::IDENTIFIER && lexer.Get().data.stringData == unitName)
	{
		lexer.Push();
		lexer.Next();

		if (lexer.Get().type == TokenType::ROUND_OB)
		{
			isConstructor = true;
			lexer.Pop();
		}
		else
		{
			lexer.Revert();
		}
	}

	if (isConstructor)
	{
		dataType = primitiveTypes.at(TokenType::VOID);

		ASSERT_TOKEN(err, lexer, TokenType::ROUND_OB, nullptr)
	}
	else if (lexer.Get().type == TokenType::OPERATOR)
	{
		lexer.Next();

		isOperator = true;

		if (lexer.Get().type == TokenType::LESS)
		{
			lexer.Next();

			operatorType = OperatorType::EXPLICIT_CAST;

			dataType = ParseDataType(err, lexer);
			IFERR_RETURN(err, nullptr)

			ASSERT_TOKEN(err, lexer, TokenType::GREATER, nullptr)
			lexer.Next();
		}
		else
		{
			operatorType = OperatorType::IMPLICIT_CAST;

			dataType = ParseDataType(err, lexer);
			IFERR_RETURN(err, nullptr)
		}
	}
	else if (lexer.Get().type == TokenType::TILDE)
	{
		lexer.Next();

		ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)

		if (lexer.Get().data.stringData != unitName)
		{
			err.PrintError(lexer.Get(), "Destructor name must match the unit name!");
			return nullptr;
		}

		lexer.Next();

		isDestructor = true;
		dataType = primitiveTypes.at(TokenType::VOID);
	}
	else
	{
		dataType = ParseDataType(err, lexer);
		IFERR_RETURN(err, nullptr)

		isInternal = lexer.Get().type == TokenType::INTERNAL;
		if (isInternal)
		{
			lexer.Next();
		}

		isInline = lexer.Get().type == TokenType::INLINE;
		if (isInline)
		{
			lexer.Next();
		}

		isOperator = lexer.Get().type == TokenType::OPERATOR;
		if (isOperator)
		{
			lexer.Next();

			Pair<TokenType, TokenType> operatorTokens;

			operatorTokens.first = lexer.Next().type;

			operatorTokens.second = lexer.Get().type;
			if (operatorTokens.second != TokenType::ROUND_OB)
			{
				lexer.Next();
			}

			if (operatorTokens.first != TokenType::ROUND_OB && operatorTokens.second != TokenType::ROUND_OB)
			{
				ASSERT_TOKEN(err, lexer, TokenType::ROUND_OB, nullptr)
			}

			if (operatorTypes.find(operatorTokens) == operatorTypes.end())
			{
				err.PrintError(lexer.Get(), "Invalid operator!");
				return nullptr;
			}

			operatorType = operatorTypes.at(operatorTokens);
		}
		else
		{
			ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
			name = lexer.Next().data.stringData;
		}
	}

	if (isConstructor || isDestructor || isOperator || lexer.Get().type == TokenType::ROUND_OB)
	{
		lexer.Next();

		Ref<MethodDeclaration> result;
		if (isConstructor)
		{
			result = Allocate<ConstructorDeclaration>();
		}
		else if (isDestructor)
		{
			result = Allocate<MethodDeclaration>();
			result->methodType = MethodType::DESTRUCTOR;
		}
		else if (isOperator)
		{
			result = Allocate<OperatorDeclaration>();
		}
		else
		{
			result = Allocate<MethodDeclaration>();
		}

		result->flags = flags;
		result->name = name;
		result->dataType = dataType;

		result->methodType = MethodType::METHOD;
		if (isConstructor)
		{
			result->methodType = MethodType::CONSTRUCTOR;
		}
		else if (isDestructor)
		{
			result->methodType = MethodType::DESTRUCTOR;
		}

		ParseParameterList(err, lexer, result->parameters, TokenType::ROUND_CB);
		IFERR_RETURN(err, nullptr)

		result->flags = result->flags | ParseDeclarationFlags(lexer);

		if (isConstructor)
		{
			Ref<ConstructorDeclaration> constructor = std::dynamic_pointer_cast<ConstructorDeclaration>(result);

			if (lexer.Get().type == TokenType::COLON)
			{
				lexer.Next();

				while (lexer.HasNext())
				{
					ConstructorInitializer initializer;

					ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
					initializer.name = lexer.Next().data.stringData;

					ASSERT_TOKEN(err, lexer, TokenType::ROUND_OB, nullptr)

					initializer.tempValue = ScanAndSkipNested(err, lexer, TokenType::ROUND_OB, TokenType::ROUND_CB);
					IFERR_RETURN(err, nullptr)

					constructor->initializers.push_back(initializer);

					if (lexer.Get().type != TokenType::COMMA)
					{
						break;
					}
					lexer.Next();
				}
			}
		}
		else if (isOperator)
		{
			if (result->parameters.size() > 1)
			{
				err.PrintError(lexer.Get(), "Invalid parameter count " + std::to_string(result->parameters.size()) + " for operator! Operators either have 0, 1 parameter.");
				return nullptr;
			}

			Ref<OperatorDeclaration> operatorDeclaration = std::dynamic_pointer_cast<OperatorDeclaration>(result);
			if (operatorType == OperatorType::MINUS)
			{
				if (result->parameters.empty())
				{
					operatorDeclaration->operatorType = OperatorType::NEGATIVE;
				}
				else
				{
					operatorDeclaration->operatorType = OperatorType::MINUS;
				}
			}
			else
			{
				operatorDeclaration->operatorType = operatorType;

				if ((result->parameters.size() == 1) != IsBinaryOperator(operatorType))
				{
					err.PrintError(lexer.Get(), "Invalid parameter count " + std::to_string(result->parameters.size()) + " for binary operator " + ToString(operatorType) + "!");
					return nullptr;
				}
			}
		}

		if (lexer.Get().type == TokenType::CURLY_OB)
		{
			lexer.Push(); // remove Push/Revert and tempBody

			result->body = ParseBlockStatement(err, lexer);
			IFERR_RETURN(err, nullptr)

			lexer.Revert();

			result->tempBody = ScanAndSkipNested(err, lexer, TokenType::CURLY_OB, TokenType::CURLY_CB);
			IFERR_RETURN(err, nullptr)
		}
		else
		{
			ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr);
			lexer.Next();
		}

		return result;
	}

	Ref<MemberVariableDeclaration> result = Allocate<MemberVariableDeclaration>();
	result->flags = flags;
	result->name = name;
	result->dataType = dataType;

	if (lexer.Get().type == TokenType::CURLY_OB)
	{
		lexer.Next();

		while (lexer.HasNext())
		{
			if (lexer.Get().type == TokenType::CURLY_CB)
			{
				lexer.Next();
				break;
			}

			Ref<MethodDeclaration> accessor = ParseAccessor(err, lexer);
			IFERR_RETURN(err, nullptr)

			result->accessors.push_back(accessor);
		}
	}

	if (lexer.Get().type == TokenType::EQUALS)
	{
		lexer.Next();

		// TODO: parse default assignment
	}

	ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
	lexer.Next();

	return result;
}

static void ParseSuperTypeList(ErrorStream &err, Lexer &lexer, Array<Ref<ObjectType>> &target, TokenType endToken)
{
	while (lexer.HasNext())
	{
		const Token &startToken = lexer.Get();

		Ref<DataType> superType = ParseDataType(err, lexer);

		if (superType->dataTypeType != DataTypeType::OBJECT)
		{
			err.PrintError(startToken, "A super type must be a object type!");
			return;
		}

		target.push_back(std::dynamic_pointer_cast<ObjectType>(superType));

		if (lexer.Get().type == endToken)
		{
			return;
		}

		ASSERT_TOKEN(err, lexer, TokenType::COMMA, )
		lexer.Next();
	}
}

static Ref<UnitDeclaration> ParseTypeDeclaration(ErrorStream &err, Lexer &lexer, const String &unitName, bool isClass, bool singleton)
{
	Ref<TypeDeclaration> result = isClass ? Allocate<ClassDeclaration>(singleton) : Allocate<TypeDeclaration>();

	ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
	if (lexer.Get().data.stringData != unitName)
	{
		err.PrintError(lexer.Get(), "The declared name must match the unit name!");
	}
	lexer.Next();

	if (lexer.Get().type == TokenType::LESS)
	{
		lexer.Next();

		result->typeTemplate = ParseTemplateDeclaration(err, lexer);
		IFERR_RETURN(err, nullptr)
	}

	if (lexer.Get().type == TokenType::COLON)
	{
		lexer.Next();

		ParseSuperTypeList(err, lexer, result->superTypes, TokenType::CURLY_OB);
		IFERR_RETURN(err, nullptr)
	}

	ASSERT_TOKEN(err, lexer, TokenType::CURLY_OB, nullptr)
	lexer.Next();

	while (lexer.HasNext())
	{
		if (lexer.Get().type == TokenType::CURLY_CB)
		{
			lexer.Next();
			break;
		}

		result->members.push_back(ParseMemberDeclaration(err, lexer, unitName));
		IFERR_RETURN(err, nullptr)
	}

	return result;
}

static HashSet<TokenType> unitDeclarationTypeSet = {TokenType::ERROR, TokenType::CLASS, TokenType::SINGLETON, TokenType::TYPE};

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
		declaration = ParseTypeDeclaration(err, lexer, unitName, true, false);
		break;
	case TokenType::SINGLETON:
		declaration = ParseTypeDeclaration(err, lexer, unitName, true, true);
		break;
	case TokenType::TYPE:
		declaration = ParseTypeDeclaration(err, lexer, unitName, false, false);
		break;
	default:
		return nullptr;
	}
	IFERR_RETURN(err, nullptr)

	declaration->flags = flags;

	IFERR_RETURN(err, nullptr)

	return declaration;
}

Ref<Unit> ParseUnit(ErrorStream &err, Lexer lexer, const String &name)
{
	Ref<Unit> unit = Allocate<Unit>();
	unit->name = name;

	while (lexer.HasNext())
	{
		const Token &token = lexer.Get();

		if (token.type == TokenType::USING)
		{
			lexer.Next();
			unit->dependencyNames.push_back(ParseUsing(err, lexer));
			IFERR_RETURN(err, nullptr)
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
		IFERR_RETURN(err, nullptr)
	}

	return unit;
}
