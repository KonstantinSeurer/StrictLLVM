
#include "Parser.h"

#include <iostream>

static String ParseUsing(ErrorStream& err, Lexer& lexer)
{
	String dependency;

	while (lexer.HasNext())
	{
		const Token& token = lexer.Next();

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

#define ASSERT_TOKEN(err, lexer, tokenType, returnValue)                                           \
	if (lexer.Get().type != tokenType)                                                             \
	{                                                                                              \
		err.PrintError(lexer.Get(), String("Unexpected token ") + ToString(lexer.Get().type) +     \
		                                " expected " + ToString(tokenType) + "!");                 \
		return returnValue;                                                                        \
	}

#define IFERR_RETURN(err, returnValue)                                                             \
	if (err.HasErrorOccured())                                                                     \
	{                                                                                              \
		return returnValue;                                                                        \
	}

static HashMap<TokenType, DeclarationFlags> declarationFlags = {
	{TokenType::PRIVATE, DeclarationFlags::PRIVATE},
	{TokenType::PROTECTED, DeclarationFlags::PROTECTED},
	{TokenType::INTERNAL, DeclarationFlags::INTERNAL},
	{TokenType::EXTERNAL, DeclarationFlags::EXTERNAL},
	{TokenType::PUBLIC, DeclarationFlags::PUBLIC},
	{TokenType::MUT, DeclarationFlags::MUT},
	{TokenType::IMPURE, DeclarationFlags::IMPURE},
	{TokenType::VIRTUAL, DeclarationFlags::VIRTUAL}};

static DeclarationFlags ParseDeclarationFlags(Lexer& lexer)
{
	DeclarationFlags flags = DeclarationFlags::PRIVATE;

	while (lexer.HasNext())
	{
		const Token& token = lexer.Get();

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

static DeclarationFlags ParseVisibilityFlags(Lexer& lexer)
{
	DeclarationFlags flags = DeclarationFlags::PRIVATE;

	while (lexer.HasNext())
	{
		const Token& token = lexer.Get();

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

static Ref<UnitDeclaration> ParseErrorDeclaration(ErrorStream& err, Lexer& lexer,
                                                  const String& unitName)
{
	Ref<ErrorDeclaration> result = Allocate<ErrorDeclaration>();
	result->characterIndex = lexer.Get().characterIndex;

	ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
	result->name = lexer.Get().data.stringData;
	if (result->name != unitName)
	{
		err.PrintError(lexer.Get(), "The declared name must match the unit name!");
		return nullptr;
	}
	lexer.Next();

	if (lexer.Get().type == TokenType::EQUALS)
	{
		lexer.Next();

		result->hasValue = true;

		const Token& valueToken = lexer.Next();
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
			err.PrintError(lexer.Get(),
			               String("Unexpected token ") + ToString(valueToken.type) + "!");
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

static HashSet<TokenType> primitiveTypeSet = {
	TokenType::VOID,  TokenType::BOOL,   TokenType::INT8,    TokenType::UINT8,
	TokenType::INT16, TokenType::UINT16, TokenType::INT32,   TokenType::UINT32,
	TokenType::INT64, TokenType::UINT64, TokenType::FLOAT32, TokenType::FLOAT64};

static Ref<DataType> ParseDataType(ErrorStream& err, Lexer& lexer);

static Ref<Expression> ParseExpression(ErrorStream& err, Lexer& lexer, UInt32 basePrecedence = 0);

static Ref<Template> ParseTemplate(ErrorStream& err, Lexer& lexer)
{
	Ref<Template> result = Allocate<Template>();
	result->characterIndex = lexer.Get().characterIndex;

	while (lexer.HasNext())
	{
		if (lexer.Get().type == TokenType::GREATER)
		{
			lexer.Next();
			return result;
		}

		lexer.Push();
		err.Try();

		TemplateArgument argument;

		argument.expression = ParseExpression(err, lexer);

		if (err.Catch())
		{
			lexer.Revert();
			lexer.Push();
			err.Try();

			argument.dataType = ParseDataType(err, lexer);

			if (err.Catch())
			{
				lexer.Revert();
				err.PrintError("Expected either a data type or an expression!");
				return nullptr;
			}
			else
			{
				lexer.Pop();
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

static Ref<DataType> ParseDataType(ErrorStream& err, Lexer& lexer)
{
	Ref<DataType> result;

	DeclarationFlags flags = ParseDeclarationFlags(lexer);

	if (lexer.Get().type == TokenType::ROUND_OB)
	{
		lexer.Next();

		result = ParseDataType(err, lexer);
		IFERR_RETURN(err, nullptr)

		result->characterIndex = lexer.Get().characterIndex;

		result->flags = result->flags | flags;
	}
	else
	{
		if (lexer.Get().type == TokenType::TYPE)
		{
			lexer.Next();

			result = Allocate<DataType>();
			result->characterIndex = lexer.Get().characterIndex;
			result->flags = flags;
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
			result->characterIndex = lexer.Get().characterIndex;
		}
		else
		{
			const TokenType primitiveType = lexer.Get().type;

			if (primitiveTypeSet.find(primitiveType) == primitiveTypeSet.end())
			{
				err.PrintError(lexer.Get(), String("Unexpected token ") + ToString(primitiveType) +
				                                " expected primitive type!");
				return nullptr;
			}

			result = Allocate<PrimitiveType>(primitiveType);
			result->characterIndex = lexer.Get().characterIndex;
			result->flags = flags;
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
		case TokenType::STAR: {
			pointerResult->dataTypeType = DataTypeType::POINTER;
			break;
		}
		case TokenType::AND: {
			pointerResult->dataTypeType = DataTypeType::REFERENCE;
			break;
		}
		case TokenType::SQUARE_OB: {
			pointerResult->dataTypeType = DataTypeType::ARRAY;
			lexer.Next();

			if (lexer.Get().type != TokenType::SQUARE_CB)
			{
				pointerResult->arrayLength = ParseExpression(err, lexer);
				IFERR_RETURN(err, nullptr)
			}

			ASSERT_TOKEN(err, lexer, TokenType::SQUARE_CB, nullptr)
			break;
		}
		default:
			return result;
		}

		lexer.Next();

		result = pointerResult;
		result->characterIndex = lexer.Get().characterIndex;
	}

	return result;
}

static void ParseParameterList(ErrorStream& err, Lexer& lexer,
                               Array<Ref<VariableDeclaration>>& target, TokenType endToken)
{
	while (lexer.HasNext())
	{
		if (lexer.Get().type == endToken)
		{
			lexer.Next();
			return;
		}

		Ref<VariableDeclaration> argument = Allocate<VariableDeclaration>();
		argument->characterIndex = lexer.Get().characterIndex;

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

static Ref<TemplateDeclaration> ParseTemplateDeclaration(ErrorStream& err, Lexer& lexer)
{
	Ref<TemplateDeclaration> result = Allocate<TemplateDeclaration>();
	result->characterIndex = lexer.Get().characterIndex;

	ParseParameterList(err, lexer, result->parameters, TokenType::GREATER);
	IFERR_RETURN(err, nullptr)

	return result;
}

static Ref<LiteralExpression> ParseLiteralExpression(Lexer& lexer)
{
	Ref<LiteralExpression> result = Allocate<LiteralExpression>();
	result->characterIndex = lexer.Get().characterIndex;

	result->data = lexer.Next();

	return result;
}

static Ref<IdentifierExpression> ParseIdentifierExpression(Lexer& lexer)
{
	Ref<IdentifierExpression> result = Allocate<IdentifierExpression>();
	result->characterIndex = lexer.Get().characterIndex;

	result->name = lexer.Next().data.stringData;

	return result;
}

static Ref<Expression> ParseBracketExpression(ErrorStream& err, Lexer& lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::ROUND_OB, nullptr)
	lexer.Next();

	Ref<BracketExpression> result = Allocate<BracketExpression>();
	result->characterIndex = lexer.Get().characterIndex;

	result->expression = ParseExpression(err, lexer);
	IFERR_RETURN(err, nullptr)

	ASSERT_TOKEN(err, lexer, TokenType::ROUND_CB, nullptr)
	lexer.Next();

	return result;
}

#define ACCESS_PRECEDENCE 80
#define CAST_PRECEDENCE 70
#define UNARY_PRECEDENCE 60
#define ARITHMETIC_PRECEDENCE 50
#define COMPARE_PRECEDENCE 40
#define LOGIC_PRECEDENCE 30
#define BINARY_MUTATE_PRECEDENCE 10

#define PRECEDENCE_FACTOR 1000

static HashMap<OperatorType, UInt32> operatorPrecedences = { //
	{OperatorType::NONE, 0},
	// Non mutating binary operators
	{OperatorType::PLUS, ARITHMETIC_PRECEDENCE},
	{OperatorType::MINUS, ARITHMETIC_PRECEDENCE},
	{OperatorType::MULTIPLY, ARITHMETIC_PRECEDENCE + 1},
	{OperatorType::DIVIDE, ARITHMETIC_PRECEDENCE + 1},
	{OperatorType::AND, LOGIC_PRECEDENCE},
	{OperatorType::AND_AND, LOGIC_PRECEDENCE},
	{OperatorType::OR, LOGIC_PRECEDENCE},
	{OperatorType::OR_OR, LOGIC_PRECEDENCE},
	{OperatorType::XOR, LOGIC_PRECEDENCE},
	{OperatorType::SHIFT_LEFT, LOGIC_PRECEDENCE},
	{OperatorType::SHIFT_RIGHT, LOGIC_PRECEDENCE},
	{OperatorType::GREATER, COMPARE_PRECEDENCE},
	{OperatorType::LESS, COMPARE_PRECEDENCE},
	{OperatorType::EQUAL, COMPARE_PRECEDENCE},
	{OperatorType::NOT_EQUAL, COMPARE_PRECEDENCE},
	{OperatorType::GREATER_EQUAL, COMPARE_PRECEDENCE},
	{OperatorType::LESS_EQUAL, COMPARE_PRECEDENCE},
	// Mutating binary operators
	{OperatorType::PLUS_EQUAL, BINARY_MUTATE_PRECEDENCE + 2},
	{OperatorType::MINUS_EQUAL, BINARY_MUTATE_PRECEDENCE + 2},
	{OperatorType::MULTIPLY_EQUAL, BINARY_MUTATE_PRECEDENCE + 3},
	{OperatorType::DIVIDE_EQUAL, BINARY_MUTATE_PRECEDENCE + 3},
	{OperatorType::AND_EQUAL, BINARY_MUTATE_PRECEDENCE + 1},
	{OperatorType::OR_EQUAL, BINARY_MUTATE_PRECEDENCE + 1},
	{OperatorType::XOR_EQUAL, BINARY_MUTATE_PRECEDENCE + 1},
	{OperatorType::SHIFT_LEFT_EQUAL, BINARY_MUTATE_PRECEDENCE},
	{OperatorType::SHIFT_RIGHT_EQUAL, BINARY_MUTATE_PRECEDENCE},
	{OperatorType::ASSIGN, BINARY_MUTATE_PRECEDENCE},
	// Misc binary operators
	{OperatorType::ARRAY_ACCESS, ACCESS_PRECEDENCE},
	// Non mutating unary operators
	{OperatorType::NEGATIVE, UNARY_PRECEDENCE},
	{OperatorType::NOT, UNARY_PRECEDENCE},
	{OperatorType::INVERSE, UNARY_PRECEDENCE},
	{OperatorType::EXPLICIT_CAST, CAST_PRECEDENCE},
	{OperatorType::DEREFERENCE, CAST_PRECEDENCE},
	// Mutating unary operators
	{OperatorType::INCREMENT, UNARY_PRECEDENCE + 2},
	{OperatorType::DECREMENT, UNARY_PRECEDENCE + 2},
	// Internal operators
	{OperatorType::ACCESS, ACCESS_PRECEDENCE + 2},
	{OperatorType::CALL, CAST_PRECEDENCE - 1},
	{OperatorType::POST_STAR, ACCESS_PRECEDENCE + 1},
	{OperatorType::POST_AND, ACCESS_PRECEDENCE + 1},
	{OperatorType::TEMPLATE, CAST_PRECEDENCE}};

static UInt32 GetOperatorPrecedence(OperatorType type)
{
	return operatorPrecedences.at(type) * PRECEDENCE_FACTOR;
}

static HashSet<OperatorType> binaryOperatorSet = {
	// Non mutating binary operators
	OperatorType::PLUS, OperatorType::MINUS, OperatorType::MULTIPLY, OperatorType::DIVIDE,
	OperatorType::AND, OperatorType::AND_AND, OperatorType::OR, OperatorType::OR_OR,
	OperatorType::XOR, OperatorType::SHIFT_LEFT, OperatorType::SHIFT_RIGHT, OperatorType::GREATER,
	OperatorType::LESS, OperatorType::EQUAL, OperatorType::NOT_EQUAL, OperatorType::GREATER_EQUAL,
	OperatorType::LESS_EQUAL,
	// Misc binary operators
	OperatorType::ARRAY_ACCESS,
	// Mutating binary operators
	OperatorType::PLUS_EQUAL, OperatorType::MINUS_EQUAL, OperatorType::MULTIPLY_EQUAL,
	OperatorType::DIVIDE_EQUAL, OperatorType::AND_EQUAL, OperatorType::OR_EQUAL,
	OperatorType::XOR_EQUAL, OperatorType::SHIFT_LEFT_EQUAL, OperatorType::SHIFT_RIGHT_EQUAL,
	OperatorType::ASSIGN,
	// Internal operators
	OperatorType::ACCESS};

static bool IsBinaryOperator(OperatorType type)
{
	return binaryOperatorSet.find(type) != binaryOperatorSet.end();
}

static Ref<OperatorExpression> ParsePrimaryUnaryExpression(ErrorStream& err, Lexer& lexer)
{
	Ref<OperatorExpression> result = Allocate<OperatorExpression>();
	result->characterIndex = lexer.Get().characterIndex;

	switch (lexer.Get().type)
	{
	case TokenType::MINUS:
		result->operatorType = OperatorType::MINUS;
		break;
	case TokenType::NOT:
		result->operatorType = OperatorType::NOT;
		break;
	case TokenType::TILDE:
		result->operatorType = OperatorType::INVERSE;
		break;
	default:
		err.PrintError(lexer.Get(), "Unexpected token " + ToString(lexer.Get().type) +
		                                "! Expected '-', '!' or '~'.");
		return nullptr;
	}
	lexer.Next();

	result->a = ParseExpression(err, lexer, GetOperatorPrecedence(result->operatorType));
	IFERR_RETURN(err, nullptr)

	return result;
}

static Ref<NewExpression> ParseNewExpression(ErrorStream& err, Lexer& lexer)
{
	Ref<NewExpression> result = Allocate<NewExpression>();
	result->characterIndex = lexer.Get().characterIndex;
	result->allocationType = AllocationType::HEAP;

	ASSERT_TOKEN(err, lexer, TokenType::NEW, nullptr)
	lexer.Next();

	result->dataType = ParseDataType(err, lexer);
	IFERR_RETURN(err, nullptr)

	if (lexer.Get().type == TokenType::ROUND_OB)
	{
		lexer.Next();

		while (lexer.HasNext())
		{
			if (lexer.Get().type == TokenType::ROUND_CB)
			{
				lexer.Next();
				break;
			}

			result->arguments.push_back(ParseExpression(err, lexer));
			IFERR_RETURN(err, nullptr)

			if (lexer.Get().type == TokenType::ROUND_CB)
			{
				lexer.Next();
				break;
			}

			ASSERT_TOKEN(err, lexer, TokenType::COMMA, nullptr)
			lexer.Next();
		}
	}

	return result;
}

static Ref<Expression> ParsePrimaryExpression(ErrorStream& err, Lexer& lexer)
{
	switch (lexer.Get().type)
	{
	case TokenType::INT_LITERAL:
	case TokenType::UINT_LITERAL:
	case TokenType::FLOAT_LITERAL:
	case TokenType::STRING_LITERAL:
		return ParseLiteralExpression(lexer);
	case TokenType::IDENTIFIER:
		return ParseIdentifierExpression(lexer);
	case TokenType::ROUND_OB:
		return ParseBracketExpression(err, lexer);
	case TokenType::MINUS:
	case TokenType::NOT:
	case TokenType::TILDE:
		return ParsePrimaryUnaryExpression(err, lexer);
	case TokenType::NEW:
		return ParseNewExpression(err, lexer);
	default:
		err.PrintError(lexer.Get(), "Unexpected token " + ToString(lexer.Get().type) + "!");
		return nullptr;
	}

	return nullptr;
}

static OperatorType ParseOperatorType(ErrorStream& err, Lexer& lexer)
{
	switch (lexer.Next().type)
	{
	case TokenType::PLUS:
		if (lexer.Get().type == TokenType::PLUS)
		{
			lexer.Next();
			return OperatorType::INCREMENT;
		}

		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::PLUS_EQUAL;
		}

		return OperatorType::PLUS;
	case TokenType::MINUS:
		if (lexer.Get().type == TokenType::MINUS)
		{
			lexer.Next();
			return OperatorType::DECREMENT;
		}

		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::MINUS_EQUAL;
		}

		return OperatorType::MINUS;
	case TokenType::STAR:
		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::MULTIPLY_EQUAL;
		}

		return OperatorType::MULTIPLY;
	case TokenType::SLASH:
		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::DIVIDE_EQUAL;
		}

		return OperatorType::DIVIDE;
	case TokenType::AND:
		if (lexer.Get().type == TokenType::AND)
		{
			lexer.Next();
			return OperatorType::AND_AND;
		}

		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::AND_EQUAL;
		}

		return OperatorType::AND;
	case TokenType::OR:
		if (lexer.Get().type == TokenType::OR)
		{
			lexer.Next();
			return OperatorType::OR_OR;
		}

		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::OR_EQUAL;
		}

		return OperatorType::OR;
	case TokenType::POWER:
		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::XOR_EQUAL;
		}

		return OperatorType::XOR;
	case TokenType::GREATER:
		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::GREATER_EQUAL;
		}

		if (lexer.Get().type == TokenType::GREATER)
		{
			lexer.Next();

			if (lexer.Get().type == TokenType::EQUALS)
			{
				lexer.Next();
				return OperatorType::SHIFT_RIGHT_EQUAL;
			}

			return OperatorType::SHIFT_RIGHT;
		}

		return OperatorType::GREATER;
	case TokenType::LESS:
		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::LESS_EQUAL;
		}

		if (lexer.Get().type == TokenType::LESS)
		{
			lexer.Next();

			if (lexer.Get().type == TokenType::EQUALS)
			{
				lexer.Next();
				return OperatorType::SHIFT_LEFT_EQUAL;
			}

			return OperatorType::SHIFT_LEFT;
		}

		return OperatorType::LESS;
	case TokenType::EQUALS:
		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::EQUAL;
		}

		return OperatorType::ASSIGN;
	case TokenType::NOT:
		if (lexer.Get().type == TokenType::EQUALS)
		{
			lexer.Next();
			return OperatorType::NOT_EQUAL;
		}

		break;
	case TokenType::SQUARE_OB:
		return OperatorType::ARRAY_ACCESS;
	case TokenType::PERIOD:
		return OperatorType::ACCESS;
	case TokenType::ROUND_OB:
		return OperatorType::CALL;
	default:
		break;
	}

	lexer.Prev();
	err.PrintError(lexer.Get(),
	               "Unexpected token " + ToString(lexer.Get().type) + " for operator type!");
	lexer.Next();

	return OperatorType::NONE;
}

static HashSet<TokenType> expressionEndTokens = {TokenType::SEMICOLON, TokenType::COMMA,
                                                 TokenType::COLON,     TokenType::ROUND_CB,
                                                 TokenType::SQUARE_CB, TokenType::QUESTIONMARK};

static Ref<Expression> ParseBinaryOperatorExpression(ErrorStream& err, Lexer& lexer,
                                                     Ref<Expression> left, UInt32 basePrecedence)
{
	while (true)
	{
		if (expressionEndTokens.find(lexer.Get().type) != expressionEndTokens.end())
		{
			return left;
		}

		OperatorType operatorType = ParseOperatorType(err, lexer);
		IFERR_RETURN(err, nullptr)

		Ref<DataType> dataType;
		Ref<Template> typeTemplate;
		if (operatorType == OperatorType::LESS)
		{
			if (lexer.Get().type == TokenType::GREATER)
			{
				lexer.Next();

				operatorType = OperatorType::DEREFERENCE;
			}
			else
			{
				lexer.Push();
				err.Try();

				dataType = ParseDataType(err, lexer);

				if (err.Catch() || lexer.Get().type != TokenType::GREATER)
				{
					lexer.Revert();
					lexer.Push();
					err.Try();

					typeTemplate = ParseTemplate(err, lexer);

					if (err.Catch())
					{
						lexer.Revert();
					}
					else
					{
						lexer.Pop();

						operatorType = OperatorType::TEMPLATE;
					}
				}
				else
				{
					lexer.Pop();
					lexer.Next();

					operatorType = OperatorType::EXPLICIT_CAST;
				}
			}
		}

		const UInt32 precedence = GetOperatorPrecedence(operatorType);

		if (precedence < basePrecedence)
		{
			return left;
		}

		if (operatorType == OperatorType::CALL)
		{
			Ref<CallExpression> call = Allocate<CallExpression>();
			call->characterIndex = lexer.Get().characterIndex;
			call->method = left;

			while (lexer.HasNext())
			{
				if (lexer.Get().type == TokenType::ROUND_CB)
				{
					lexer.Next();
					break;
				}

				call->arguments.push_back(ParseExpression(err, lexer));
				IFERR_RETURN(err, nullptr)

				if (lexer.Get().type == TokenType::ROUND_CB)
				{
					lexer.Next();
					break;
				}

				ASSERT_TOKEN(err, lexer, TokenType::COMMA, nullptr)
				lexer.Next();
			}

			if (expressionEndTokens.find(lexer.Get().type) != expressionEndTokens.end())
			{
				return call;
			}

			left = call;
		}
		else
		{
			Ref<OperatorExpression> op = Allocate<OperatorExpression>();
			op->characterIndex = lexer.Get().characterIndex;
			op->a = left;
			op->operatorType = operatorType;
			op->b = SecondOperand();

			if (operatorType == OperatorType::EXPLICIT_CAST)
			{
				op->b->dataType = dataType;
			}
			else if (operatorType == OperatorType::TEMPLATE)
			{
				op->b->typeTemplate = typeTemplate;
			}
			else if (IsBinaryOperator(operatorType))
			{
				if (operatorType == OperatorType::ARRAY_ACCESS)
				{
					op->b->expression = ParseExpression(err, lexer);
					IFERR_RETURN(err, nullptr)

					ASSERT_TOKEN(err, lexer, TokenType::SQUARE_CB, nullptr)
					lexer.Next();
				}
				else if (operatorType == OperatorType::MULTIPLY ||
				         operatorType == OperatorType::AND)
				{
					err.Try();
					lexer.Push();

					op->b->expression = ParsePrimaryExpression(err, lexer);

					if (err.Catch())
					{
						lexer.Revert();
						// This is part os a data type instead of an operator since data types are
						// valid expressions. This expression will be converted into a data type by
						// ResolveIdentifiers.
						if (operatorType == OperatorType::MULTIPLY)
						{
							operatorType = OperatorType::POST_STAR;
						}
						else
						{
							operatorType = OperatorType::POST_AND;
						}
						op->operatorType = operatorType;
					}
					else
					{
						lexer.Pop();
					}
				}
				else
				{
					op->b->expression = ParsePrimaryExpression(err, lexer);
					IFERR_RETURN(err, nullptr)
				}
			}
			else
			{
				op->b.reset();
			}

			if (expressionEndTokens.find(lexer.Get().type) != expressionEndTokens.end())
			{
				return op;
			}

			lexer.Push();
			const OperatorType nextOperatorType = ParseOperatorType(err, lexer);
			lexer.Revert();
			IFERR_RETURN(err, nullptr)

			const UInt32 nextPrecedence = GetOperatorPrecedence(nextOperatorType);

			if (nextPrecedence > precedence && operatorType != OperatorType::EXPLICIT_CAST)
			{
				op->b->expression =
					ParseBinaryOperatorExpression(err, lexer, op->b->expression, precedence + 1);
				IFERR_RETURN(err, nullptr)
			}

			left = op;
		}
	}

	return nullptr;
}

static Ref<Expression> ParseExpression(ErrorStream& err, Lexer& lexer, UInt32 basePrecedence)
{
	Ref<Expression> left = ParsePrimaryExpression(err, lexer);
	IFERR_RETURN(err, nullptr)

	Ref<Expression> expression = ParseBinaryOperatorExpression(err, lexer, left, basePrecedence);
	IFERR_RETURN(err, nullptr)

	if (lexer.Get().type == TokenType::QUESTIONMARK)
	{
		lexer.Next();

		Ref<TernaryExpression> result = Allocate<TernaryExpression>();
		result->characterIndex = lexer.Get().characterIndex;
		result->condition = expression;

		result->thenExpression = ParseExpression(err, lexer);
		IFERR_RETURN(err, nullptr)

		ASSERT_TOKEN(err, lexer, TokenType::COLON, nullptr)
		lexer.Next();

		result->elseExpression = ParseExpression(err, lexer);
		IFERR_RETURN(err, nullptr)

		return result;
	}

	return expression;
}

static Ref<Statement> ParseStatement(ErrorStream& err, Lexer& lexer);

static Ref<BlockStatement> ParseBlockStatement(ErrorStream& err, Lexer& lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::CURLY_OB, nullptr)
	lexer.Next();

	Ref<BlockStatement> result = Allocate<BlockStatement>();
	result->characterIndex = lexer.Get().characterIndex;

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

static Ref<IfStatement> ParseIfStatement(ErrorStream& err, Lexer& lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::IF, nullptr)
	lexer.Next();

	ASSERT_TOKEN(err, lexer, TokenType::ROUND_OB, nullptr)
	lexer.Next();

	Ref<IfStatement> result = Allocate<IfStatement>();
	result->characterIndex = lexer.Get().characterIndex;

	result->condition = ParseExpression(err, lexer);
	IFERR_RETURN(err, nullptr)

	ASSERT_TOKEN(err, lexer, TokenType::ROUND_CB, nullptr)
	lexer.Next();

	result->thenStatement = ParseStatement(err, lexer);
	IFERR_RETURN(err, nullptr)

	if (lexer.Get().type == TokenType::ELSE)
	{
		lexer.Next();

		result->elseStatement = ParseStatement(err, lexer);
		IFERR_RETURN(err, nullptr)
	}

	return result;
}

static Ref<ForStatement> ParseForStatement(ErrorStream& err, Lexer& lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::FOR, nullptr)
	lexer.Next();

	Ref<ForStatement> result = Allocate<ForStatement>();
	result->characterIndex = lexer.Get().characterIndex;

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

static Ref<WhileStatement> ParseWhileStatement(ErrorStream& err, Lexer& lexer)
{
	Ref<WhileStatement> result = Allocate<WhileStatement>();
	result->characterIndex = lexer.Get().characterIndex;

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
		err.PrintError(lexer.Get(), "Unexpected token " + ToString(lexer.Get().type) +
		                                "! Expected either do or while!");
		return nullptr;
	}

	return result;
}

static Ref<ReturnStatement> ParseReturnStatement(ErrorStream& err, Lexer& lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::RETURN, nullptr)
	lexer.Next();

	Ref<ReturnStatement> result = Allocate<ReturnStatement>();
	result->characterIndex = lexer.Get().characterIndex;

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

static Ref<Statement> ParseTokenStatement(ErrorStream& err, Lexer& lexer, StatementType type)
{
	lexer.Next();

	Ref<Statement> result = Allocate<Statement>(type);
	result->characterIndex = lexer.Get().characterIndex;

	ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
	lexer.Next();

	return result;
}

static Ref<ExpressionStatement> ParseExpressionStatement(ErrorStream& err, Lexer& lexer)
{
	Ref<ExpressionStatement> result = Allocate<ExpressionStatement>();
	result->characterIndex = lexer.Get().characterIndex;

	result->expression = ParseExpression(err, lexer);
	IFERR_RETURN(err, nullptr)

	ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
	lexer.Next();

	return result;
}

static Ref<VariableDeclarationStatement> ParseVariableDeclarationStatement(ErrorStream& err,
                                                                           Lexer& lexer)
{
	Ref<VariableDeclarationStatement> result = Allocate<VariableDeclarationStatement>();
	result->characterIndex = lexer.Get().characterIndex;
	result->declaration = Allocate<VariableDeclaration>();
	result->declaration->characterIndex = lexer.Get().characterIndex;

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

static Ref<DeleteStatement> ParseDeleteStatement(ErrorStream& err, Lexer& lexer)
{
	ASSERT_TOKEN(err, lexer, TokenType::DELETE, nullptr)
	lexer.Next();

	Ref<DeleteStatement> result = Allocate<DeleteStatement>();
	result->characterIndex = lexer.Get().characterIndex;

	result->expression = ParseExpression(err, lexer);
	IFERR_RETURN(err, nullptr)

	ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
	lexer.Next();

	return result;
}

static Ref<Statement> ParseStatement(ErrorStream& err, Lexer& lexer)
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
	case TokenType::DELETE:
		return ParseDeleteStatement(err, lexer);
	default: {
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

		result->characterIndex = lexer.Get().characterIndex;

		return result;
	}
	}
	return nullptr;
}

static Ref<MethodDeclaration> ParseAccessor(ErrorStream& err, Lexer& lexer)
{
	Ref<MethodDeclaration> result = Allocate<MethodDeclaration>(MethodType::METHOD);
	result->characterIndex = lexer.Get().characterIndex;
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
		err.PrintError(lexer.Get(), "Unexpected token " + ToString(lexer.Get().type) +
		                                "! Expected either get or set.");
		return nullptr;
	}

	ASSERT_TOKEN(err, lexer, TokenType::ROUND_OB, nullptr)
	lexer.Next();

	ParseParameterList(err, lexer, result->parameters, TokenType::ROUND_CB);
	IFERR_RETURN(err, nullptr)

	if (lexer.Get().type == TokenType::CURLY_OB)
	{
		result->body = ParseBlockStatement(err, lexer);
		IFERR_RETURN(err, nullptr)
	}
	else
	{
		ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
		lexer.Next();
	}

	return result;
}

struct pair_hash
{
	template <class T1, class T2> std::size_t operator()(const std::pair<T1, T2>& pair) const
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
	// Don't implement NEGATIVE since the difference betweeen NEGATIVE and
	// MINUS depends on the parameter count.
	{{TokenType::NOT, TokenType::ROUND_OB}, OperatorType::NOT},
	{{TokenType::TILDE, TokenType::ROUND_OB}, OperatorType::INVERSE},
	// Mutating unary operators
	{{TokenType::PLUS, TokenType::PLUS}, OperatorType::INCREMENT},
	{{TokenType::MINUS, TokenType::MINUS}, OperatorType::DECREMENT}};

static Ref<VariableDeclaration> ParseMemberDeclaration(ErrorStream& err, Lexer& lexer,
                                                       const String& unitName)
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
		dataType = Allocate<PrimitiveType>(TokenType::VOID);

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
		dataType = Allocate<PrimitiveType>(TokenType::VOID);
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

			if (operatorTokens.first != TokenType::ROUND_OB &&
			    operatorTokens.second != TokenType::ROUND_OB)
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
			result = Allocate<MethodDeclaration>(MethodType::DESTRUCTOR);
		}
		else if (isOperator)
		{
			result = Allocate<OperatorDeclaration>();
		}
		else
		{
			result = Allocate<MethodDeclaration>(MethodType::METHOD);
		}
		result->characterIndex = lexer.Get().characterIndex;

		result->flags = flags;
		result->name = name;
		result->dataType = dataType;

		ParseParameterList(err, lexer, result->parameters, TokenType::ROUND_CB);
		IFERR_RETURN(err, nullptr)

		result->flags = result->flags | ParseDeclarationFlags(lexer);

		if (isConstructor)
		{
			Ref<ConstructorDeclaration> constructor =
				std::dynamic_pointer_cast<ConstructorDeclaration>(result);

			if (lexer.Get().type == TokenType::COLON)
			{
				lexer.Next();

				while (lexer.HasNext())
				{
					ConstructorInitializer initializer;

					ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
					initializer.name = lexer.Next().data.stringData;

					ASSERT_TOKEN(err, lexer, TokenType::ROUND_OB, nullptr)
					lexer.Next();

					initializer.value = ParseExpression(err, lexer);
					IFERR_RETURN(err, nullptr)

					ASSERT_TOKEN(err, lexer, TokenType::ROUND_CB, nullptr)
					lexer.Next();

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
				err.PrintError(lexer.Get(), "Invalid parameter count " +
				                                std::to_string(result->parameters.size()) +
				                                " for operator! Operators either have "
				                                "0, 1 parameter.");
				return nullptr;
			}

			Ref<OperatorDeclaration> operatorDeclaration =
				std::dynamic_pointer_cast<OperatorDeclaration>(result);
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
					err.PrintError(lexer.Get(), "Invalid parameter count " +
					                                std::to_string(result->parameters.size()) +
					                                " for binary operator " +
					                                ToString(operatorType) + "!");
					return nullptr;
				}
			}
		}

		if (lexer.Get().type == TokenType::CURLY_OB)
		{
			result->body = ParseBlockStatement(err, lexer);
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

		result->value = ParseExpression(err, lexer);
		IFERR_RETURN(err, nullptr)
	}

	ASSERT_TOKEN(err, lexer, TokenType::SEMICOLON, nullptr)
	lexer.Next();

	return result;
}

static void ParseSuperTypeList(ErrorStream& err, Lexer& lexer, Array<Ref<ObjectType>>& target,
                               TokenType endToken)
{
	while (lexer.HasNext())
	{
		const Token& startToken = lexer.Get();

		Ref<DataType> superType = ParseDataType(err, lexer);
		superType->characterIndex = lexer.Get().characterIndex;

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

static Ref<UnitDeclaration> ParseTypeDeclaration(ErrorStream& err, Lexer& lexer,
                                                 const String& unitName, bool isClass,
                                                 bool singleton)
{
	Ref<TypeDeclaration> result =
		isClass ? Allocate<ClassDeclaration>(singleton) : Allocate<TypeDeclaration>();
	result->characterIndex = lexer.Get().characterIndex;

	ASSERT_TOKEN(err, lexer, TokenType::IDENTIFIER, nullptr)
	result->name = lexer.Get().data.stringData;
	if (result->name != unitName)
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

static HashSet<TokenType> unitDeclarationTypeSet = {TokenType::ERROR, TokenType::CLASS,
                                                    TokenType::SINGLETON, TokenType::TYPE};

static Ref<UnitDeclaration> ParseUnitDeclaration(ErrorStream& err, Lexer& lexer,
                                                 const String& unitName)
{
	DeclarationFlags flags = ParseDeclarationFlags(lexer);

	const Token& token = lexer.Next();

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

Ref<Unit> ParseUnit(ErrorStream& err, Lexer lexer, const String& name)
{
	Ref<Unit> unit = Allocate<Unit>();
	unit->characterIndex = lexer.Get().characterIndex;
	unit->name = name;
	unit->unitMeta.lexer = lexer;

	while (lexer.HasNext())
	{
		const Token& token = lexer.Get();

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
			err.PrintError(token, "Declaring multiple types in the "
			                      "same unit is not allowed!");
			return nullptr;
		}

		unit->declaredType = ParseUnitDeclaration(err, lexer, name);
		IFERR_RETURN(err, nullptr)
	}

	return unit;
}
