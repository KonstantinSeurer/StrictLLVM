
#include "Lexer.h"

Token::~Token()
{
	if (type == TokenType::STRING_LITERAL || type == TokenType::IDENTIFIER)
	{
		delete[] data.stringData;
	}
}

TokenStream::~TokenStream()
{
	if (ownsMemory)
	{
		delete[] tokens;
	}
}

Ref<TokenStream> TokenStream::Create(const String &source)
{
	Array<Token> *tokens = (Array<Token> *)malloc(sizeof(Array<Token>));

	for (UInt64 index = 0; index < source.length(); index++)
	{
		const char currentChar = source[index];

		if (isspace(currentChar))
		{
			continue;
		}

		if (isdigit(currentChar))
		{
			// Numeric literal
		}
		else if (isalnum(currentChar))
		{
			// Identifyer of keyword
		}
		else if (currentChar == '\'')
		{
			// character literal (type = INT_LITERAL)
		}
		else if (currentChar == '\"')
		{
			// STRING_LITERAL
		}
		else
		{
			// Other token
		}
	}

	auto result = allocate<TokenStream>(tokens->data(), tokens->size(), true);
	free(tokens);
	return result;
}

const Token &TokenStream::Get() const
{
	return tokens[offset];
}

const Token &TokenStream::Next()
{
	const Token &result = tokens[offset];
	offset++;
	return result;
}

void TokenStream::Push()
{
	stack.push_back(offset);
}

void TokenStream::Pop()
{
	stack.erase(stack.begin() + stack.size() - 1);
}

void TokenStream::Revert()
{
	offset = stack[stack.size() - 1];
	Pop();
}
