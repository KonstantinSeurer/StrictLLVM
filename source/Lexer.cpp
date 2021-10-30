
#include "Lexer.h"

#include <string.h>
#include <iostream>

TokenStream::~TokenStream()
{
	if (ownsMemory)
	{
		for (UInt64 tokenIndex = 0; tokenIndex < length; tokenIndex++)
		{
			const Token &token = tokens[tokenIndex];

			if (token.type == TokenType::STRING_LITERAL || token.type == TokenType::IDENTIFIER)
			{
				delete token.data.stringData;
			}
		}

		delete tokens;
	}
}

static HashMap<String, TokenType> keyWordTable = {
	// Data types
	{"Bool", TokenType::BOOL},
	{"Int8", TokenType::INT8},
	{"UInt8", TokenType::UINT8},
	{"Int16", TokenType::INT16},
	{"UInt16", TokenType::UINT16},
	{"Int32", TokenType::INT32},
	{"Uint32", TokenType::UINT32},
	{"Int64", TokenType::INT64},
	{"UInt64", TokenType::UINT64},
	{"Float32", TokenType::FLOAT32},
	{"Float64", TokenType::FLOAT64},
	{"void", TokenType::VOID},

	// Control flow
	{"if", TokenType::IF},
	{"else", TokenType::ELSE},
	{"for", TokenType::FOR},
	{"while", TokenType::WHILE},
	{"do", TokenType::DO},
	{"break", TokenType::BREAK},
	{"continue", TokenType::CONTINUE},
	{"try", TokenType::TRY},
	{"catch", TokenType::CATCH},
	{"throw", TokenType::THROW},

	// Structure
	{"public", TokenType::PUBLIC},
	{"private", TokenType::PRIVATE},
	{"protected", TokenType::PROTECTED},
	{"internal", TokenType::INTERNAL},
	{"external", TokenType::EXTERNAL},
	{"virtual", TokenType::VIRTUAL},
	{"class", TokenType::CLASS},
	{"singleton", TokenType::SINGLETON},
	{"type", TokenType::TYPE},
	{"error", TokenType::ERROR},
	{"using", TokenType::USING},
	{"get", TokenType::GET},
	{"set", TokenType::SET},
	{"operator", TokenType::OPERATOR},

	// Literals
	{"null", TokenType::NULL_LITERAL}};

static HashMap<char, TokenType> singleCharacterTokenTable = {
	{'(', TokenType::ROUND_OB},
	{')', TokenType::ROUND_CB},
	{'{', TokenType::CURLY_OB},
	{'}', TokenType::CURLY_CB},
	{'[', TokenType::SQUARE_OB},
	{']', TokenType::SQUARE_CB},
	{'.', TokenType::PERIOD},
	{';', TokenType::SEMICOLON},
	{':', TokenType::COLON},
	{',', TokenType::COMMA},
	{'+', TokenType::PLUS},
	{'-', TokenType::MINUS},
	{'*', TokenType::STAR},
	{'/', TokenType::SLASH},
	{'&', TokenType::AND},
	{'|', TokenType::OR},
	{'!', TokenType::NOT},
	{'?', TokenType::QUESTIONMARK},
	{'=', TokenType::EQUALS},
	{'<', TokenType::LESS},
	{'>', TokenType::GREATER},
	{'~', TokenType::TILDE}};

Ref<TokenStream> TokenStream::Create(const String &source)
{
	Array<Token> *tokens = new Array<Token>; // irrelevant memory leak

	for (UInt64 index = 0; index < source.length();)
	{
		const char currentChar = source[index];

		if (isspace(currentChar))
		{
			index++;
			continue;
		}

		if (isdigit(currentChar))
		{
			// Numeric literal
			index++;
		}
		else if (isalpha(currentChar))
		{
			// Identifyer of keyword
			const UInt64 startIndex = index;
			for (; index < source.length() && isalnum(source[index]); index++)
				;
			String string = source.substr(startIndex, index - startIndex);

			if (keyWordTable.find(string) != keyWordTable.end())
			{
				Token token;
				token.type = keyWordTable.at(string);
				tokens->push_back(token);
			}
			else if (string == "true")
			{
				Token token;
				token.type = TokenType::BOOL_LITERAL;
				token.data.boolData = true;
				tokens->push_back(token);
			}
			else if (string == "false")
			{
				Token token;
				token.type = TokenType::BOOL_LITERAL;
				token.data.boolData = false;
				tokens->push_back(token);
			}
			else
			{
				char *data = new char[string.length() + 1];
				strncpy(data, string.c_str(), string.length());

				Token token;
				token.type = TokenType::IDENTIFIER;
				token.data.stringData = data;
				tokens->push_back(std::move(token));
			}
		}
		else if (currentChar == '\'')
		{
			// character literal (type = INT_LITERAL)
			index++;

			UInt64 value = source[index];
			index++;

			if (source[index] != '\'')
			{
				std::cerr << "Unexpected character '" << source[index] << "', expected '''!" << std::endl;
			}
			index++;

			Token token;
			token.type = TokenType::INT_LITERAL;
			token.data.uintData = value;
			tokens->push_back(token);
		}
		else if (currentChar == '\"')
		{
			// STRING_LITERAL
			index++;
		}
		else
		{
			// Other token
			if (singleCharacterTokenTable.find(currentChar) == singleCharacterTokenTable.end())
			{
				std::cerr << "Unexpected character '" << currentChar << "'!" << std::endl;
			}

			Token token;
			token.type = singleCharacterTokenTable.at(currentChar);
			tokens->push_back(token);

			index++;
		}
	}

	return allocate<TokenStream>(tokens->data(), tokens->size(), true);
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

Bool TokenStream::HasNext() const
{
	return offset < length - 1;
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
