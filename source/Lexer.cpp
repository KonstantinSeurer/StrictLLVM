
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
	{"return", TokenType::RETURN},

	// Structure
	{"public", TokenType::PUBLIC},
	{"private", TokenType::PRIVATE},
	{"protected", TokenType::PROTECTED},
	{"internal", TokenType::INTERNAL},
	{"external", TokenType::EXTERNAL},
	{"mut", TokenType::MUT},
	{"impure", TokenType::IMPURE},
	{"virtual", TokenType::VIRTUAL},
	{"class", TokenType::CLASS},
	{"singleton", TokenType::SINGLETON},
	{"type", TokenType::TYPE},
	{"error", TokenType::ERROR},
	{"using", TokenType::USING},
	{"get", TokenType::GET},
	{"set", TokenType::SET},
	{"operator", TokenType::OPERATOR}};

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

static HashMap<char, char> escapeSequenceTable = {
	{'\\', '\\'},
	{'a', '\a'},
	{'b', '\b'},
	{'f', '\f'},
	{'n', '\n'},
	{'r', '\r'},
	{'t', '\t'},
	{'v', '\v'}};

static char convertEscapeSequence(char character, char quote)
{
	if (character == quote)
	{
		return quote;
	}

	if (escapeSequenceTable.find(character) == escapeSequenceTable.end())
	{
		std::cerr << "Unexpected escape sequence '\\" << character << "'!" << std::endl;
		return 0;
	}

	return escapeSequenceTable.at(character);
}

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

		// skip single line comments
		if (index + 1 < source.length() && currentChar == '/' && source[index + 1] == '/')
		{
			for (; index < source.length(); index++)
			{
				if (source[index] == '\n')
				{
					break;
				}
			}

			continue;
		}

		if (isdigit(currentChar))
		{
			// Numeric literal

			const UInt64 startIndex = index;
			for (; index < source.length() && (isalnum(source[index]) || source[index] == '.'); index++)
				;
			String string = source.substr(startIndex, index - startIndex);

			if (string.find('.') == String::npos)
			{
				if (string[0] == '0')
				{
					if (string.length() == 1)
					{
						Token token;
						token.type = TokenType::UINT_LITERAL;
						token.data.uintData = 0;
						tokens->push_back(token);
					}
					else
					{
						Int32 base = 10;
						switch (string[1])
						{
						case 'x':
							base = 16;
							break;
						case 'b':
							base = 2;
							break;
						default:
							std::cerr << "Unexpected character '" << string[1] << "' for the base of an integer literal!" << std::endl;
							break;
						}

						Token token;
						token.type = TokenType::UINT_LITERAL;
						token.data.uintData = strtoull(string.c_str(), nullptr, base);
						tokens->push_back(token);
					}
				}
				else if (string[0] == '-')
				{
					Token token;
					token.type = TokenType::INT_LITERAL;
					token.data.intData = strtoll(string.c_str(), nullptr, 10);
					tokens->push_back(token);
				}
				else
				{
					Token token;
					token.type = TokenType::UINT_LITERAL;
					token.data.uintData = strtoull(string.c_str(), nullptr, 10);
					tokens->push_back(token);
				}
			}
			else
			{
				Token token;
				token.type = TokenType::FLOAT_LITERAL;
				token.data.floatData = strtod(string.c_str(), nullptr);
				tokens->push_back(token);
			}
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
				token.type = TokenType::UINT_LITERAL;
				token.data.uintData = 1;
				tokens->push_back(token);
			}
			else if (string == "false" || string == "null")
			{
				Token token;
				token.type = TokenType::UINT_LITERAL;
				token.data.uintData = 0;
				tokens->push_back(token);
			}
			else
			{
				char *data = new char[string.length() + 1];
				strcpy(data, string.c_str());

				Token token;
				token.type = TokenType::IDENTIFIER;
				token.data.stringData = data;
				tokens->push_back(token);
			}
		}
		else if (currentChar == '\'')
		{
			// character literal (type = INT_LITERAL)
			index++;

			char value = source[index];
			index++;

			if (value == '\\')
			{
				value = convertEscapeSequence(value, '\'');
				index++;
			}

			if (source[index] != '\'')
			{
				std::cerr << "Unexpected character '" << source[index] << "', expected '''!" << std::endl;
			}
			index++;

			Token token;
			token.type = TokenType::INT_LITERAL;
			token.data.intData = value;
			tokens->push_back(token);
		}
		else if (currentChar == '\"')
		{
			// STRING_LITERAL
			index++;

			String string;

			for (; index < source.length(); index++)
			{
				const char character = source[index];

				if (character == '"')
				{
					index++;
					break;
				}

				if (character == '\\')
				{
					// escaped character
					index++;

					string += convertEscapeSequence(source[index], '"');
				}
				else
				{
					string += character;
				}
			}

			char *data = new char[string.length() + 1];
			strncpy(data, string.c_str(), string.length());

			Token token;
			token.type = TokenType::STRING_LITERAL;
			token.data.stringData = data;
			tokens->push_back(token);
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
