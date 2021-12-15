
#include "Lexer.h"

#include <string.h>
#include <iostream>

Lexer::~Lexer()
{
	if (tokens.use_count() == 1)
	{
		for (const Token &token : *tokens)
		{
			if (token.type == TokenType::STRING_LITERAL || token.type == TokenType::IDENTIFIER)
			{
				delete token.data.stringData;
			}
		}
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
	{"UInt32", TokenType::UINT32},
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
	{"inline", TokenType::INLINE},
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
	{"operator", TokenType::OPERATOR},

	// Other symbols
	{"new", TokenType::NEW},
	{"delete", TokenType::DELETE}};

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
	{'~', TokenType::TILDE},
	{'^', TokenType::POWER}};

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

Ref<Lexer> Lexer::Create(const String &source)
{
	Ref<Array<Token>> tokens = Allocate<Array<Token>>();

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
			Token token;
			token.characterIndex = index;

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

						token.type = TokenType::UINT_LITERAL;
						token.data.uintData = strtoull(string.c_str(), nullptr, base);
						tokens->push_back(token);
					}
				}
				else if (string[0] == '-')
				{
					token.type = TokenType::INT_LITERAL;
					token.data.intData = strtoll(string.c_str(), nullptr, 10);
					tokens->push_back(token);
				}
				else
				{
					token.type = TokenType::UINT_LITERAL;
					token.data.uintData = strtoull(string.c_str(), nullptr, 10);
					tokens->push_back(token);
				}
			}
			else
			{
				token.type = TokenType::FLOAT_LITERAL;
				token.data.floatData = strtod(string.c_str(), nullptr);
				tokens->push_back(token);
			}
		}
		else if (isalpha(currentChar))
		{
			Token token;
			token.characterIndex = index;

			// Identifyer or keyword
			const UInt64 startIndex = index;
			for (; index < source.length() && isalnum(source[index]); index++)
				;
			String string = source.substr(startIndex, index - startIndex);

			if (keyWordTable.find(string) != keyWordTable.end())
			{
				token.type = keyWordTable.at(string);
				tokens->push_back(token);
			}
			else if (string == "true")
			{
				token.type = TokenType::UINT_LITERAL;
				token.data.uintData = 1;
				tokens->push_back(token);
			}
			else if (string == "false" || string == "null")
			{
				token.type = TokenType::UINT_LITERAL;
				token.data.uintData = 0;
				tokens->push_back(token);
			}
			else
			{
				char *data = new char[string.length() + 1];
				strcpy(data, string.c_str());

				token.type = TokenType::IDENTIFIER;
				token.data.stringData = data;
				tokens->push_back(token);
			}
		}
		else if (currentChar == '\'')
		{
			Token token;
			token.characterIndex = index;

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

			token.type = TokenType::INT_LITERAL;
			token.data.intData = value;
			tokens->push_back(token);
		}
		else if (currentChar == '\"')
		{
			Token token;
			token.characterIndex = index;

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
			token.characterIndex = index;
			token.type = singleCharacterTokenTable.at(currentChar);
			tokens->push_back(token);

			index++;
		}
	}

	return Allocate<Lexer>(Allocate<String>(source), tokens);
}

const Token &Lexer::Get() const
{
	return tokens->operator[](offset);
}

const Token &Lexer::Next()
{
	const Token &result = Get();
	offset++;
	return result;
}

void Lexer::Prev()
{
	if (offset <= 0)
	{
		return;
	}

	offset--;
}

Bool Lexer::HasNext() const
{
	return offset < length - 1;
}

void Lexer::Push()
{
	stack.push_back(offset);
}

void Lexer::Pop()
{
	stack.erase(stack.begin() + stack.size() - 1);
}

void Lexer::Revert()
{
	offset = stack[stack.size() - 1];
	Pop();
}
