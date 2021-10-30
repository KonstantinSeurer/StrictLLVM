#ifndef SOURCE_LEXER
#define SOURCE_LEXER

#include "Base.h"

union TokenData
{
	Float64 floatData;
	Int64 intData;
	UInt64 uintData;
	Bool boolData;
	const char *stringData;
};

enum class TokenType
{
	// Data types
	BOOL,
	INT8,
	UINT8,
	INT16,
	UINT16,
	INT32,
	UINT32,
	INT64,
	UINT64,
	FLOAT32,
	FLOAT64,
	VOID,

	// Control flow
	IF,
	ELSE,
	FOR,
	WHILE,
	DO,
	BREAK,
	CONTINUE,
	TRY,
	CATCH,
	THROW,

	// Structure
	PUBLIC,
	PRIVATE,
	PROTECTED,
	INTERNAL,
	MUT,
	IMPURE,
	CLASS,
	SINGLETON,
	TYPE,
	ERROR,
	USING,
	GET,
	SET,
	OPERATOR,

	// Literals
	INT_LITERAL,
	FLOAT_LITERAL,
	STRING_LITERAL,
	BOOL_LITERAL,
	NULL_LITERAL,

	// Other symbols
	IDENTIFIER,
	ROUND_OB,
	ROUND_CB,
	CURLY_OB,
	CURLY_CB,
	SQUARE_OB,
	SQUARE_CB,
	PERIOD,
	SEMICOLON,
	COLON,
	COMMA,
	PLUS,
	MINUS,
	STAR,
	SLASH,
	AND,
	OR,
	NOT,
	QUESTIONMARK,
	QUOTE,
	DOUBLE_QUOTE,
	EQUALS
};

struct Token
{
	TokenType type;
	TokenData data;

	~Token();
};

class TokenStream
{
private:
	Token *tokens;
	Int64 offset;
	Array<Int64> stack;
	Int64 length;
	Bool ownsMemory;

public:
	TokenStream(Token *tokens, Int64 length, Bool ownsMemory)
		: tokens(tokens), length(length), ownsMemory(ownsMemory)
	{
	}

	~TokenStream();

	Ref<TokenStream> Create(const String &source);

	const Token &Get() const;
	const Token &Next();

	void Push();
	void Pop();
	void Revert();
};

#endif /* SOURCE_LEXER */
