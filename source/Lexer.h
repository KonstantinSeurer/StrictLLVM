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

STRICT_ENUM(TokenType,
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
			RETURN,

			// Structure
			PUBLIC,
			PRIVATE,
			PROTECTED,
			INTERNAL,
			EXTERNAL,
			INLINE,
			MUT,
			IMPURE,
			VIRTUAL,
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
			UINT_LITERAL,
			FLOAT_LITERAL,
			STRING_LITERAL,

			// Other symbols
			IDENTIFIER,
			NEW,
			DELETE,
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
			EQUALS,
			LESS,
			GREATER,
			TILDE,
			POWER);

const String &ToString(TokenType type);

struct Token
{
public:
	TokenData data;
	TokenType type;
	UInt32 characterIndex;

public:
	bool operator==(const Token &other) const;
};

class Lexer
{
private:
	Ref<const String> source;
	Ref<const Array<Token>> tokens;
	Int64 offset;
	Int64 length;
	Array<Int64> stack;

public:
	Lexer()
	{
	}

	Lexer(Ref<const String> source, Ref<const Array<Token>> tokens)
		: source(source), tokens(tokens), offset(0), length(tokens->size())
	{
	}

	Lexer(const Lexer &source, Int64 position, Int64 length)
		: source(source.source), tokens(source.tokens), offset(position), length(std::min(position + length, (Int64)tokens->size()))
	{
	}

	~Lexer();

	static Ref<Lexer> Create(const String &source);

	const Token &Get() const;
	const Token &Next();
	void Prev();
	Bool HasNext() const;

	void Push();
	void Pop();
	void Revert();

	const String &GetSource() const
	{
		return *source;
	}

	Int64 GetPosition() const
	{
		return offset;
	}
};

#endif /* SOURCE_LEXER */
