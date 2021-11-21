#ifndef SOURCE_AST_ASTITEM
#define SOURCE_AST_ASTITEM

#include "../Base.h"
#include "../Lexer.h"

STRICT_ENUM(ASTItemType, MODULE, UNIT)

class ASTItem
{
protected:
	ASTItemType type;
	TokenStream lexer;

public:
	ASTItem(ASTItemType type, const TokenStream &lexer)
		: type(type), lexer(lexer)
	{
	}

	virtual Bool ParseStructure() = 0;
	virtual Bool ParseImplementation() = 0;
	virtual Bool Link() = 0;

public:
	ASTItemType GetType() const
	{
		return type;
	}
};

#endif /* SOURCE_AST_ASTITEM */
