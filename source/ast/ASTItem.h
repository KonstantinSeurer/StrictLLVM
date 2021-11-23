#ifndef SOURCE_AST_ASTITEM
#define SOURCE_AST_ASTITEM

#include "../Base.h"
#include "../Lexer.h"

STRICT_ENUM(ASTItemType, MODULE, UNIT)

class ASTItem
{
protected:
	ASTItemType type;

public:
	ASTItem(ASTItemType type)
		: type(type)
	{
	}

public:
	ASTItemType GetType() const
	{
		return type;
	}
};

#endif /* SOURCE_AST_ASTITEM */
