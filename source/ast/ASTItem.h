#ifndef SOURCE_AST_ASTITEM
#define SOURCE_AST_ASTITEM

#include "../Base.h"
#include "../Lexer.h"

STRICT_ENUM(ASTItemType, MODULE, UNIT, CLASS_DECLARATION, ERROR_DECLARATION, TYPE_DECLARATION)

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
