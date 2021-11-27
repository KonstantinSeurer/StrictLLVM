#ifndef SOURCE_AST_DECLARATION
#define SOURCE_AST_DECLARATION

#include "../Base.h"

STRICT_ENUM(VisibilityFlags, PRIVATE = 0, INTERNAL = 1, PROTECTED = 2, PUBLIC = 7)
STRICT_ENUM(DeclarationFlags, NONE = 0, MUT = 1, IMPURE = 2)

class UnitDeclaration : public ASTItem
{
public:
	VisibilityFlags visibility;

public:
	UnitDeclaration(ASTItemType type)
		: ASTItem(type)
	{
	}

public:
	virtual JSON GetStructureJSON() const
	{
		return JSON(JSON::value_t::array);
	}
};

class ErrorDeclaration : public UnitDeclaration
{
public:
	Int32 value;
	bool hasValue;

public:
	ErrorDeclaration()
		: UnitDeclaration(ASTItemType::ERROR_DECLARATION)
	{
	}

public:
};

class ClassDeclaration : public UnitDeclaration
{
public:
public:
	ClassDeclaration()
		: UnitDeclaration(ASTItemType::CLASS_DECLARATION)
	{
	}

public:
};

class TypeDeclaration : public UnitDeclaration
{
public:
public:
	TypeDeclaration()
		: UnitDeclaration(ASTItemType::TYPE_DECLARATION)
	{
	}

public:
};

#endif /* SOURCE_AST_DECLARATION */
