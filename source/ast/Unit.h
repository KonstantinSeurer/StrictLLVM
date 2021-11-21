#ifndef SOURCE_AST_UNIT
#define SOURCE_AST_UNIT

#include "../JSON.h"
#include "ASTItem.h"

STRICT_ENUM(UnitType, ERROR, CLASS, SINGLETON)

class Unit : public ASTItem
{
private:
	UnitType type;
	Array<String> dependencyNames;
	String name;

public:
	Unit(const TokenStream &lexer)
		: ASTItem(ASTItemType::UNIT, lexer)
	{
	}

	Unit(const JSON &structureJSON);

public:
	virtual Bool ParseStructure();
	virtual Bool ParseImplementation();
	virtual Bool Link();

public:
	UnitType GetUnitType() const
	{
		return type;
	}

	const Array<String> &GetDependencyNames() const
	{
		return dependencyNames;
	}

	const String &GetName() const
	{
		return name;
	}

	void SetLexer(const TokenStream &lexer)
	{
		this->lexer = lexer;
	}

	JSON GetStructureJSON() const;
};

#endif /* SOURCE_AST_UNIT */
