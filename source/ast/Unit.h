#ifndef SOURCE_AST_UNIT
#define SOURCE_AST_UNIT

#include "../JSON.h"
#include "ASTItem.h"

STRICT_ENUM(UnitType, ERROR, CLASS, SINGLETON)

class Unit : public ASTItem
{
public:
	UnitType unitType;
	Array<String> dependencyNames;
	String name;

public:
	Unit()
		: ASTItem(ASTItemType::UNIT)
	{
	}

	Unit(const JSON &structureJSON);

public:
	JSON GetStructureJSON() const;
};

#endif /* SOURCE_AST_UNIT */
