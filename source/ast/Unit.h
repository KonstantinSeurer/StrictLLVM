#ifndef SOURCE_AST_UNIT
#define SOURCE_AST_UNIT

#include "../JSON.h"
#include "ASTItem.h"
#include "Declaration.h"

class Unit : public ASTItem
{
public:
	Array<String> dependencyNames;
	String name;
	Ref<UnitDeclaration> declaredType;

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
