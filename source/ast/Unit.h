#ifndef SOURCE_AST_UNIT
#define SOURCE_AST_UNIT

#include "../Base.h"
#include "../Lexer.h"

STRICT_ENUM(UnitType, ERROR, CLASS, SINGLETON)

class UnitData
{
private:
	String name;

public:
	UnitData(const String &name)
		: name(name)
	{
	}

	virtual ~UnitData()
	{
	}

	const String &getName() const
	{
		return name;
	}
};

class Unit
{
private:
	UnitType type;
	Array<Ref<Unit>> dependencies;
	Ref<UnitData> data;

public:
	Unit(UnitType type, const Array<Ref<Unit>> &dependencies, Ref<UnitData> data)
		: type(type), dependencies(dependencies), data(data)
	{
	}

	static Ref<Unit> Create(TokenStream &lexer);

	UnitType getType() const
	{
		return type;
	}

	const Array<Ref<Unit>> &getDependencies() const
	{
		return dependencies;
	}

	Ref<UnitData> getData() const
	{
		return data;
	}
};

#endif /* SOURCE_AST_UNIT */
