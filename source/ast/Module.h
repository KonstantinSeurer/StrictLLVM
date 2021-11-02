#ifndef SOURCE_AST_MODULE
#define SOURCE_AST_MODULE

#include "Unit.h"
#include "../JSON.h"

class Module
{
private:
	String name;
	Array<Ref<Unit>> units;

public:
	Module(const String &name, const Array<Ref<Unit>> &units)
		: name(name), units(units)
	{
	}

	Ref<Module> Create(const JSON &json);

	const String &getName() const
	{
		return name;
	}

	const Array<Ref<Unit>> &getUnits() const
	{
		return units;
	}

	Ref<Unit> getUnit(const String &name) const
	{
		for (auto unit : units)
		{
			if (unit->getData()->getName() == name)
			{
				return unit;
			}
		}

		return nullptr;
	}
};

#endif /* SOURCE_AST_MODULE */
