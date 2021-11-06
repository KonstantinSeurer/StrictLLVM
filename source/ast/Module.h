#ifndef SOURCE_AST_MODULE
#define SOURCE_AST_MODULE

#include "Unit.h"
#include "../JSON.h"

STRICT_ENUM(ModuleType, STATIC, DYNAMIC, INLINE)
STRICT_ENUM(TargetFlags, NONE = 0, BIT32 = 1, BIT64 = 2, X86 = 4, LINUX = 8)

bool IsTargetActive(TargetFlags target, TargetFlags buildTarget);
TargetFlags JSONToTargetFlags(const JSON &json);

class Module
{
private:
	String name;
	Array<Ref<Unit>> units;
	Array<Ref<Module>> dependencies;

public:
	Module(const String &name, const Array<Ref<Unit>> &units, const Array<Ref<Module>> &dependencies)
		: name(name), units(units), dependencies(dependencies)
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
