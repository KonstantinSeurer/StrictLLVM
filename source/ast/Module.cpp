
#include "Module.h"

#include <iostream>

bool IsTargetActive(TargetFlags target, TargetFlags buildTarget)
{
	return (target & buildTarget) == target; // target must be a subset of buildTarget to be active
}

TargetFlags JSONToTargetFlags(const JSON &json)
{
	TargetFlags flags = TargetFlags::NONE;

	for (const auto &flag : json)
	{
		flags = flags | StringToTargetFlags(String(flag));
	}

	return flags;
}

Ref<Module> Module::Create(const JSON &json)
{
	return nullptr;
}
