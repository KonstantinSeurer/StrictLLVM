
#include "AST.h"

Unit::Unit(const JSON &structureJSON)
	: ASTItem(ASTItemType::UNIT)
{
	name = structureJSON["name"];

	for (const JSON &dependencyName : structureJSON["dependencies"])
	{
		dependencyNames.push_back(String(dependencyName));
	}
}

JSON Unit::GetStructureJSON() const
{
	JSON result;
	JSON dependenciesJSON(JSON::value_t::array);

	for (UInt64 dependencyIndex = 0; dependencyIndex < dependencyNames.size(); dependencyIndex++)
	{
		dependenciesJSON[dependencyIndex] = dependencyNames[dependencyIndex];
	}

	result["dependencies"] = dependenciesJSON;
	result["name"] = name;

	return result;
}

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
