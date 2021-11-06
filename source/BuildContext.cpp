
#include "BuildContext.h"

#include <iostream>
#include <filesystem>
#include <fstream>

String BuildContext::ResolveModulePath(const String &moduleName) const
{
	String moduleNameCopy = moduleName;
	std::replace(moduleNameCopy.begin(), moduleNameCopy.end(), '.', '/');
	const String moduleSubDirectory = String("/") + moduleNameCopy;
	const String moduleFile = moduleSubDirectory + "/module.json";

	for (const String &path : modulePath)
	{
		String canonicalModuleFile = path + moduleFile;
		if (std::filesystem::exists(canonicalModuleFile))
		{
			return path + moduleSubDirectory;
		}
	}

	std::cerr << "Unable to resolve module '" << moduleName << "'!" << std::endl;
	return "";
}

void BuildContext::AddModule(const String &moduleName)
{
	if (taskSet.find(moduleName) != taskSet.end())
	{
		return;
	}

	const String modulePath = ResolveModulePath(moduleName);
	const String moduleFile = modulePath + "/module.json";

	std::ifstream moduleFileStream(moduleFile);
	const String moduleFileContent = String(std::istreambuf_iterator<char>(moduleFileStream), std::istreambuf_iterator<char>());

	const JSON moduleJSON = JSON::parse(moduleFileContent);

	if (moduleJSON.contains("requires"))
	{
		AddRequiredModules(moduleJSON["requires"]);
	}

	taskSet[moduleName] = HashSet<String>();
	taskList.push_back(Pair<String, Array<String>>(moduleName, Array<String>()));

	if (moduleJSON.contains("targets"))
	{
		AddTargetUnits(moduleName, moduleJSON["targets"]);
	}
}

void BuildContext::AddRequiredModules(const JSON &moduleRequiresJSON)
{
	for (const auto &requireJSON : moduleRequiresJSON)
	{
		AddModule(String(requireJSON));
	}
}

void BuildContext::AddTargetUnits(const String &moduleName, const JSON &moduleTargetsJSON)
{
	for (const auto &targetJSON : moduleTargetsJSON)
	{
		if (!targetJSON.contains("exports"))
		{
			continue;
		}

		TargetFlags flags = targetJSON.contains("flags") ? JSONToTargetFlags(targetJSON["flags"]) : TargetFlags::NONE;

		if (!IsTargetActive(flags, target))
		{
			continue;
		}

		const JSON exportsJSON = targetJSON["exports"];
		for (const auto &exportJSON : exportsJSON)
		{
			const String sourceFile = String(exportJSON);

			taskSet[moduleName].insert(sourceFile);
			taskList[taskList.size() - 1].second.push_back(sourceFile);
		}
	}
}

void BuildContext::Build()
{
}
