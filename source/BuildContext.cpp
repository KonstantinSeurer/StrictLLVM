
#include "BuildContext.h"
#include "Time.h"

#include <iostream>
#include <filesystem>
#include <fstream>

bool operator==(const BuildTask &a, const BuildTask &b)
{
	return a.name == b.name;
}

bool operator==(const ModuleName &a, const ModuleName &b)
{
	return a.name == b.name;
}

BuildContext::BuildContext(const Array<String> &modulePath, const String &cachePath, TargetFlags target)
	: modulePath(modulePath), cachePath(cachePath), target(target)
{
	if (!std::filesystem::exists(cachePath))
	{
		std::filesystem::create_directory(cachePath);
	}
}

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

void BuildContext::AddModule(const String &name)
{
	if (taskSet.find(name) != taskSet.end())
	{
		return;
	}

	const String modulePath = ResolveModulePath(name);
	const String moduleFile = modulePath + "/module.json";

	const ModuleName moduleName(name, modulePath);

	std::ifstream moduleFileStream(moduleFile);
	const String moduleFileContent = String(std::istreambuf_iterator<char>(moduleFileStream), std::istreambuf_iterator<char>());

	const JSON moduleJSON = JSON::parse(moduleFileContent);

	if (moduleJSON.contains("requires"))
	{
		AddRequiredModules(moduleJSON["requires"]);
	}

	taskSet[moduleName] = HashSet<BuildTask>();
	taskList.push_back(Pair<ModuleName, Array<BuildTask>>(moduleName, Array<BuildTask>()));

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

void BuildContext::AddTargetUnits(const ModuleName &moduleName, const JSON &moduleTargetsJSON)
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
			AddTargetUnit(moduleName, String(exportJSON));
		}
	}
}

void BuildContext::AddTargetUnit(const ModuleName &moduleName, const String &sourceFile)
{
	BuildTask task(sourceFile, false);

	taskSet[moduleName].insert(task);
	taskList[taskList.size() - 1].second.push_back(task);
}

void BuildContext::Build()
{
	std::cout << "Generating rebuild graph...";
	Time graphGenStart;

	MarkChangedUnits();
	PropagateBuildFlag();
	ReduceTasks();

	std::cout.precision(1);
	std::cout << " (" << (Time() - graphGenStart).milliSeconds() << "ms)" << std::endl;

	// TODO: compile every task and write dependency information and ir to cache files
}

void BuildContext::MarkChangedUnits()
{
	const String lastWriteTimesFile = cachePath + "/lastWriteTimes.json";

	JSON lastWriteTimesJSON;
	if (std::filesystem::exists(lastWriteTimesFile))
	{
		std::ifstream lastWriteTimesStream(lastWriteTimesFile);
		const String lastWriteTimesContent = String(std::istreambuf_iterator<char>(lastWriteTimesStream), std::istreambuf_iterator<char>());

		lastWriteTimesJSON = JSON::parse(lastWriteTimesContent);
	}

	for (UInt64 moduleIndex = 0; moduleIndex < taskList.size(); moduleIndex++)
	{
		auto &module = taskList[moduleIndex];

		if (lastWriteTimesJSON.find(module.first.name) == lastWriteTimesJSON.end())
		{
			AddModuleToLastWriteJSON(moduleIndex, lastWriteTimesJSON);
			continue;
		}

		JSON &moduleJSON = lastWriteTimesJSON[module.first.name];

		for (UInt64 unitIndex = 0; unitIndex < module.second.size(); unitIndex++)
		{
			auto &unit = module.second[unitIndex];

			std::filesystem::file_time_type lastWrite = std::filesystem::last_write_time(module.first.canonicalPath + "/" + unit.name);

			if (moduleJSON.find(unit.name) == moduleJSON.end())
			{
				unit.build = true;
				moduleJSON[unit.name] = lastWrite.time_since_epoch().count();

				continue;
			}

			unit.build = (lastWrite.time_since_epoch().count() != (int64_t)moduleJSON[unit.name]);

			if (unit.build)
			{
				moduleJSON[unit.name] = lastWrite.time_since_epoch().count();
			}
		}
	}

	std::ofstream lastWriteTimesStream(lastWriteTimesFile);
	lastWriteTimesStream << lastWriteTimesJSON;
}

void BuildContext::AddModuleToLastWriteJSON(UInt64 moduleIndex, JSON &target)
{
	auto &module = taskList[moduleIndex];

	JSON moduleJSON;
	for (UInt64 unitIndex = 0; unitIndex < module.second.size(); unitIndex++)
	{
		auto &unit = module.second[unitIndex];
		unit.build = true;

		std::filesystem::file_time_type lastWrite = std::filesystem::last_write_time(module.first.canonicalPath + "/" + unit.name);
		moduleJSON[unit.name] = lastWrite.time_since_epoch().count();
	}

	target[module.first.name] = moduleJSON;
}

void BuildContext::PropagateBuildFlag()
{
	// TODO: iterate over all build tasks in the build order and mark every tasks whose dependencies are marked.
	//       (use cache files to cache the dependencies and avoid reparsing every file)
}

void BuildContext::ReduceTasks()
{
	// TODO: remove every task that isn't marked
}
