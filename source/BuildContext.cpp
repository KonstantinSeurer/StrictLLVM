
#include "BuildContext.h"
#include "Time.h"

#include <iostream>
#include <filesystem>
#include <fstream>

bool operator==(const UnitTask &a, const UnitTask &b)
{
	return a.name == b.name;
}

bool operator==(const ModuleTask &a, const ModuleTask &b)
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
	if (moduleSet.find(name) != moduleSet.end())
	{
		return;
	}

	moduleSet.insert(name);

	const String modulePath = ResolveModulePath(name);
	const String moduleFile = modulePath + "/module.json";

	std::ifstream moduleFileStream(moduleFile);
	const String moduleFileContent = String(std::istreambuf_iterator<char>(moduleFileStream), std::istreambuf_iterator<char>());

	const JSON moduleJSON = JSON::parse(moduleFileContent);

	ModuleTask moduleTask(name, modulePath);
	if (moduleJSON.contains("requires"))
	{
		for (const auto &requireJSON : moduleJSON["requires"])
		{
			AddModule(String(requireJSON));
			moduleTask.dependencyIndices.push_back(taskList.size() - 1);
		}
	}

	taskList.push_back(Pair<ModuleTask, Array<UnitTask>>(moduleTask, Array<UnitTask>()));

	if (moduleJSON.contains("targets"))
	{
		AddTargetUnits(moduleTask, moduleJSON["targets"]);
	}
}

void BuildContext::AddTargetUnits(const ModuleTask &moduleName, const JSON &moduleTargetsJSON)
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
			taskList[taskList.size() - 1].second.push_back(UnitTask(String(exportJSON)));
		}
	}
}

void BuildContext::Build()
{
	std::cout << "Generating rebuild graph...";
	Time graphGenStart;

	bool build = false;
	MarkChangedUnits(build);

	if (build)
	{
		PropagateBuildFlag();
		ReduceTasks();
	}

	std::cout.precision(1);
	std::cout << " (" << (Time() - graphGenStart).milliSeconds() << "ms)" << std::endl;

	if (!build)
	{
		std::cout << "No build required!" << std::endl;
		return;
	}

	std::cout << "Build tasks:" << std::endl;

	for (const auto &module : taskList)
	{
		if (!module.first.build)
		{
			continue;
		}

		std::cout << "\t" << module.first.name << ":" << std::endl;

		for (const auto &unit : module.second)
		{
			if (!unit.build)
			{
				continue;
			}

			std::cout << "\t\t" << unit.name << std::endl;
		}
	}

	// TODO: compile every task and write dependency information and ir to cache files
}

void BuildContext::MarkChangedUnits(bool &build)
{
	const String lastWriteTimesFile = cachePath + "/lastWriteTimes.json";

	JSON lastWriteTimesJSON;
	if (std::filesystem::exists(lastWriteTimesFile))
	{
		std::ifstream lastWriteTimesStream(lastWriteTimesFile);
		const String lastWriteTimesContent = String(std::istreambuf_iterator<char>(lastWriteTimesStream), std::istreambuf_iterator<char>());

		lastWriteTimesJSON = JSON::parse(lastWriteTimesContent);
	}

	for (auto &module : taskList)
	{
		if (lastWriteTimesJSON.find(module.first.name) == lastWriteTimesJSON.end())
		{
			build = true;
			AddModuleToLastWriteJSON(module, lastWriteTimesJSON);
			continue;
		}

		JSON &moduleJSON = lastWriteTimesJSON[module.first.name];

		for (auto &unit : module.second)
		{
			std::filesystem::file_time_type lastWrite = std::filesystem::last_write_time(module.first.canonicalPath + "/" + unit.name);

			if (moduleJSON.find(unit.name) == moduleJSON.end() || (lastWrite.time_since_epoch().count() != (int64_t)moduleJSON[unit.name]))
			{
				build = true;
				unit.build = true;
				module.first.build = true;

				moduleJSON[unit.name] = lastWrite.time_since_epoch().count();

				continue;
			}
		}
	}

	std::ofstream lastWriteTimesStream(lastWriteTimesFile);
	lastWriteTimesStream << lastWriteTimesJSON;
}

void BuildContext::AddModuleToLastWriteJSON(Pair<ModuleTask, Array<UnitTask>> &module, JSON &target)
{
	module.first.build = true; // build new/uncached modules

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
	// propagate the ModuleTask build flag
	for (auto &module : taskList)
	{
		if (module.first.build)
		{
			continue;
		}

		for (UInt64 dependencyIndex : module.first.dependencyIndices)
		{
			if (taskList[dependencyIndex].first.build)
			{
				module.first.build = true;
				break;
			}
		}
	}

	for (auto &module : taskList)
	{
		if (!module.first.build)
		{
			continue;
		}
	}

	// TODO: iterate over all build tasks in the build order and mark every tasks whose dependencies are marked.
	//       (use cache files to cache the dependencies and avoid reparsing every file)
}

void BuildContext::ReduceTasks()
{
	// TODO: remove every task that isn't marked
}
