
#include "BuildContext.h"
#include "Time.h"
#include "ast/Parser.h"

#include <iostream>
#include <filesystem>

// TODO: What happend when source files get deleted -> add cache cleanup pass for deleted/renamed files

void UnitTask::InitFileName()
{
	fileName = name;
	std::replace(fileName.begin(), fileName.end(), '.', '/');
	fileName += ".strict";
}

bool operator==(const UnitTask &a, const UnitTask &b)
{
	return a.name == b.name;
}

bool operator==(const ModuleTask &a, const ModuleTask &b)
{
	return a.name == b.name;
}

BuildContext::BuildContext(const Array<String> &modulePath, const String &outputPath, const String &cachePath, const Optional<String> &logFile, TargetFlags target)
	: modulePath(modulePath), outputPath(outputPath), cachePath(cachePath), target(target), errorCount(0)
{
	if (!std::filesystem::exists(outputPath))
	{
		std::filesystem::create_directory(outputPath);
	}

	if (!std::filesystem::exists(cachePath))
	{
		std::filesystem::create_directory(cachePath);
	}

	logToFile = logFile.has_value();
	if (logToFile)
	{
		logFileOutput = std::ofstream(*logFile);
	}
}

void BuildContext::Print(const String &string, bool console)
{
	if (console)
	{
		std::cout << string;
	}

	if (logToFile)
	{
		logFileOutput << string;
	}
}

#define PRINT_FUNCTION [this](const String &string) { Print(string); }

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

Pair<String, String> BuildContext::ResolveUnitIdentifier(const String &identifier) const
{
	for (const auto &module : taskList)
	{
		if (identifier.find(module.first.name) == 0)
		{
			return Pair<String, String>(module.first.name, identifier.substr(module.first.name.length() + 1));
		}
	}

	std::cerr << "Could not resolve unit '" << identifier << "'!" << std::endl;
	return Pair<String, String>();
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

	if (moduleJSON.contains("units"))
	{
		const JSON unitsJSON = moduleJSON["units"];
		for (const auto &unitJSON : unitsJSON)
		{
			taskList[taskList.size() - 1].second.push_back(UnitTask(String(unitJSON)));
		}
	}
}

void BuildContext::Build()
{
	Print("Generating rebuild graph...\n");
	Time graphGenStart;

	bool build = false;
	MarkChangedUnits(build);

	if (build)
	{
		PropagateBuildFlag();
		ReduceTasks();
	}

	Print("(" + std::to_string((Time() - graphGenStart).milliSeconds()) + "ms)\n");

	if (!build)
	{
		Print("No build required!\n");
		return;
	}

	Print("Build tasks:\n");

	for (const auto &module : taskList)
	{
		Print("\t" + module.first.name + ":\n");

		for (const auto &unit : module.second)
		{
			Print("\t\t" + unit.name + "\n");
		}
	}

	for (const auto &pass : passes)
	{
		if ((pass(PRINT_FUNCTION, *this) & PassResultFlags::CRITICAL_ERROR) == PassResultFlags::CRITICAL_ERROR)
		{
			break;
		}
	}

	Print("Errors: " + std::to_string(errorCount) + "\n");

	// TODO: compile every task and write ir to cache files
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
			std::filesystem::file_time_type lastWrite = std::filesystem::last_write_time(module.first.canonicalPath + "/" + unit.fileName);

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

		std::filesystem::file_time_type lastWrite = std::filesystem::last_write_time(module.first.canonicalPath + "/" + unit.fileName);
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

		const String moduleCachePath = cachePath + "/" + module.first.name;

		if (!std::filesystem::exists(moduleCachePath))
		{
			std::filesystem::create_directory(moduleCachePath);
		}

		for (auto &unit : module.second)
		{
			const String unitCachePath = moduleCachePath + "/" + unit.name + ".json";

			if (unit.build)
			{
				const String unitSourcePath = module.first.canonicalPath + "/" + unit.fileName;

				std::ifstream unitSourceStream(unitSourcePath);
				const String unitSource = String(std::istreambuf_iterator<char>(unitSourceStream), std::istreambuf_iterator<char>());

				Ref<Lexer> lexer = Lexer::Create(unitSource);
				lexerCache[unitSourcePath] = lexer;

				ErrorStream err(unit.fileName, PRINT_FUNCTION, lexer);

				unit.unit = ParseUnit(err, *lexer, unit.name);

				if (err.HasErrorOccured())
				{
					errorCount += err.GetErrorCount();
					return;
				}

				Print(unit.unit->ToString(0) + "\n", false);

				JSON unitJSON = unit.unit->GetStructureJSON();

				std::ofstream unitCacheStream(unitCachePath);
				unitCacheStream << unitJSON;
			}
			else
			{
				std::ifstream unitCacheStream(unitCachePath);
				const String unitCacheContent = String(std::istreambuf_iterator<char>(unitCacheStream), std::istreambuf_iterator<char>());

				unit.unit = Allocate<Unit>(JSON::parse(unitCacheContent));

				for (const auto &dependencyName : unit.unit->dependencyNames)
				{
					const auto moduleAndUnit = ResolveUnitIdentifier(dependencyName);
					const UInt64 moduleIndex = FindModule(moduleAndUnit.first);
					const UInt64 unitIndex = FindUnit(moduleIndex, moduleAndUnit.second);

					unit.build |= taskList[moduleIndex].second[unitIndex].build;
				}

				if (unit.build)
				{
					const String unitSourcePath = module.first.canonicalPath + "/" + unit.fileName;

					std::ifstream unitSourceStream(unitSourcePath);
					const String unitSource = String(std::istreambuf_iterator<char>(unitSourceStream), std::istreambuf_iterator<char>());

					Ref<Lexer> lexer = Lexer::Create(unitSource);
					lexerCache[unitSourcePath] = lexer;

					ErrorStream err(unit.fileName, PRINT_FUNCTION, lexer);

					unit.unit = ParseUnit(err, *lexer, unit.name);

					if (err.HasErrorOccured())
					{
						errorCount += err.GetErrorCount();
						return;
					}
				}
			}
		}
	}
}

void BuildContext::ReduceTasks()
{
	for (Int64 moduleIndex = taskList.size() - 1; moduleIndex >= 0; moduleIndex--)
	{
		if (taskList[moduleIndex].first.build)
		{
			continue;
		}

		taskList.erase(taskList.begin() + moduleIndex);
	}

	for (auto &module : taskList)
	{
		for (Int64 unitIndex = module.second.size() - 1; unitIndex >= 0; unitIndex--)
		{
			if (module.second[unitIndex].build)
			{
				continue;
			}

			module.second.erase(module.second.begin() + unitIndex);
		}
	}
}

UInt64 BuildContext::FindModule(const String &name)
{
	for (UInt64 moduleIndex = 0; moduleIndex < taskList.size(); moduleIndex++)
	{
		if (taskList[moduleIndex].first.name == name)
		{
			return moduleIndex;
		}
	}

	std::cerr << "Could not find module '" << name << "'!" << std::endl;
	return 0;
}

UInt64 BuildContext::FindUnit(UInt64 moduleIndex, const String &name)
{
	const auto &units = taskList[moduleIndex].second;

	for (UInt64 unitIndex = 0; unitIndex < units.size(); unitIndex++)
	{
		if (units[unitIndex].name == name)
		{
			return unitIndex;
		}
	}

	std::cerr << "Could not find unit '" << name << "' in module '" << taskList[moduleIndex].first.name << "'!" << std::endl;
	return 0;
}
