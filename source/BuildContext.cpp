
#include "BuildContext.h"
#include "Time.h"
#include "ast/Parser.h"

#include "passes/GatherInformationPass.h"
#include "passes/InlineTemplatesPass.h"
#include "passes/LinkModules.h"
#include "passes/LowerImplicitCastsPass.h"
#include "passes/LowerImpliedDeclarationFlagsPass.h"
#include "passes/LowerOperatorsPass.h"
#include "passes/LowerToIRPass.h"
#include "passes/OutputModulesPass.h"
#include "passes/ResolveIdentifiersPass.h"
#include "passes/ValidateStructurePass.h"

#include <filesystem>
#include <iostream>

// TODO: What happend when source files get deleted -> add cache cleanup pass for deleted/renamed
// files

void UnitTask::InitFileName()
{
	baseFileName = name;
	std::replace(baseFileName.begin(), baseFileName.end(), '.', '/');
}

bool operator==(const UnitTask& a, const UnitTask& b)
{
	return a.name == b.name;
}

bool operator==(const ModuleTask& a, const ModuleTask& b)
{
	return a.name == b.name;
}

BuildContext::BuildContext(const Array<String>& modulePath, const String& outputPath,
                           const String& cachePath, const Optional<String>& logFile,
                           TargetFlags target, bool dumpIR, OptimizationLevel optimizationLevel)
	: modulePath(modulePath), outputPath(outputPath), cachePath(cachePath), target(target),
	  errorCount(0), dumpIR(dumpIR), optimizationLevel(optimizationLevel)
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

	// Add required passes
	AddPass(Allocate<ValidateStructurePass>());
	AddPass(Allocate<GatherInformationPass>(
		GatherInformationFlags::PARENT | GatherInformationFlags::THIS |
		GatherInformationFlags::MEMBER_INDEX | GatherInformationFlags::METHOD_NAME));
	AddPass(Allocate<ResolveIdentifiersPass>());
	AddPass(Allocate<InlineTemplatesPass>());
	AddPass(Allocate<LowerImpliedDeclarationFlagsPass>());
	AddPass(Allocate<ResolveIdentifiersPass>());
	AddPass(Allocate<LowerOperatorsPass>());
	AddPass(Allocate<LowerImplicitCastsPass>());
	AddPass(Allocate<LowerToIRPass>());
	AddPass(Allocate<LinkModulesPass>());
	AddPass(Allocate<OutputModulesPass>());
}

void BuildContext::Print(const String& string, bool console)
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

#define PRINT_FUNCTION [this](const String& string) { Print(string); }

String BuildContext::ResolveModulePath(const String& moduleName) const
{
	String moduleNameCopy = moduleName;
	std::replace(moduleNameCopy.begin(), moduleNameCopy.end(), '.', '/');
	const String moduleSubDirectory = String("/") + moduleNameCopy;
	const String moduleFile = moduleSubDirectory + "/module.json";

	for (const String& path : modulePath)
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

Pair<String, String> BuildContext::ResolveUnitIdentifier(const String& identifier) const
{
	for (const auto& module : taskList)
	{
		if (identifier.find(module.first.name) == 0)
		{
			return Pair<String, String>(module.first.name,
			                            identifier.substr(module.first.name.length() + 1));
		}
	}

	std::cerr << "Could not resolve unit '" << identifier << "'!" << std::endl;
	return Pair<String, String>();
}

Ref<Unit> BuildContext::ResolveUnit(const String& identifier) const
{
	const auto moduleAndUnit = ResolveUnitIdentifier(identifier);
	const UInt64 moduleIndex = FindModule(moduleAndUnit.first);
	const UInt64 unitIndex = FindUnit(moduleIndex, moduleAndUnit.second);
	return taskList[moduleIndex].second[unitIndex].unit;
}

UInt32 BuildContext::AddModule(const String& name)
{
	if (moduleSet.find(name) != moduleSet.end())
	{
		for (UInt32 taskIndex = 0; taskIndex < taskList.size(); taskIndex++)
		{
			if (taskList[taskIndex].first.name == name)
			{
				return taskIndex;
			}
		}
		STRICT_UNREACHABLE;
	}

	moduleSet.insert(name);

	const String modulePath = ResolveModulePath(name);
	const String moduleFile = modulePath + "/module.json";

	std::ifstream moduleFileStream(moduleFile);
	const String moduleFileContent =
		String(std::istreambuf_iterator<char>(moduleFileStream), std::istreambuf_iterator<char>());

	const JSON moduleJSON = JSON::parse(moduleFileContent);

	ModuleTask moduleTask(name, modulePath);
	if (moduleJSON.contains("requires"))
	{
		for (const auto& requireJSON : moduleJSON["requires"])
		{
			moduleTask.dependencyIndices.push_back(AddModule(String(requireJSON)));
		}
	}

	if (moduleJSON.contains("type"))
	{
		moduleTask.type = StringToModuleType(moduleJSON["type"]);
	}

	taskList.push_back(Pair<ModuleTask, Array<UnitTask>>(moduleTask, Array<UnitTask>()));

	if (moduleJSON.contains("units"))
	{
		const JSON unitsJSON = moduleJSON["units"];
		for (const auto& unitJSON : unitsJSON)
		{
			taskList[taskList.size() - 1].second.push_back(UnitTask(String(unitJSON)));
		}
	}

	return taskList.size() - 1;
}

void BuildContext::Build()
{
	Print("Scanning units... ");
	Time scanStart;

	bool build = false;
	MarkChangedUnits(build);

	Print("(" + std::to_string((Time() - scanStart).milliSeconds()) + "ms)\n");

	if (!build)
	{
		Print("No build required!\n");
		return;
	}

	Print("Parsing...\n");
	Time parseStart;

	PropagateBuildFlagAndParse();

	Print("(" + std::to_string((Time() - parseStart).milliSeconds()) + "ms)\n");
	Print("Errors: " + std::to_string(errorCount) + "\n");

	Print("Compiling...\n");
	Time compileStart;

	for (const auto& pass : passes)
	{
		if ((pass->Run(PRINT_FUNCTION, *this) & PassResultFlags::CRITICAL_ERROR) ==
		    PassResultFlags::CRITICAL_ERROR)
		{
			Print(pass->name + " failed!\n");
			break;
		}
	}

	Print("(" + std::to_string((Time() - compileStart).milliSeconds()) + "ms)\n");

	for (auto& module : taskList)
	{
		for (auto& unit : module.second)
		{
			if (!unit.build)
			{
				continue;
			}

			Print(unit.unit->ToString(0) + "\n", false);
		}
	}

	// TODO: compile every task and write ir to cache files
}

void BuildContext::MarkChangedUnits(bool& build)
{
	const String lastWriteTimesFile = cachePath + "/lastWriteTimes.json";

	JSON lastWriteTimesJSON;
	if (std::filesystem::exists(lastWriteTimesFile))
	{
		std::ifstream lastWriteTimesStream(lastWriteTimesFile);
		const String lastWriteTimesContent = String(
			std::istreambuf_iterator<char>(lastWriteTimesStream), std::istreambuf_iterator<char>());

		lastWriteTimesJSON = JSON::parse(lastWriteTimesContent);
	}

	for (auto& module : taskList)
	{
		if (lastWriteTimesJSON.find(module.first.name) == lastWriteTimesJSON.end())
		{
			build = true;
			AddModuleToLastWriteJSON(module, lastWriteTimesJSON);
			continue;
		}

		JSON& moduleJSON = lastWriteTimesJSON[module.first.name];

		for (auto& unit : module.second)
		{
			std::filesystem::file_time_type lastWrite = std::filesystem::last_write_time(
				module.first.canonicalPath + "/" + unit.baseFileName + ".strict");

			if (moduleJSON.find(unit.name) == moduleJSON.end() ||
			    (lastWrite.time_since_epoch().count() != (int64_t)moduleJSON[unit.name]))
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

void BuildContext::AddModuleToLastWriteJSON(Pair<ModuleTask, Array<UnitTask>>& module, JSON& target)
{
	module.first.build = true; // build new/uncached modules

	JSON moduleJSON;
	for (UInt64 unitIndex = 0; unitIndex < module.second.size(); unitIndex++)
	{
		auto& unit = module.second[unitIndex];
		unit.build = true;

		std::filesystem::file_time_type lastWrite = std::filesystem::last_write_time(
			module.first.canonicalPath + "/" + unit.baseFileName + ".strict");
		moduleJSON[unit.name] = lastWrite.time_since_epoch().count();
	}

	target[module.first.name] = moduleJSON;
}

void BuildContext::PropagateBuildFlagAndParse()
{
	// propagate the ModuleTask build flag
	for (auto& module : taskList)
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

	// Create module AST items for module tasks
	for (auto& module : taskList)
	{
		if (!module.first.build)
		{
			continue;
		}

		String moduleOutputPath = outputPath + "/" + module.first.name;
		if (!std::filesystem::exists(moduleOutputPath))
		{
			std::filesystem::create_directory(moduleOutputPath);
		}

		module.first.module = Allocate<Module>(module.first.type, module.first.name);
		module.first.module->moduleMeta.outputPath = moduleOutputPath;
		module.first.module->moduleMeta.path = module.first.canonicalPath;
		modules.push_back(module.first.module);
	}

	// Resolve module dependencies
	for (auto& module : taskList)
	{
		if (!module.first.build)
		{
			continue;
		}

		for (UInt64 dependencyIndex : module.first.dependencyIndices)
		{
			module.first.module->dependencies.push_back(taskList[dependencyIndex].first.module);
		}
	}

	for (auto& module : taskList)
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

		for (auto& unit : module.second)
		{
			const String unitCachePath = moduleCachePath + "/" + unit.name + ".json";

			if (unit.build)
			{
				CompileUnit(module.first, unit);

				JSON unitJSON = unit.unit->GetStructureJSON();

				std::ofstream unitCacheStream(unitCachePath);
				unitCacheStream << unitJSON;
			}
			else
			{
				std::ifstream unitCacheStream(unitCachePath);
				const String unitCacheContent =
					String(std::istreambuf_iterator<char>(unitCacheStream),
				           std::istreambuf_iterator<char>());

				unit.unit = Allocate<Unit>(JSON::parse(unitCacheContent));

				for (const auto& dependencyName : unit.unit->dependencyNames)
				{
					const auto moduleAndUnit = ResolveUnitIdentifier(dependencyName);
					const UInt64 moduleIndex = FindModule(moduleAndUnit.first);
					const UInt64 unitIndex = FindUnit(moduleIndex, moduleAndUnit.second);

					unit.build |= taskList[moduleIndex].second[unitIndex].build;
				}

				if (unit.build)
				{
					CompileUnit(module.first, unit);
				}
			}

			module.first.module->units.push_back(unit.unit);
		}
	}
}

void BuildContext::CompileUnit(const ModuleTask& module, UnitTask& unit)
{
	const String unitBasePath = module.canonicalPath + "/" + unit.baseFileName;

	std::ifstream unitSourceStream(unitBasePath + ".strict");
	const String unitSource =
		String(std::istreambuf_iterator<char>(unitSourceStream), std::istreambuf_iterator<char>());

	Ref<Lexer> lexer = Lexer::Create(unitSource);
	lexerCache[unitBasePath] = lexer;

	ErrorStream err(unit.baseFileName + ".strict", PRINT_FUNCTION, lexer.get());

	unit.unit = ParseUnit(err, *lexer, unit.name);

	if (err.HasErrorOccured())
	{
		errorCount += err.GetErrorCount();
		return;
	}

	const String unitCSourcePath = unitBasePath + ".c";
	if (std::filesystem::exists(unitCSourcePath))
	{
		const String unitCOutputPath =
			module.module->moduleMeta.outputPath + "/" + unit.baseFileName + ".o";
		InvokeCompiler(unitCSourcePath, unitCOutputPath);

		unit.unit->unitMeta.hasExternals = true;
	}
}

UInt64 BuildContext::FindModule(const String& name) const
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

UInt64 BuildContext::FindUnit(UInt64 moduleIndex, const String& name) const
{
	const auto& units = taskList[moduleIndex].second;

	for (UInt64 unitIndex = 0; unitIndex < units.size(); unitIndex++)
	{
		if (units[unitIndex].name == name)
		{
			return unitIndex;
		}
	}

	std::cerr << "Could not find unit '" << name << "' in module '"
			  << taskList[moduleIndex].first.name << "'!" << std::endl;
	return 0;
}

void BuildContext::InvokeCompiler(const String inputFile, const String& outputFile)
{
	String command = "clang -c " + inputFile + " -o " + outputFile;

	int exitCode = system(command.c_str());

	if (exitCode)
	{
		std::cerr << "Executing " << command << " returned " << exitCode << "!" << std::endl;
	}
}

void BuildContext::InvokeLinker(const HashSet<String>& inputFiles, const String& outputFile)
{
	String command = "clang  -o " + outputFile;
	for (const String& inputFile : inputFiles)
	{
		command += " " + inputFile;
	}

	int exitCode = system(command.c_str());

	if (exitCode)
	{
		std::cerr << "Executing " << command << " returned " << exitCode << "!" << std::endl;
	}
}
