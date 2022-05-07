#ifndef SOURCE_BUILDCONTEXT
#define SOURCE_BUILDCONTEXT

#include "Base.h"
#include "ErrorStream.h"
#include "Lexer.h"
#include "ast/AST.h"
#include "passes/Pass.h"

#include <fstream>

STRICT_ENUM(OptimizationLevel, DEBUGGING, PERFORMANCE, SIZE);

struct UnitTask
{
public:
	String name;
	String baseFileName;
	bool build;
	Ref<Unit> unit;

public:
	UnitTask(const String& name) : name(name), build(false)
	{
		InitFileName();
	}

	UnitTask(const String& name, bool build) : name(name), build(build)
	{
		InitFileName();
	}

private:
	void InitFileName();
};

struct ModuleTask
{
public:
	ModuleType type = ModuleType::INLINE;
	String name;
	String canonicalPath;
	Array<UInt64> dependencyIndices;
	Ref<Module> module;
	bool build;

public:
	ModuleTask(const String& name) : name(name), build(false)
	{
	}

	ModuleTask(const String& name, const String& canonicalPath) : name(name), canonicalPath(canonicalPath), build(false)
	{
	}

	ModuleTask(const String& name, const String& canonicalPath, bool build) : name(name), canonicalPath(canonicalPath), build(build)
	{
	}
};

bool operator==(const UnitTask& a, const UnitTask& b);

bool operator==(const ModuleTask& a, const ModuleTask& b);

namespace std
{
template <> struct hash<UnitTask>
{
	size_t operator()(const UnitTask& task) const
	{
		hash<String> hashFunction;
		return hashFunction(task.name);
	}
};

template <> struct hash<ModuleTask>
{
	size_t operator()(const ModuleTask& name) const
	{
		hash<String> hashFunction;
		return hashFunction(name.name);
	}
};
} // namespace std

class BuildContext
{
private:
	Array<String> modulePath;
	String outputPath;
	String cachePath;
	TargetFlags target;

	OptimizationLevel optimizationLevel;

	HashMap<String, Ref<Lexer>> lexerCache;

	HashSet<String> moduleSet;
	Array<Pair<ModuleTask, Array<UnitTask>>> taskList;

	Array<Ref<Module>> modules;

	Array<Ref<Pass>> passes;

	UInt32 errorCount;

	bool logToFile;
	std::ofstream logFileOutput;

public:
	const bool dumpIR;

public:
	BuildContext(const Array<String>& modulePath, const String& outputPath, const String& cachePath, const Optional<String>& logFile, TargetFlags target,
	             bool dumpIR, OptimizationLevel optimizationLevel);

	String ResolveModulePath(const String& moduleName) const;
	Pair<String, String> ResolveUnitIdentifier(const String& identifier) const;
	Ref<Unit> ResolveUnit(const String& identifier) const;

	UInt32 AddModule(const String& moduleName);
	void Build();

	void AddPass(Ref<Pass> pass)
	{
		passes.push_back(pass);
	}

	const Array<Ref<Module>>& GetModules() const
	{
		return modules;
	}

	Array<Ref<Module>>& GetModules()
	{
		return modules;
	}

	void InvokeCompiler(const String inputFile, const String& outputFile);
	void InvokeLinker(const HashSet<String>& inputFiles, const String& outputFile);

private:
	void Print(const String& string, bool console = true);

	void MarkChangedUnits(bool& build);
	void AddModuleToLastWriteJSON(Pair<ModuleTask, Array<UnitTask>>& module, JSON& target);

	void PropagateBuildFlagAndParse();

	void CompileUnit(const ModuleTask& module, UnitTask& unit);

	UInt64 FindModule(const String& name) const;
	UInt64 FindUnit(UInt64 moduleIndex, const String& name) const;
};

#endif /* SOURCE_BUILDCONTEXT */
