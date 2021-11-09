#ifndef SOURCE_BUILDCONTEXT
#define SOURCE_BUILDCONTEXT

#include "Base.h"
#include "ast/Module.h"
#include "Lexer.h"

struct UnitTask
{
public:
	String name;
	String fileName;
	bool build;

public:
	UnitTask(const String &name)
		: name(name), build(false)
	{
		InitFileName();
	}

	UnitTask(const String &name, bool build)
		: name(name), build(build)
	{
		InitFileName();
	}

private:
	void InitFileName();
};

struct ModuleTask
{
public:
	String name;
	String canonicalPath;
	Array<UInt64> dependencyIndices;
	bool build;

public:
	ModuleTask(const String &name)
		: name(name), build(false)
	{
	}

	ModuleTask(const String &name, const String &canonicalPath)
		: name(name), canonicalPath(canonicalPath), build(false)
	{
	}

	ModuleTask(const String &name, const String &canonicalPath, bool build)
		: name(name), canonicalPath(canonicalPath), build(build)
	{
	}
};

bool operator==(const UnitTask &a, const UnitTask &b);

bool operator==(const ModuleTask &a, const ModuleTask &b);

namespace std
{
	template <>
	struct hash<UnitTask>
	{
		size_t operator()(const UnitTask &task) const
		{
			hash<String> hashFunction;
			return hashFunction(task.name);
		}
	};

	template <>
	struct hash<ModuleTask>
	{
		size_t operator()(const ModuleTask &name) const
		{
			hash<String> hashFunction;
			return hashFunction(name.name);
		}
	};
}

class BuildContext
{
private:
	Array<String> modulePath;
	String cachePath;
	TargetFlags target;

	HashMap<String, Ref<TokenStream>> lexerCache;

	HashSet<String> moduleSet;
	Array<Pair<ModuleTask, Array<UnitTask>>> taskList;

	Array<Ref<Module>> modules;

public:
	BuildContext(const Array<String> &modulePath, const String &cachePath, TargetFlags target);

	String ResolveModulePath(const String &moduleName) const;
	Pair<String, String> ResolveUnitIdentifier(const String &identifier) const;

	void AddModule(const String &moduleName);
	void Build();

private:
	void AddTargetUnits(const ModuleTask &moduleName, const JSON &moduleTargetsJSON);

	void MarkChangedUnits(bool &build);
	void AddModuleToLastWriteJSON(Pair<ModuleTask, Array<UnitTask>> &module, JSON &target);

	void PropagateBuildFlag();
	void ParseDependencyInformation(TokenStream &lexer, JSON &target) const;

	void ReduceTasks();

	UInt64 FindModule(const String &name);
	UInt64 FindUnit(UInt64 moduleIndex, const String &name);
};

#endif /* SOURCE_BUILDCONTEXT */
