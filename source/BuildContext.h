#ifndef SOURCE_BUILDCONTEXT
#define SOURCE_BUILDCONTEXT

#include "Base.h"
#include "ast/Module.h"
#include "Lexer.h"

struct BuildTask
{
public:
	String name;
	bool build;

public:
	BuildTask(const String &name, bool build)
		: name(name), build(build)
	{
	}
};

struct ModuleName
{
public:
	String name;
	String canonicalPath;

public:
	ModuleName(const String &name)
		: name(name)
	{
	}

	ModuleName(const String &name, const String &canonicalPath)
		: name(name), canonicalPath(canonicalPath)
	{
	}
};

bool operator==(const BuildTask &a, const BuildTask &b);

bool operator==(const ModuleName &a, const ModuleName &b);

namespace std
{
	template <>
	struct hash<BuildTask>
	{
		size_t operator()(const BuildTask &task) const
		{
			hash<String> hashFunction;
			return hashFunction(task.name);
		}
	};

	template <>
	struct hash<ModuleName>
	{
		size_t operator()(const ModuleName &name) const
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

	HashMap<ModuleName, HashSet<BuildTask>> taskSet;
	Array<Pair<ModuleName, Array<BuildTask>>> taskList;

	Array<Ref<Module>> modules;

public:
	BuildContext(const Array<String> &modulePath, const String &cachePath, TargetFlags target);

	String ResolveModulePath(const String &moduleName) const;
	void AddModule(const String &moduleName);
	void Build();

private:
	void AddRequiredModules(const JSON &moduleRequiresJSON);

	void AddTargetUnits(const ModuleName &moduleName, const JSON &moduleTargetsJSON);
	void AddTargetUnit(const ModuleName &moduleName, const String &sourceFile);

	void MarkChangedUnits();
	void AddModuleToLastWriteJSON(UInt64 moduleIndex, JSON &target);

	void PropagateBuildFlag();

	void ReduceTasks();
};

#endif /* SOURCE_BUILDCONTEXT */
