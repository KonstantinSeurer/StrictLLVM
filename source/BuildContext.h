#ifndef SOURCE_BUILDCONTEXT
#define SOURCE_BUILDCONTEXT

#include "Base.h"
#include "ast/Module.h"
#include "Lexer.h"

class BuildContext
{
private:
	Array<String> modulePath;

	HashMap<String, Ref<TokenStream>> lexerCache;

	HashMap<String, HashSet<String>> taskSet;
	Array<Pair<String, Array<String>>> taskList;

	Array<Ref<Module>> modules;

	TargetFlags target;

public:
	BuildContext(const Array<String> &modulePath, TargetFlags target)
		: modulePath(modulePath), target(target)
	{
	}

	String ResolveModulePath(const String &moduleName) const;
	void AddModule(const String &moduleName);
	void Build();

private:
	void AddRequiredModules(const JSON &moduleRequiresJSON);
	void AddTargetUnits(const String &moduleName, const JSON &moduleTargetsJSON);
};

#endif /* SOURCE_BUILDCONTEXT */
