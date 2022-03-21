
#ifndef SOURCE_PASSES_LINKMODULES
#define SOURCE_PASSES_LINKMODULES

#include "../ast/AST.h"
#include "Pass.h"

class LinkModulesPass : public Pass
{
public:
	LinkModulesPass();

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);

private:
	PassResultFlags LinkDependency(Ref<Module> module, Ref<Module> dependency);
};

#endif
