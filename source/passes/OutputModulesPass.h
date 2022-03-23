
#ifndef SOURCE_PASSES_OUTPUTMODULESPASS
#define SOURCE_PASSES_OUTPUTMODULESPASS

#include "Pass.h"

class OutputModulesPass : public Pass
{
public:
	OutputModulesPass();

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);
};

#endif
