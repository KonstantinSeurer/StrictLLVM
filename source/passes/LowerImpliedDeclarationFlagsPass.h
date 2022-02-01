#ifndef SOURCE_PASSES_LOWERIMPLIEDDECLARATIONFLAGS
#define SOURCE_PASSES_LOWERIMPLIEDDECLARATIONFLAGS

#include "../BuildContext.h"

class LowerImpliedDeclarationFlagsPass : public Pass
{
public:
	LowerImpliedDeclarationFlagsPass() : Pass("LowerImpliedDeclarationFlagsPass")
	{
	}

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);
};

#endif /* SOURCE_PASSES_LOWERIMPLIEDDECLARATIONFLAGS */
