#ifndef SOURCE_PASSES_VALIDATESTRUCTURE
#define SOURCE_PASSES_VALIDATESTRUCTURE

#include "../BuildContext.h"

class ValidateStructurePass : public Pass
{
public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);
};

#endif /* SOURCE_PASSES_VALIDATESTRUCTURE */
