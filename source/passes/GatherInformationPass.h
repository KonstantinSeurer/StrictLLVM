#ifndef SOURCE_PASSES_GATHERPRERESOLVEMETA
#define SOURCE_PASSES_GATHERPRERESOLVEMETA

#include "../BuildContext.h"

class GatherInformationPass : public Pass
{
public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);
};

#endif /* SOURCE_PASSES_GATHERPRERESOLVEMETA */
