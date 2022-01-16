#ifndef SOURCE_PASSES_INLINETEMPLATES
#define SOURCE_PASSES_INLINETEMPLATES

#include "../BuildContext.h"

class InlineTemplatesPass : public Pass
{
public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);
};

#endif /* SOURCE_PASSES_INLINETEMPLATES */
