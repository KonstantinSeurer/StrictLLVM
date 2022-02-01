#ifndef SOURCE_PASSES_PASS
#define SOURCE_PASSES_PASS

#include "../ErrorStream.h"

STRICT_FLAGS(PassResultFlags, SUCCESS = 0, WARNING = 1, ERROR = 2, CRITICAL_ERROR = 4)

class BuildContext;

class Pass
{
public:
	String name;

public:
	Pass(const String& name) : name(name)
	{
	}

	virtual ~Pass()
	{
	}

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context) = 0;
};

#endif /* SOURCE_PASSES_PASS */
