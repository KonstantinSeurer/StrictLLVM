#ifndef SOURCE_PASSES_PASS
#define SOURCE_PASSES_PASS

#include "../ErrorStream.h"

STRICT_FLAGS(PassResultFlags, SUCCESS = 0, WARNING = 1, ERROR = 2, CRITICAL_ERROR = 4)

class BuildContext;

using BuildPass = Function<PassResultFlags(PrintFunction, BuildContext&)>;

#endif /* SOURCE_PASSES_PASS */
