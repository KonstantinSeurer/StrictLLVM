#ifndef SOURCE_DEBUGALLOCATOR
#define SOURCE_DEBUGALLOCATOR

#include "Base.h"

#ifdef DEBUG
#define USE_DEBUG_ALLOCATOR 1
#else
#define USE_DEBUG_ALLOCATOR 0
#endif

UInt64 GetCurrentAllocationSize();
UInt64 GetMaxAllocationSize();

#endif /* SOURCE_DEBUGALLOCATOR */
