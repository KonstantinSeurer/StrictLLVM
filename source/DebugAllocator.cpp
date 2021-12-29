
#include "DebugAllocator.h"

static UInt64 currentAllocationSize = 0;
static UInt64 maxAllocationSize = 0;

UInt64 GetCurrentAllocationSize()
{
	return currentAllocationSize;
}

UInt64 GetMaxAllocationSize()
{
	return maxAllocationSize;
}

#if USE_DEBUG_ALLOCATOR

#include <malloc.h>
#include <stdlib.h>

#ifdef __GLIBC__
#define ALLOC_SIZE(pointer) malloc_usable_size(pointer)
#else
#ifdef __WIN32__
#define ALLOC_SIZE(pointer) _msize(pointer)
#endif
#endif

void* operator new(size_t size)
{
	currentAllocationSize += size;
	if (currentAllocationSize > maxAllocationSize)
	{
		maxAllocationSize = currentAllocationSize;
	}

	return malloc(size);
}

void operator delete(void* p)
{
	currentAllocationSize -= ALLOC_SIZE(p);
	free(p);
}

#endif
