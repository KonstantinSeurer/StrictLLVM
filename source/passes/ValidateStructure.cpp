
#include "ValidateStructure.h"

PassResultFlags ValidateStructure(PrintFunction print, BuildContext &context)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (const auto &module : context.GetModules())
	{
		for (const auto &unit : module->units)
		{
		}
	}

	return result;
}
