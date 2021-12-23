
#include "ValidateStructure.h"

PassResultFlags ValidateStructure(PrintFunction print, BuildContext &context)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (const auto module : context.GetModules())
	{
		for (const auto unit : module->units)
		{
			// TODO: Catch methods with the same signature
			//       Catch variables with the same name
			//       Assert the immutability of members for immutable types
			//       Assert the immutability of non mutating operators and getters
		}
	}

	return result;
}
