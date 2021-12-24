
#include "InlineTemplates.h"

void GenerateTemplateSpecializations(Ref<Module> module)
{
}

PassResultFlags InlineTemplates(PrintFunction print, BuildContext &context)
{
	for (auto module : context.GetModules())
	{
		GenerateTemplateSpecializations(module);
	}
	// TODO: Clone the unit declaration for every unique specialization
	//       Replace references to the template paramater with the template arguments for every specialization
	//       Replace references to the templated types with references to the inlined versions
	return PassResultFlags::SUCCESS;
}
