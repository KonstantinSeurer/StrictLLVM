
#include "LowerToIR.h"

IRContext::IRContext()
{
	context = Allocate<llvm::LLVMContext>();
	builder = Allocate<llvm::IRBuilder<>>(*context);
}

PassResultFlags IRContext::LowerToIR(Ref<ClassDeclaration> classDeclaration, const String& name)
{
	if (classDeclaration->typeTemplate)
	{
		return PassResultFlags::SUCCESS;
	}

	classDeclaration->classDeclarationMeta.module = Allocate<llvm::Module>(name, *context);

	classDeclaration->classDeclarationMeta.module->print(llvm::outs(), nullptr);

	return PassResultFlags::SUCCESS;
}

static IRContext irContext;

PassResultFlags LowerToIR(PrintFunction print, BuildContext& context)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto module : context.GetModules())
	{
		for (auto unit : module->units)
		{
			if (unit->declaredType->declarationType != UnitDeclarationType::CLASS)
			{
				continue;
			}

			result = result | irContext.LowerToIR(std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType), unit->name);
		}
	}

	return result;
}
