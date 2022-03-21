
#include "LinkModules.h"
#include "../BuildContext.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Transforms/Utils/Cloning.h"

LinkModulesPass::LinkModulesPass() : Pass("LinkModulesPass")
{
}

PassResultFlags LinkModulesPass::LinkDependency(Ref<Module> module, Ref<Module> dependency)
{
	llvm::Linker linker(*module->moduleMeta.module);

	if (linker.linkInModule(llvm::CloneModule(*dependency->moduleMeta.module), llvm::Linker::Flags::OverrideFromSrc))
	{
		return PassResultFlags::CRITICAL_ERROR;
	}

	for (auto subDependency : dependency->dependencies)
	{
		if (LinkDependency(module, subDependency) != PassResultFlags::SUCCESS)
		{
			return PassResultFlags::CRITICAL_ERROR;
		}
	}

	return PassResultFlags::SUCCESS;
}

PassResultFlags LinkModulesPass::Run(PrintFunction print, BuildContext& context)
{
	for (auto module : context.GetModules())
	{
		llvm::Linker linker(*module->moduleMeta.module);

		for (auto& specialization : module->moduleMeta.templateSpecializations)
		{
			if (linker.linkInModule(std::move(specialization.second->classDeclarationMeta.module), llvm::Linker::Flags::OverrideFromSrc))
			{
				return PassResultFlags::CRITICAL_ERROR;
			}
		}

		for (auto unit : module->units)
		{
			if (unit->declaredType->declarationType != UnitDeclarationType::CLASS)
			{
				continue;
			}

			Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType);
			if (classDeclaration->typeTemplate)
			{
				continue;
			}

			if (linker.linkInModule(std::move(classDeclaration->classDeclarationMeta.module), llvm::Linker::Flags::OverrideFromSrc))
			{
				return PassResultFlags::CRITICAL_ERROR;
			}
		}
	}

	for (auto module : context.GetModules())
	{
		for (auto dependency : module->dependencies)
		{
			if (LinkDependency(module, dependency) != PassResultFlags::SUCCESS)
			{
				return PassResultFlags::CRITICAL_ERROR;
			}
		}

		if (context.dumpIR)
		{
			std::error_code error;
			llvm::raw_fd_ostream dumpIRStream(module->moduleMeta.outputPath + "/output.ll", error);
			module->moduleMeta.module->print(dumpIRStream, nullptr);
		}
	}

	return PassResultFlags::SUCCESS;
}
