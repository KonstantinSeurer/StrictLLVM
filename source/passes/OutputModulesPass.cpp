
#include "OutputModulesPass.h"
#include "../BuildContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

OutputModulesPass::OutputModulesPass() : Pass("OutputModulesPass")
{
}

PassResultFlags OutputModulesPass::Run(PrintFunction print, BuildContext& context)
{
	// TODO: Cross compiling
	String targetTriple = llvm::sys::getDefaultTargetTriple();

	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

	String error;
	auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
	if (!target)
	{
		llvm::errs() << error << "\n";
		return PassResultFlags::CRITICAL_ERROR;
	}

	llvm::TargetOptions opt;
	auto rm = llvm::Optional<llvm::Reloc::Model>();
	auto targetMachine = target->createTargetMachine(targetTriple, "generic", "", opt, rm);
	auto dataLayout = targetMachine->createDataLayout();

	for (auto module : context.GetModules())
	{
		if (module->moduleType == ModuleType::INLINE)
		{
			continue;
		}

		module->moduleMeta.module->setTargetTriple(targetTriple);
		module->moduleMeta.module->setDataLayout(dataLayout);

		std::error_code errorCode;
		llvm::raw_fd_ostream outputStream(module->moduleMeta.outputPath + "/output.o", errorCode);
		if (errorCode)
		{
			llvm::errs() << "Could not open file: " << errorCode.message() << "\n";
			return PassResultFlags::CRITICAL_ERROR;
		}

		llvm::legacy::PassManager passManager;
		if (targetMachine->addPassesToEmitFile(passManager, outputStream, nullptr, llvm::CGFT_ObjectFile))
		{
			llvm::errs() << "TargetMachine can't emit a file of this type!\n";
			return PassResultFlags::CRITICAL_ERROR;
		}

		passManager.run(*module->moduleMeta.module);
		outputStream.flush();
	}

	return PassResultFlags::SUCCESS;
}
