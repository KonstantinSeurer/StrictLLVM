
#ifndef SOURCE_PASSES_LOWERTOIR
#define SOURCE_PASSES_LOWERTOIR

#include "../BuildContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"

class LowerToIRPass : public Pass
{
private:
	Ref<llvm::IRBuilder<>> builder;
	Ref<llvm::LLVMContext> context;

public:
	LowerToIRPass();

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);

private:
	PassResultFlags LowerToIR(Ref<ClassDeclaration> classDeclaration, const String& namet);
};

#endif /* SOURCE_PASSES_LOWERTOIR */
