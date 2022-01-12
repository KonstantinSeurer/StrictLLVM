
#ifndef SOURCE_PASSES_LOWERTOIR
#define SOURCE_PASSES_LOWERTOIR

#include "../BuildContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"

class IRContext
{
private:
	Ref<llvm::IRBuilder<>> builder;
	Ref<llvm::LLVMContext> context;

public:
	IRContext();

	PassResultFlags LowerToIR(Ref<ClassDeclaration> classDeclaration, const String& namet);
};

PassResultFlags LowerToIR(PrintFunction print, BuildContext& context);

#endif /* SOURCE_PASSES_LOWERTOIR */
