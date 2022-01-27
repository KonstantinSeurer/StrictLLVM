
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
	void LowerDataType(Ref<llvm::Module> module, Ref<DataType> type);

	void LowerMethod(Ref<llvm::Module> module, Ref<MethodDeclaration> method);

	void LowerClass(Ref<ClassDeclaration> classDeclaration);
};

#endif /* SOURCE_PASSES_LOWERTOIR */
