
#ifndef SOURCE_PASSES_LOWERTOIR
#define SOURCE_PASSES_LOWERTOIR

#include "../BuildContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"

class LowerFunctionToIRState
{
public:
	MethodDeclaration* method;
	llvm::BasicBlock* currentBlock;
	llvm::BasicBlock* breakBlock;
	llvm::BasicBlock* continueBlock;
};

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

	void LowerExpression(Ref<llvm::Module> module, Ref<Expression> expression);

	void LowerDeleteStatement(Ref<llvm::Module> module, Ref<DeleteStatement> statement, LowerFunctionToIRState* state);

	void LowerForStatement(Ref<llvm::Module> module, Ref<ForStatement> statement, LowerFunctionToIRState* state);

	void LowerIfStatement(Ref<llvm::Module> module, Ref<IfStatement> statement, LowerFunctionToIRState* state);

	void LowerVariableDeclarationStatement(Ref<llvm::Module> module, Ref<VariableDeclarationStatement> statement, LowerFunctionToIRState* state);

	void LowerWhileStatement(Ref<llvm::Module> module, Ref<WhileStatement> statement, LowerFunctionToIRState* state);

	void LowerStatement(Ref<llvm::Module> module, Ref<Statement> statement, LowerFunctionToIRState* state);

	void LowerMethod(Ref<llvm::Module> module, Ref<MethodDeclaration> method);

	void LowerClass(Ref<ClassDeclaration> classDeclaration);
};

#endif /* SOURCE_PASSES_LOWERTOIR */
