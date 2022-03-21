
#ifndef SOURCE_PASSES_LOWERTOIR
#define SOURCE_PASSES_LOWERTOIR

#include "../BuildContext.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"

class LowerFunctionToIRState
{
public:
	Ref<ClassDeclaration> classDeclaration;
	MethodDeclaration* method;
	llvm::BasicBlock* currentBlock;
	llvm::BasicBlock* breakBlock;
	llvm::BasicBlock* continueBlock;
	llvm::Value* thisPointer;
	llvm::Function* function;
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
	void LowerDataType(llvm::Module* module, Ref<DataType> type);

	void LowerCallExpression(llvm::Module* module, Ref<CallExpression> expression, LowerFunctionToIRState* state);

	void LowerIdentifierExpression(llvm::Module* module, Ref<IdentifierExpression> expression, LowerFunctionToIRState* state);

	void LowerLiteralExpression(llvm::Module* module, Ref<LiteralExpression> expression, LowerFunctionToIRState* state);

	void LowerNewExpression(llvm::Module* module, Ref<NewExpression> expression, LowerFunctionToIRState* state);

	llvm::Value* LowerIntOperator(OperatorType type, llvm::Value* a, llvm::Value* b, bool isSigned);

	llvm::Value* LowerFloatOperator(OperatorType type, llvm::Value* a, llvm::Value* b);

	void LowerOperatorExpression(llvm::Module* module, Ref<OperatorExpression> expression, LowerFunctionToIRState* state);

	void LowerTernaryExpression(llvm::Module* module, Ref<TernaryExpression> expression, LowerFunctionToIRState* state);

	void LowerExpression(llvm::Module* module, Ref<Expression> expression, LowerFunctionToIRState* state);

	void LowerBlockStatement(llvm::Module* module, Ref<BlockStatement> statement, LowerFunctionToIRState* state);

	void LowerDeleteStatement(llvm::Module* module, Ref<DeleteStatement> statement, LowerFunctionToIRState* state);

	void LowerForStatement(llvm::Module* module, Ref<ForStatement> statement, LowerFunctionToIRState* state);

	void LowerIfStatement(llvm::Module* module, Ref<IfStatement> statement, LowerFunctionToIRState* state);

	void LowerVariableDeclarationStatement(llvm::Module* module, Ref<VariableDeclarationStatement> statement, LowerFunctionToIRState* state);

	void LowerWhileStatement(llvm::Module* module, Ref<WhileStatement> statement, LowerFunctionToIRState* state);

	void LowerReturnStatement(llvm::Module* module, Ref<ReturnStatement> statement, LowerFunctionToIRState* state);

	void LowerStatement(llvm::Module* module, Ref<Statement> statement, LowerFunctionToIRState* state);

	llvm::Function* CreateFunction(llvm::Module* module, Ref<MethodDeclaration> method);

	void LowerMethod(llvm::Module* module, Ref<MethodDeclaration> method, Ref<ClassDeclaration> classDeclaration, llvm::legacy::FunctionPassManager* fpm);

	void LowerClass(Ref<Module> parentModule, Ref<ClassDeclaration> classDeclaration, BuildContext& context);

	void InitializeSingleton(Ref<llvm::Module> entryModule, Ref<Unit> unit, llvm::IRBuilder<>& entryBuilder, HashSet<UnitDeclaration*> initializedUnits);

	void InitializeSingleton(Ref<llvm::Module> entryModule, Ref<ClassDeclaration> classDeclaration, llvm::IRBuilder<>& entryBuilder,
	                         HashSet<UnitDeclaration*> initializedUnits);

	void LowerModule(Ref<Module> module, BuildContext& context);
};

#endif /* SOURCE_PASSES_LOWERTOIR */
