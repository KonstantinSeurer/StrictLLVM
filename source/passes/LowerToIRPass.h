
#ifndef SOURCE_PASSES_LOWERTOIR
#define SOURCE_PASSES_LOWERTOIR

#include "../BuildContext.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"

class LowerUnitToIRState
{
public:
	Ref<ClassDeclaration> classDeclaration;
	llvm::Module* module;
};

class LowerFunctionToIRState
{
public:
	const LowerUnitToIRState* parent;
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
	llvm::IRBuilder<>* builder;
	llvm::LLVMContext* context;

public:
	LowerToIRPass();

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);

private:
	void LowerDataType(Ref<DataType> type, const LowerUnitToIRState* state);

	void LowerCallExpression(Ref<CallExpression> expression, LowerFunctionToIRState* state);

	void LowerIdentifierExpression(Ref<IdentifierExpression> expression, LowerFunctionToIRState* state);

	void LowerLiteralExpression(Ref<LiteralExpression> expression, LowerFunctionToIRState* state);

	void LowerNewExpression(Ref<NewExpression> expression, LowerFunctionToIRState* state);

	llvm::Value* LowerIntOperator(OperatorType type, llvm::Value* a, llvm::Value* b, bool isSigned);

	llvm::Value* LowerFloatOperator(OperatorType type, llvm::Value* a, llvm::Value* b);

	void LowerOperatorExpression(Ref<OperatorExpression> expression, LowerFunctionToIRState* state);

	void LowerTernaryExpression(Ref<TernaryExpression> expression, LowerFunctionToIRState* state);

	void LowerExpression(Ref<Expression> expression, LowerFunctionToIRState* state);

	void LowerBlockStatement(Ref<BlockStatement> statement, LowerFunctionToIRState* state);

	void LowerDeleteStatement(Ref<DeleteStatement> statement, LowerFunctionToIRState* state);

	void LowerForStatement(Ref<ForStatement> statement, LowerFunctionToIRState* state);

	void LowerIfStatement(Ref<IfStatement> statement, LowerFunctionToIRState* state);

	void LowerVariableDeclarationStatement(Ref<VariableDeclarationStatement> statement, LowerFunctionToIRState* state);

	void LowerWhileStatement(Ref<WhileStatement> statement, LowerFunctionToIRState* state);

	void LowerReturnStatement(Ref<ReturnStatement> statement, LowerFunctionToIRState* state);

	void LowerStatement(Ref<Statement> statement, LowerFunctionToIRState* state);

	llvm::Function* CreateFunction(Ref<MethodDeclaration> method, const LowerUnitToIRState* state);

	PassResultFlags LowerMethod(Ref<MethodDeclaration> method, llvm::legacy::FunctionPassManager* fpm, const LowerUnitToIRState* state);

	PassResultFlags LowerClass(Ref<Module> parentModule, Ref<ClassDeclaration> classDeclaration, BuildContext& context);

	void InitializeSingleton(Ref<llvm::Module> entryModule, Ref<Unit> unit, llvm::IRBuilder<>& entryBuilder, HashSet<UnitDeclaration*> initializedUnits);

	void InitializeSingleton(Ref<llvm::Module> entryModule, Ref<ClassDeclaration> classDeclaration, llvm::IRBuilder<>& entryBuilder,
	                         HashSet<UnitDeclaration*> initializedUnits);

	PassResultFlags LowerModule(Ref<Module> module, BuildContext& context);
};

#endif /* SOURCE_PASSES_LOWERTOIR */
