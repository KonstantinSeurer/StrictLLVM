
#include "LowerToIRPass.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

LowerToIRPass::LowerToIRPass() : Pass("LowerToIRPass")
{
	context = Allocate<llvm::LLVMContext>();
	builder = Allocate<llvm::IRBuilder<>>(*context);
}

void LowerToIRPass::LowerDataType(Ref<llvm::Module> module, Ref<DataType> type)
{
	if (type->dataTypeMeta.ir)
		return;

	if (type->dataTypeType == DataTypeType::PRIMITIVE)
	{
		Ref<PrimitiveType> primitive = std::dynamic_pointer_cast<PrimitiveType>(type);
		switch (primitive->primitiveType)
		{
		case TokenType::VOID:
			type->dataTypeMeta.ir = llvm::Type::getVoidTy(*context);
			break;
		case TokenType::BOOL:
			type->dataTypeMeta.ir = llvm::Type::getInt1Ty(*context);
			break;
		case TokenType::INT8:
		case TokenType::UINT8:
			type->dataTypeMeta.ir = llvm::Type::getInt8Ty(*context);
			break;
		case TokenType::INT16:
		case TokenType::UINT16:
			type->dataTypeMeta.ir = llvm::Type::getInt16Ty(*context);
			break;
		case TokenType::INT32:
		case TokenType::UINT32:
			type->dataTypeMeta.ir = llvm::Type::getInt32Ty(*context);
			break;
		case TokenType::INT64:
		case TokenType::UINT64:
			type->dataTypeMeta.ir = llvm::Type::getInt64Ty(*context);
			break;
		case TokenType::FLOAT32:
			type->dataTypeMeta.ir = llvm::Type::getFloatTy(*context);
			break;
		case TokenType::FLOAT64:
			type->dataTypeMeta.ir = llvm::Type::getDoubleTy(*context);
			break;
		default:
			STRICT_UNREACHABLE;
		}
	}
}

void LowerToIRPass::LowerCallExpression(Ref<llvm::Module> module, Ref<CallExpression> expression, LowerFunctionToIRState* state)
{
}

void LowerToIRPass::LowerIdentifierExpression(Ref<llvm::Module> module, Ref<IdentifierExpression> expression, LowerFunctionToIRState* state)
{
	if (expression->identifierExpressionMeta.destination->type == ASTItemType::VARIABLE_DECLARATION)
	{
		Ref<VariableDeclaration> destination = std::dynamic_pointer_cast<VariableDeclaration>(expression->identifierExpressionMeta.destination);

		if (destination->variableDeclarationMeta.irArgument)
		{
			expression->expressionMeta.ir = destination->variableDeclarationMeta.irArgument;
			return;
		}
	}

	STRICT_UNREACHABLE;
}

void LowerToIRPass::LowerLiteralExpression(Ref<llvm::Module> module, Ref<LiteralExpression> expression, LowerFunctionToIRState* state)
{
}

void LowerToIRPass::LowerNewExpression(Ref<llvm::Module> module, Ref<NewExpression> expression, LowerFunctionToIRState* state)
{
}

llvm::Value* LowerToIRPass::LowerIntOperator(OperatorType type, llvm::Value* a, llvm::Value* b, bool isSigned)
{
	switch (type)
	{
	case OperatorType::AND:
		return builder->CreateAnd(a, b);
	case OperatorType::DIVIDE: {
		if (isSigned)
		{
			return builder->CreateSDiv(a, b);
		}
		else
		{
			return builder->CreateUDiv(a, b);
		}
	}
	case OperatorType::EQUAL:
		return builder->CreateICmpEQ(a, b);
	case OperatorType::GREATER: {
		if (isSigned)
		{
			return builder->CreateICmpSGT(a, b);
		}
		else
		{
			return builder->CreateICmpUGT(a, b);
		}
	}
	case OperatorType::GREATER_EQUAL: {
		if (isSigned)
		{
			return builder->CreateICmpSGE(a, b);
		}
		else
		{
			return builder->CreateICmpUGE(a, b);
		}
	}
	case OperatorType::INVERSE:
		return builder->CreateNot(a);
	case OperatorType::LESS: {
		if (isSigned)
		{
			return builder->CreateICmpSLT(a, b);
		}
		else
		{
			return builder->CreateICmpULT(a, b);
		}
	}
	case OperatorType::LESS_EQUAL: {
		if (isSigned)
		{
			return builder->CreateICmpSLE(a, b);
		}
		else
		{
			return builder->CreateICmpULE(a, b);
		}
	}
	case OperatorType::MINUS:
		return builder->CreateSub(a, b);
	case OperatorType::MULTIPLY:
		return builder->CreateMul(a, b);
	case OperatorType::NEGATIVE:
		return builder->CreateNeg(a);
	case OperatorType::NOT:
		return builder->CreateNot(a);
	case OperatorType::OR:
		return builder->CreateOr(a, b);
	case OperatorType::PLUS:
		return builder->CreateAdd(a, b);
	case OperatorType::SHIFT_LEFT:
		return builder->CreateShl(a, b);
	case OperatorType::SHIFT_RIGHT:
		return builder->CreateLShr(a, b);
	case OperatorType::XOR:
		return builder->CreateXor(a, b);
	default:
		STRICT_UNREACHABLE;
	}
}

llvm::Value* LowerToIRPass::LowerFloatOperator(OperatorType type, llvm::Value* a, llvm::Value* b)
{
	switch (type)
	{
	case OperatorType::DIVIDE:
		return builder->CreateSDiv(a, b);
	case OperatorType::EQUAL:
		return builder->CreateICmpEQ(a, b);
	case OperatorType::GREATER:
		return builder->CreateICmpSGT(a, b);
	case OperatorType::GREATER_EQUAL:
		return builder->CreateICmpSGE(a, b);
	case OperatorType::LESS:
		return builder->CreateICmpSLT(a, b);
	case OperatorType::LESS_EQUAL:
		return builder->CreateICmpSLE(a, b);
	case OperatorType::MINUS:
		return builder->CreateSub(a, b);
	case OperatorType::MULTIPLY:
		return builder->CreateMul(a, b);
	case OperatorType::NEGATIVE:
		return builder->CreateNeg(a);
	case OperatorType::PLUS:
		return builder->CreateAdd(a, b);
	default:
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerOperatorExpression(Ref<llvm::Module> module, Ref<OperatorExpression> expression, LowerFunctionToIRState* state)
{
	LowerExpression(module, expression->a, state);

	llvm::Value* a = expression->a->expressionMeta.ir;
	llvm::Value* b = nullptr;

	if (expression->b && expression->b->expression)
	{
		LowerExpression(module, expression->b->expression, state);
		b = expression->b->expression->expressionMeta.ir;
	}

	if (expression->expressionMeta.dataType->dataTypeType == DataTypeType::PRIMITIVE)
	{
		Ref<PrimitiveType> primitiveType = std::dynamic_pointer_cast<PrimitiveType>(expression->expressionMeta.dataType);
		switch (primitiveType->primitiveType)
		{
		case TokenType::INT8:
		case TokenType::INT16:
		case TokenType::INT32:
		case TokenType::INT64: {
			expression->expressionMeta.ir = LowerIntOperator(expression->operatorType, a, b, true);
			break;
		}
		case TokenType::BOOL:
		case TokenType::UINT8:
		case TokenType::UINT16:
		case TokenType::UINT32:
		case TokenType::UINT64: {
			expression->expressionMeta.ir = LowerIntOperator(expression->operatorType, a, b, false);
			break;
		}
		case TokenType::FLOAT32:
		case TokenType::FLOAT64: {
			expression->expressionMeta.ir = LowerFloatOperator(expression->operatorType, a, b);
			break;
		}
		default:
			STRICT_UNREACHABLE;
		}
	}
}

void LowerToIRPass::LowerTernaryExpression(Ref<llvm::Module> module, Ref<TernaryExpression> expression, LowerFunctionToIRState* state)
{
	LowerExpression(module, expression->condition, state);

	llvm::BasicBlock* entryBlock = state->currentBlock;

	llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*context, "then", state->method->methodDeclarationMeta.ir);
	state->currentBlock = thenBlock;
	builder->SetInsertPoint(thenBlock);
	LowerExpression(module, expression->thenExpression, state);

	llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(*context, "else", state->method->methodDeclarationMeta.ir);
	state->currentBlock = elseBlock;
	builder->SetInsertPoint(elseBlock);
	LowerExpression(module, expression->elseExpression, state);

	llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*context, "merge", state->method->methodDeclarationMeta.ir);

	builder->SetInsertPoint(entryBlock);
	builder->CreateCondBr(expression->condition->expressionMeta.ir, thenBlock, elseBlock ? elseBlock : mergeBlock);

	builder->SetInsertPoint(thenBlock);
	builder->CreateBr(mergeBlock);

	builder->SetInsertPoint(elseBlock);
	builder->CreateBr(mergeBlock);

	state->currentBlock = mergeBlock;
	builder->SetInsertPoint(mergeBlock);

	LowerDataType(module, expression->expressionMeta.dataType);
	auto phi = builder->CreatePHI(expression->expressionMeta.dataType->dataTypeMeta.ir, 2);
	phi->addIncoming(expression->thenExpression->expressionMeta.ir, thenBlock);
	phi->addIncoming(expression->elseExpression->expressionMeta.ir, elseBlock);
	expression->expressionMeta.ir = phi;
}

void LowerToIRPass::LowerExpression(Ref<llvm::Module> module, Ref<Expression> expression, LowerFunctionToIRState* state)
{
	switch (expression->expressionType)
	{
	case ExpressionType::BRACKET: {
		Ref<BracketExpression> bracketExpression = std::dynamic_pointer_cast<BracketExpression>(expression);
		LowerExpression(module, bracketExpression->expression, state);
		break;
	}
	case ExpressionType::CALL: {
		Ref<CallExpression> callExpression = std::dynamic_pointer_cast<CallExpression>(expression);
		LowerCallExpression(module, callExpression, state);
		break;
	}
	case ExpressionType::IDENTIFIER: {
		Ref<IdentifierExpression> identifierExpression = std::dynamic_pointer_cast<IdentifierExpression>(expression);
		LowerIdentifierExpression(module, identifierExpression, state);
		break;
	}
	case ExpressionType::LITERAL: {
		Ref<LiteralExpression> literalExpression = std::dynamic_pointer_cast<LiteralExpression>(expression);
		LowerLiteralExpression(module, literalExpression, state);
		break;
	}
	case ExpressionType::NEW: {
		Ref<NewExpression> newExpression = std::dynamic_pointer_cast<NewExpression>(expression);
		LowerNewExpression(module, newExpression, state);
		break;
	}
	case ExpressionType::OPERATOR: {
		Ref<OperatorExpression> operatorExpression = std::dynamic_pointer_cast<OperatorExpression>(expression);
		LowerOperatorExpression(module, operatorExpression, state);
		break;
	}
	case ExpressionType::TERNARY: {
		Ref<TernaryExpression> ternaryExpression = std::dynamic_pointer_cast<TernaryExpression>(expression);
		LowerTernaryExpression(module, ternaryExpression, state);
		break;
	}
	}
}

void LowerToIRPass::LowerDeleteStatement(Ref<llvm::Module> module, Ref<DeleteStatement> statement, LowerFunctionToIRState* state)
{
	LowerExpression(module, statement->expression, state);
}

void LowerToIRPass::LowerForStatement(Ref<llvm::Module> module, Ref<ForStatement> statement, LowerFunctionToIRState* state)
{
}

bool EndsWithJump(Ref<Statement> statement)
{
	switch (statement->statementType)
	{
	case StatementType::BLOCK: {
		Ref<BlockStatement> blockStatement = std::dynamic_pointer_cast<BlockStatement>(statement);
		if (blockStatement->statements.empty())
		{
			return false;
		}
		return EndsWithJump(blockStatement->statements[blockStatement->statements.size() - 1]);
	}
	case StatementType::BREAK:
	case StatementType::CONTINUE:
	case StatementType::RETURN:
		return true;
	default:
		return false;
	}
}

void LowerToIRPass::LowerIfStatement(Ref<llvm::Module> module, Ref<IfStatement> statement, LowerFunctionToIRState* state)
{
	LowerExpression(module, statement->condition, state);

	llvm::BasicBlock* entryBlock = state->currentBlock;

	llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*context, "then", state->method->methodDeclarationMeta.ir);
	state->currentBlock = thenBlock;
	builder->SetInsertPoint(thenBlock);
	LowerStatement(module, statement->thenStatement, state);

	llvm::BasicBlock* elseBlock = nullptr;
	if (statement->elseStatement)
	{
		elseBlock = llvm::BasicBlock::Create(*context, "else", state->method->methodDeclarationMeta.ir);
		state->currentBlock = elseBlock;
		builder->SetInsertPoint(elseBlock);
		LowerStatement(module, statement->elseStatement, state);
	}

	llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*context, "merge", state->method->methodDeclarationMeta.ir);

	builder->SetInsertPoint(entryBlock);
	builder->CreateCondBr(statement->condition->expressionMeta.ir, thenBlock, elseBlock ? elseBlock : mergeBlock);

	if (!EndsWithJump(statement->thenStatement))
	{
		builder->SetInsertPoint(thenBlock);
		builder->CreateBr(mergeBlock);
	}

	if (statement->elseStatement && !EndsWithJump(statement->elseStatement))
	{
		builder->SetInsertPoint(elseBlock);
		builder->CreateBr(mergeBlock);
	}

	state->currentBlock = mergeBlock;
	builder->SetInsertPoint(mergeBlock);
}

void LowerToIRPass::LowerVariableDeclarationStatement(Ref<llvm::Module> module, Ref<VariableDeclarationStatement> statement, LowerFunctionToIRState* state)
{
}

void LowerToIRPass::LowerWhileStatement(Ref<llvm::Module> module, Ref<WhileStatement> statement, LowerFunctionToIRState* state)
{
}

void LowerToIRPass::LowerStatement(Ref<llvm::Module> module, Ref<Statement> statement, LowerFunctionToIRState* state)
{
	switch (statement->statementType)
	{
	case StatementType::BLOCK: {
		Ref<BlockStatement> blockStatement = std::dynamic_pointer_cast<BlockStatement>(statement);
		for (auto subStatement : blockStatement->statements)
		{
			LowerStatement(module, subStatement, state);
		}
		break;
	}
	case StatementType::BREAK: {
		builder->CreateBr(state->breakBlock);
		break;
	}
	case StatementType::CONTINUE: {
		builder->CreateBr(state->continueBlock);
		break;
	}
	case StatementType::DELETE: {
		Ref<DeleteStatement> deleteStatement = std::dynamic_pointer_cast<DeleteStatement>(statement);
		LowerDeleteStatement(module, deleteStatement, state);
		break;
	}
	case StatementType::EXPRESSION: {
		Ref<ExpressionStatement> expressionStatement = std::dynamic_pointer_cast<ExpressionStatement>(statement);
		LowerExpression(module, expressionStatement->expression, state);
		break;
	}
	case StatementType::FOR: {
		Ref<ForStatement> forStatement = std::dynamic_pointer_cast<ForStatement>(statement);
		LowerForStatement(module, forStatement, state);
		break;
	}
	case StatementType::IF: {
		Ref<IfStatement> ifStatement = std::dynamic_pointer_cast<IfStatement>(statement);
		LowerIfStatement(module, ifStatement, state);
		break;
	}
	case StatementType::RETURN: {
		Ref<ReturnStatement> returnStatement = std::dynamic_pointer_cast<ReturnStatement>(statement);
		LowerExpression(module, returnStatement->expression, state);
		builder->CreateRet(returnStatement->expression->expressionMeta.ir);
		break;
	}
	case StatementType::VARIABLE_DECLARATION: {
		Ref<VariableDeclarationStatement> variableDeclarationStatement = std::dynamic_pointer_cast<VariableDeclarationStatement>(statement);
		LowerVariableDeclarationStatement(module, variableDeclarationStatement, state);
		break;
	}
	case StatementType::WHILE: {
		Ref<WhileStatement> whileStatement = std::dynamic_pointer_cast<WhileStatement>(statement);
		LowerWhileStatement(module, whileStatement, state);
		break;
	}
	}
}

void LowerToIRPass::LowerMethod(Ref<llvm::Module> module, Ref<MethodDeclaration> method)
{
	LowerDataType(module, method->dataType);

	Array<llvm::Type*> parameters;
	for (auto parameter : method->parameters)
	{
		LowerDataType(module, parameter->dataType);
		parameters.push_back(parameter->dataType->dataTypeMeta.ir);
	}

	auto signature = llvm::FunctionType::get(method->dataType->dataTypeMeta.ir, parameters, false);
	method->methodDeclarationMeta.ir = llvm::Function::Create(signature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, method->name, *module);

	for (UInt32 parameterIndex = 0; parameterIndex < method->parameters.size(); parameterIndex++)
	{
		auto argument = method->methodDeclarationMeta.ir->getArg(parameterIndex);
#ifdef DEBUG
		argument->setName(method->parameters[parameterIndex]->name);
#endif
		method->parameters[parameterIndex]->variableDeclarationMeta.irArgument = argument;
	}

	if (method->body)
	{
		auto block = llvm::BasicBlock::Create(*context, "entry", method->methodDeclarationMeta.ir);
		builder->SetInsertPoint(block);

		LowerFunctionToIRState state;
		state.currentBlock = block;
		state.method = method.get();

		LowerStatement(module, method->body, &state);
	}
}

void LowerToIRPass::LowerClass(Ref<ClassDeclaration> classDeclaration)
{
	if (classDeclaration->typeTemplate)
	{
		return;
	}

	auto module = Allocate<llvm::Module>(classDeclaration->name, *context);

	for (auto member : classDeclaration->members)
	{
		if (member->variableType == VariableDeclarationType::METHOD)
		{
			LowerMethod(module, std::dynamic_pointer_cast<MethodDeclaration>(member));
		}
	}

	module->print(llvm::outs(), nullptr);

	classDeclaration->classDeclarationMeta.module = module;
}

PassResultFlags LowerToIRPass::Run(PrintFunction print, BuildContext& context)
{
	for (auto module : context.GetModules())
	{
		for (auto& specialization : module->moduleMeta.templateSpecializations)
		{
			LowerClass(specialization.second);
		}

		for (auto unit : module->units)
		{
			if (unit->declaredType->declarationType != UnitDeclarationType::CLASS)
			{
				continue;
			}

			LowerClass(std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType));
		}
	}

	return PassResultFlags::SUCCESS;
}
