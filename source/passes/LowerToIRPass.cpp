
#include "LowerToIRPass.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"

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
	else if (type->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(type);
		assert(objectType->objectTypeMeta.unit);

		Ref<UnitDeclaration> unitDeclaration = objectType->objectTypeMeta.unit->declaredType;
		assert(unitDeclaration->declarationType != UnitDeclarationType::TYPE);

		if (unitDeclaration->declarationType == UnitDeclarationType::ERROR)
		{
			type->dataTypeMeta.ir = llvm::Type::getInt32Ty(*context);
		}
		else
		{
			Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(unitDeclaration);
			Array<llvm::Type*> elements;
			for (auto member : classDeclaration->members)
			{

				if (member->variableType != VariableDeclarationType::MEMBER_VARIABLE)
				{
					continue;
				}

				LowerDataType(module, member->dataType);
				elements.push_back(member->dataType->dataTypeMeta.ir);
			}
			type->dataTypeMeta.ir = llvm::StructType::create(*context, elements, objectType->name);
		}
	}
	else if (type->dataTypeType == DataTypeType::POINTER || type->dataTypeType == DataTypeType::REFERENCE || type->dataTypeType == DataTypeType::ARRAY)
	{
		Ref<PointerType> pointerType = std::dynamic_pointer_cast<PointerType>(type);
		LowerDataType(module, pointerType->value);
		type->dataTypeMeta.ir = llvm::PointerType::get(pointerType->value->dataTypeMeta.ir, 0);
	}
	else
	{
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerCallExpression(Ref<llvm::Module> module, Ref<CallExpression> expression, LowerFunctionToIRState* state)
{
	assert(expression->callExpressionMeta.destination);

	if (expression->callExpressionMeta.destination->variableType == VariableDeclarationType::METHOD)
	{
		Ref<MethodDeclaration> method = std::dynamic_pointer_cast<MethodDeclaration>(expression->callExpressionMeta.destination);

		Array<llvm::Value*> arguments;
		if (expression->callExpressionMeta.context)
		{
			Ref<Expression> context = expression->callExpressionMeta.context;
			LowerExpression(module, context, state);

			assert(context->expressionMeta.pointer);
			arguments.push_back(context->expressionMeta.ir);
		}
		else
		{
			arguments.push_back(state->thisPointer);
		}

		for (auto argument : expression->arguments)
		{
			LowerExpression(module, argument, state);
			arguments.push_back(argument->expressionMeta.Load(*builder));
		}

		expression->expressionMeta.ir = builder->CreateCall(method->methodDeclarationMeta.ir, arguments);
	}
	else
	{
		// TODO: Implement function pointers.
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerIdentifierExpression(Ref<llvm::Module> module, Ref<IdentifierExpression> expression, LowerFunctionToIRState* state)
{
	if (expression->identifierExpressionMeta.destination->type == ASTItemType::VARIABLE_DECLARATION)
	{
		Ref<VariableDeclaration> destination = std::dynamic_pointer_cast<VariableDeclaration>(expression->identifierExpressionMeta.destination);
		if (destination->variableType == VariableDeclarationType::VARIABLE)
		{
			expression->expressionMeta.ir = destination->variableDeclarationMeta.ir;
			expression->expressionMeta.pointer = true;
		}
		else if (destination->variableType == VariableDeclarationType::MEMBER_VARIABLE)
		{
			Ref<MemberVariableDeclaration> memberVariable = std::dynamic_pointer_cast<MemberVariableDeclaration>(destination);
			Array<llvm::Value*> index = {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0),
			                             llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), memberVariable->memberVariableDeclarationMeta.index)};
			expression->expressionMeta.ir =
				builder->CreateGEP(state->thisPointer->getType()->getPointerElementType(), state->thisPointer, index, memberVariable->name);
			expression->expressionMeta.pointer = true;
		}
		else
		{
			STRICT_UNREACHABLE;
		}
	}
	else if (expression->identifierExpressionMeta.destination->type == ASTItemType::UNIT)
	{
		// TODO: Lower to global variable
		expression->expressionMeta.pointer = true;
	}
	else
	{
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerLiteralExpression(Ref<llvm::Module> module, Ref<LiteralExpression> expression, LowerFunctionToIRState* state)
{
	assert(expression->expressionMeta.dataType);
	assert(expression->expressionMeta.dataType->dataTypeType == DataTypeType::PRIMITIVE);

	LowerDataType(module, expression->expressionMeta.dataType);

	Ref<PrimitiveType> dataType = std::dynamic_pointer_cast<PrimitiveType>(expression->expressionMeta.dataType);
	switch (dataType->primitiveType)
	{
	case TokenType::BOOL: {
		expression->expressionMeta.ir = llvm::ConstantInt::getBool(dataType->dataTypeMeta.ir, expression->data.data.boolData);
		break;
	}
	case TokenType::INT8:
	case TokenType::INT16:
	case TokenType::INT32:
	case TokenType::INT64: {
		expression->expressionMeta.ir = llvm::ConstantInt::get(dataType->dataTypeMeta.ir, expression->data.data.intData);
		break;
	}
	case TokenType::UINT8:
	case TokenType::UINT16:
	case TokenType::UINT32:
	case TokenType::UINT64: {
		expression->expressionMeta.ir = llvm::ConstantInt::get(dataType->dataTypeMeta.ir, expression->data.data.uintData);
		break;
	}
	case TokenType::FLOAT32:
	case TokenType::FLOAT64: {
		expression->expressionMeta.ir = llvm::ConstantFP::get(dataType->dataTypeMeta.ir, expression->data.data.floatData);
		break;
	}
	default:
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerNewExpression(Ref<llvm::Module> module, Ref<NewExpression> expression, LowerFunctionToIRState* state)
{
	LowerDataType(module, expression->dataType);

	llvm::Value* one = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 1);

	llvm::Value* arrayLength = one;
	if (expression->dataType->dataTypeType == DataTypeType::ARRAY)
	{
		Ref<PointerType> currentType = std::dynamic_pointer_cast<PointerType>(expression->dataType);
		while (true)
		{
			LowerExpression(module, currentType->arrayLength, state);
			arrayLength = builder->CreateMul(arrayLength, currentType->arrayLength->expressionMeta.ir);
			if (currentType->value->dataTypeType != DataTypeType::ARRAY)
			{
				break;
			}
			currentType = std::dynamic_pointer_cast<PointerType>(currentType->value);
		}
	}

	llvm::Value* size = builder->CreateGEP(expression->dataType->dataTypeMeta.ir,
	                                       llvm::ConstantPointerNull::get(llvm::PointerType::get(expression->dataType->dataTypeMeta.ir, 0)), {one});

	expression->expressionMeta.ir = builder->CreateCall(state->classDeclaration->classDeclarationMeta.malloc, {size});

	// TODO: Call the constructor.
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

static bool IsAccessOperator(OperatorType op)
{
	switch (op)
	{
	case OperatorType::ACCESS:
	case OperatorType::ARRAY_ACCESS:
	case OperatorType::ASSIGN:
		return true;
	}

	return false;
}

void LowerToIRPass::LowerOperatorExpression(Ref<llvm::Module> module, Ref<OperatorExpression> expression, LowerFunctionToIRState* state)
{
	LowerExpression(module, expression->a, state);

	bool access = IsAccessOperator(expression->operatorType);

	llvm::Value* a = access ? expression->a->expressionMeta.ir : expression->a->expressionMeta.Load(*builder);
	llvm::Value* b = nullptr;

	if (expression->b && expression->b->expression)
	{
		LowerExpression(module, expression->b->expression, state);
		b = expression->b->expression->expressionMeta.Load(*builder);
	}

	if (expression->operatorType == OperatorType::ASSIGN)
	{
		builder->CreateStore(b, a);
		expression->expressionMeta = expression->b->expression->expressionMeta;
		return;
	}
	else if (expression->operatorType == OperatorType::ARRAY_ACCESS)
	{
		LowerDataType(module, expression->a->expressionMeta.dataType);
		llvm::Value* ppElement = builder->CreateGEP(expression->a->expressionMeta.dataType->dataTypeMeta.ir, a, b);
		expression->expressionMeta.ir = builder->CreateLoad(ppElement->getType()->getPointerElementType(), ppElement);
		expression->expressionMeta.pointer = true;
		return;
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
	builder->CreateCondBr(expression->condition->expressionMeta.Load(*builder), thenBlock, elseBlock ? elseBlock : mergeBlock);

	builder->SetInsertPoint(thenBlock);
	builder->CreateBr(mergeBlock);

	builder->SetInsertPoint(elseBlock);
	builder->CreateBr(mergeBlock);

	state->currentBlock = mergeBlock;
	builder->SetInsertPoint(mergeBlock);

	LowerDataType(module, expression->expressionMeta.dataType);
	auto phi = builder->CreatePHI(expression->expressionMeta.dataType->dataTypeMeta.ir, 2);
	phi->addIncoming(expression->thenExpression->expressionMeta.Load(*builder), thenBlock);
	phi->addIncoming(expression->elseExpression->expressionMeta.Load(*builder), elseBlock);
	expression->expressionMeta.ir = phi;
	expression->expressionMeta.pointer = false;
}

void LowerToIRPass::LowerExpression(Ref<llvm::Module> module, Ref<Expression> expression, LowerFunctionToIRState* state)
{
	switch (expression->expressionType)
	{
	case ExpressionType::BRACKET:
		LowerExpression(module, std::dynamic_pointer_cast<BracketExpression>(expression)->expression, state);
		break;
	case ExpressionType::CALL:
		LowerCallExpression(module, std::dynamic_pointer_cast<CallExpression>(expression), state);
		break;
	case ExpressionType::IDENTIFIER:
		LowerIdentifierExpression(module, std::dynamic_pointer_cast<IdentifierExpression>(expression), state);
		break;
	case ExpressionType::LITERAL:
		LowerLiteralExpression(module, std::dynamic_pointer_cast<LiteralExpression>(expression), state);
		break;
	case ExpressionType::NEW:
		LowerNewExpression(module, std::dynamic_pointer_cast<NewExpression>(expression), state);
		break;
	case ExpressionType::OPERATOR:
		LowerOperatorExpression(module, std::dynamic_pointer_cast<OperatorExpression>(expression), state);
		break;
	case ExpressionType::TERNARY:
		LowerTernaryExpression(module, std::dynamic_pointer_cast<TernaryExpression>(expression), state);
		break;
	}
}

void LowerToIRPass::LowerBlockStatement(Ref<llvm::Module> module, Ref<BlockStatement> statement, LowerFunctionToIRState* state)
{
	for (auto subStatement : statement->statements)
	{
		LowerStatement(module, subStatement, state);
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
	LowerDataType(module, statement->declaration->dataType);

	auto function = state->method->methodDeclarationMeta.ir;
	llvm::IRBuilder<> tmpBuilder(&function->getEntryBlock(), function->getEntryBlock().begin());

	auto variable = tmpBuilder.CreateAlloca(statement->declaration->dataType->dataTypeMeta.ir, nullptr, statement->declaration->name);
	statement->declaration->variableDeclarationMeta.ir = variable;

	if (statement->value)
	{
		LowerExpression(module, statement->value, state);

		builder->CreateStore(statement->value->expressionMeta.ir, variable);
	}
}

void LowerToIRPass::LowerWhileStatement(Ref<llvm::Module> module, Ref<WhileStatement> statement, LowerFunctionToIRState* state)
{
}

void LowerToIRPass::LowerReturnStatement(Ref<llvm::Module> module, Ref<ReturnStatement> statement, LowerFunctionToIRState* state)
{
	LowerExpression(module, statement->expression, state);
	if (state->method->dataType->dataTypeType == DataTypeType::REFERENCE)
	{
		assert(statement->expression->expressionMeta.pointer);
		builder->CreateRet(statement->expression->expressionMeta.ir);
	}
	else
	{
		builder->CreateRet(statement->expression->expressionMeta.Load(*builder));
	}
}

void LowerToIRPass::LowerStatement(Ref<llvm::Module> module, Ref<Statement> statement, LowerFunctionToIRState* state)
{
	switch (statement->statementType)
	{
	case StatementType::BLOCK:
		LowerBlockStatement(module, std::dynamic_pointer_cast<BlockStatement>(statement), state);
		break;
	case StatementType::BREAK:
		builder->CreateBr(state->breakBlock);
		break;
	case StatementType::CONTINUE:
		builder->CreateBr(state->continueBlock);
		break;
	case StatementType::DELETE:
		LowerDeleteStatement(module, std::dynamic_pointer_cast<DeleteStatement>(statement), state);
		break;
	case StatementType::EXPRESSION:
		LowerExpression(module, std::dynamic_pointer_cast<ExpressionStatement>(statement)->expression, state);
		break;
	case StatementType::FOR:
		LowerForStatement(module, std::dynamic_pointer_cast<ForStatement>(statement), state);
		break;
	case StatementType::IF:
		LowerIfStatement(module, std::dynamic_pointer_cast<IfStatement>(statement), state);
		break;
	case StatementType::RETURN:
		LowerReturnStatement(module, std::dynamic_pointer_cast<ReturnStatement>(statement), state);
		break;
	case StatementType::VARIABLE_DECLARATION:
		LowerVariableDeclarationStatement(module, std::dynamic_pointer_cast<VariableDeclarationStatement>(statement), state);
		break;
	case StatementType::WHILE:
		LowerWhileStatement(module, std::dynamic_pointer_cast<WhileStatement>(statement), state);
		break;
	}
}

void LowerToIRPass::LowerMethod(Ref<llvm::Module> module, Ref<MethodDeclaration> method, Ref<ClassDeclaration> classDeclaration,
                                llvm::legacy::FunctionPassManager& fpm)
{
	LowerDataType(module, method->dataType);

	Array<llvm::Type*> parameters;
	parameters.push_back(llvm::PointerType::get(classDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir, 0));
	for (auto parameter : method->parameters)
	{
		LowerDataType(module, parameter->dataType);
		parameters.push_back(parameter->dataType->dataTypeMeta.ir);
	}

	auto signature = llvm::FunctionType::get(method->dataType->dataTypeMeta.ir, parameters, false);
	auto function = llvm::Function::Create(signature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, method->methodDeclarationMeta.name, *module);
	method->methodDeclarationMeta.ir = function;

	if (method->body)
	{
		auto block = llvm::BasicBlock::Create(*context, "entry", function);
		builder->SetInsertPoint(block);

		auto thisArgument = function->getArg(0);
		thisArgument->setName("this");

		for (UInt32 parameterIndex = 0; parameterIndex < method->parameters.size(); parameterIndex++)
		{
			auto argument = function->getArg(parameterIndex + 1);
#ifdef DEBUG
			argument->setName(method->parameters[parameterIndex]->name);
#endif
			llvm::IRBuilder<> tmpBuilder(&function->getEntryBlock(), function->getEntryBlock().begin());
			auto argumentVariable = tmpBuilder.CreateAlloca(argument->getType());
			builder->CreateStore(argument, argumentVariable);
			method->parameters[parameterIndex]->variableDeclarationMeta.ir = argumentVariable;
		}

		LowerFunctionToIRState state;
		state.currentBlock = block;
		state.method = method.get();
		state.classDeclaration = classDeclaration;
		state.thisPointer = thisArgument;

		LowerStatement(module, method->body, &state);

		if (method->dataType->dataTypeType == DataTypeType::PRIMITIVE)
		{
			Ref<PrimitiveType> primitiveType = std::dynamic_pointer_cast<PrimitiveType>(method->dataType);
			if (primitiveType->primitiveType == TokenType::VOID && !EndsWithJump(method->body))
			{
				builder->CreateRetVoid();
			}
		}

		llvm::verifyFunction(*function);
		// fpm.run(*function);
	}
}

void LowerToIRPass::LowerClass(Ref<ClassDeclaration> classDeclaration)
{
	if (classDeclaration->typeTemplate)
	{
		return;
	}

	auto module = Allocate<llvm::Module>(classDeclaration->name, *context);

	auto mallocSignature = llvm::FunctionType::get(llvm::Type::getInt8PtrTy(*context), {llvm::Type::getInt64Ty(*context)}, false);
	classDeclaration->classDeclarationMeta.malloc =
		llvm::Function::Create(mallocSignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "malloc", *module);

	auto freeSignature = llvm::FunctionType::get(llvm::Type::getVoidTy(*context), {llvm::Type::getInt8PtrTy(*context)}, false);
	classDeclaration->classDeclarationMeta.free = llvm::Function::Create(freeSignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "free", *module);

	LowerDataType(module, classDeclaration->unitDeclarationMeta.thisType);

	// TODO: Swith to the new pass manager once llvm 15 is released
	llvm::legacy::FunctionPassManager fpm = llvm::legacy::FunctionPassManager(module.get());
	fpm.add(llvm::createPromoteMemoryToRegisterPass());
	fpm.add(llvm::createInstructionCombiningPass());
	fpm.add(llvm::createReassociatePass());
	fpm.add(llvm::createGVNPass());
	fpm.add(llvm::createCFGSimplificationPass());
	fpm.doInitialization();

	for (auto member : classDeclaration->members)
	{
		LowerDataType(module, member->dataType);

		if (member->variableType == VariableDeclarationType::METHOD)
		{
			LowerMethod(module, std::dynamic_pointer_cast<MethodDeclaration>(member), classDeclaration, fpm);
		}
		else if (member->variableType == VariableDeclarationType::MEMBER_VARIABLE)
		{
			Ref<MemberVariableDeclaration> memberVariableDeclaration = std::dynamic_pointer_cast<MemberVariableDeclaration>(member);
			for (auto accessor : memberVariableDeclaration->accessors)
			{
				LowerMethod(module, accessor, classDeclaration, fpm);
			}
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
