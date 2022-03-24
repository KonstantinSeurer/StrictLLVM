
#include "LowerToIRPass.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"

LowerToIRPass::LowerToIRPass() : Pass("LowerToIRPass")
{
	context = new llvm::LLVMContext();
	builder = new llvm::IRBuilder<>(*context);
}

void LowerToIRPass::LowerDataType(llvm::Module* module, Ref<DataType> type)
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

		if (unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir)
		{
			type->dataTypeMeta.ir = unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir;
			return;
		}

		if (unitDeclaration->declarationType == UnitDeclarationType::ERROR)
		{
			unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir = llvm::Type::getInt32Ty(*context);
		}
		else
		{
			Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(unitDeclaration);
			Array<llvm::Type*> elements;
			for (auto superType : classDeclaration->superTypes)
			{
				LowerDataType(module, superType);
				elements.push_back(superType->dataTypeMeta.ir);
			}
			for (auto member : classDeclaration->members)
			{
				if (member->variableType != VariableDeclarationType::MEMBER_VARIABLE)
				{
					continue;
				}

				LowerDataType(module, member->dataType);
				elements.push_back(member->dataType->dataTypeMeta.ir);
			}
			unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir = llvm::StructType::create(*context, elements, objectType->name);
		}

		type->dataTypeMeta.ir = unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir;
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

static bool FindSuperDeclaration(Ref<TypeDeclaration> type, TypeDeclaration* declarationParent, Array<UInt32>& target)
{
	for (UInt32 index = 0; index < type->superTypes.size(); index++)
	{
		assert(type->superTypes[index]->dataTypeType == DataTypeType::OBJECT);

		Ref<ObjectType> superObject = std::dynamic_pointer_cast<ObjectType>(type->superTypes[index]);
		assert(superObject->objectTypeMeta.unit);
		assert(superObject->objectTypeMeta.unit->declaredType->IsType());

		Ref<TypeDeclaration> superTypeDeclaration = std::dynamic_pointer_cast<TypeDeclaration>(superObject->objectTypeMeta.unit->declaredType);

		if (superTypeDeclaration.get() == declarationParent || FindSuperDeclaration(superTypeDeclaration, declarationParent, target))
		{
			target.push_back(index);
			return true;
		}
	}

	return false;
}

void LowerToIRPass::LowerCallExpression(llvm::Module* module, Ref<CallExpression> expression, LowerFunctionToIRState* state)
{
	assert(expression->callExpressionMeta.destination);

	if (expression->callExpressionMeta.destination->variableType == VariableDeclarationType::METHOD)
	{
		Ref<MethodDeclaration> method = std::dynamic_pointer_cast<MethodDeclaration>(expression->callExpressionMeta.destination);
		LowerMethod(module, method, state->classDeclaration, nullptr);

		Array<llvm::Value*> arguments;
		if (expression->callExpressionMeta.context)
		{
			Ref<Expression> context = expression->callExpressionMeta.context;
			LowerExpression(module, context, state);

			assert(context->expressionMeta.pointer);
			assert(context->expressionMeta.dataType->dataTypeType == DataTypeType::OBJECT);

			Ref<ObjectType> contextType = std::dynamic_pointer_cast<ObjectType>(context->expressionMeta.dataType);
			assert(contextType->objectTypeMeta.unit);
			assert(contextType->objectTypeMeta.unit->declaredType->IsType());

			Ref<TypeDeclaration> contextTypeDeclaration = std::dynamic_pointer_cast<TypeDeclaration>(contextType->objectTypeMeta.unit->declaredType);

			if (contextTypeDeclaration.get() != method->variableDeclarationMeta.parentType)
			{
				// This method must be defined by a super type
				Array<UInt32> indices;
				bool found = FindSuperDeclaration(contextTypeDeclaration, method->variableDeclarationMeta.parentType, indices);
				assert(found);

				llvm::Value* thisPointer = context->expressionMeta.ir;
				for (Int32 indexIndex = indices.size() - 1; indexIndex >= 0; indexIndex--)
				{
					Array<llvm::Value*> index = {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*this->context), 0),
					                             llvm::ConstantInt::get(llvm::Type::getInt32Ty(*this->context), indices[indexIndex])};
					thisPointer = builder->CreateGEP(thisPointer->getType()->getPointerElementType(), thisPointer, index);
				}

				arguments.push_back(thisPointer);
			}
			else
			{
				arguments.push_back(context->expressionMeta.ir);
			}
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

		if ((method->flags & DeclarationFlags::VIRTUAL) == DeclarationFlags::VIRTUAL)
		{
			expression->expressionMeta.ir = llvm::Constant::getNullValue(method->dataType->dataTypeMeta.ir);
		}
		else
		{
			expression->expressionMeta.ir = builder->CreateCall(state->classDeclaration->classDeclarationMeta.methods.at(method.get()), arguments);
		}
	}
	else
	{
		// TODO: Implement function pointers.
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerIdentifierExpression(llvm::Module* module, Ref<IdentifierExpression> expression, LowerFunctionToIRState* state)
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
		Ref<Unit> unit = std::dynamic_pointer_cast<Unit>(expression->identifierExpressionMeta.destination);
		assert(unit->declaredType->declarationType == UnitDeclarationType::CLASS);

		Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType);
		assert(classDeclaration->isSingleton);

		auto& singletons = state->classDeclaration->classDeclarationMeta.singletons;
		if (singletons.find(classDeclaration.get()) == singletons.end())
		{
			expression->expressionMeta.ir = new llvm::GlobalVariable(*module, classDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir, false,
			                                                         llvm::GlobalValue::LinkageTypes::ExternalLinkage, nullptr, classDeclaration->name);
			singletons[classDeclaration.get()] = expression->expressionMeta.ir;
		}
		else
		{
			expression->expressionMeta.ir = singletons.at(classDeclaration.get());
		}

		expression->expressionMeta.pointer = true;
	}
	else
	{
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerLiteralExpression(llvm::Module* module, Ref<LiteralExpression> expression, LowerFunctionToIRState* state)
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

void LowerToIRPass::LowerNewExpression(llvm::Module* module, Ref<NewExpression> expression, LowerFunctionToIRState* state)
{
	Ref<DataType> dataType = expression->dataType;
	LowerDataType(module, dataType);

	if (expression->allocationType == AllocationType::HEAP)
	{
		llvm::Value* arrayLength;
		if (dataType->dataTypeType == DataTypeType::ARRAY)
		{
			Ref<PointerType> arrayType = std::dynamic_pointer_cast<PointerType>(dataType);
			LowerExpression(module, arrayType->arrayLength, state);
			arrayLength = arrayType->arrayLength->expressionMeta.Load(*builder);
			dataType = arrayType->value;
		}
		else
		{
			arrayLength = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 1);
		}

		llvm::Value* null = llvm::ConstantPointerNull::get(llvm::PointerType::get(dataType->dataTypeMeta.ir, 0));
		llvm::Value* sizePointer = builder->CreateGEP(dataType->dataTypeMeta.ir, null, {arrayLength});
		llvm::Value* size = builder->CreatePtrToInt(sizePointer, llvm::Type::getInt64Ty(*context));

		expression->expressionMeta.ir = builder->CreateCall(state->classDeclaration->classDeclarationMeta.malloc, {size});
		// TODO: Handle multidimensional arrays
	}
	else
	{
		llvm::IRBuilder<> tempBuilder(&state->function->getEntryBlock(), state->function->getEntryBlock().begin());
		auto temp = tempBuilder.CreateAlloca(dataType->dataTypeMeta.ir);

		expression->expressionMeta.ir = temp;
		expression->expressionMeta.pointer = true;
	}

	if (dataType->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(dataType);
		assert(objectType->objectTypeMeta.unit);
		assert(objectType->objectTypeMeta.unit->declaredType->declarationType == UnitDeclarationType::CLASS);

		Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(objectType->objectTypeMeta.unit->declaredType);

		Array<DataType*> parameters(expression->arguments.size());
		for (UInt32 parameterIndex = 0; parameterIndex < expression->arguments.size(); parameterIndex++)
		{
			parameters[parameterIndex] = expression->arguments[parameterIndex]->expressionMeta.dataType.get();
		}

		Ref<MethodDeclaration> constructor = classDeclaration->FindMethod(MethodType::CONSTRUCTOR, &parameters);
		if (constructor)
		{
			Array<llvm::Value*> arguments;
			arguments.push_back(expression->expressionMeta.ir);
			for (auto argument : expression->arguments)
			{
				LowerExpression(module, argument, state);
				arguments.push_back(argument->expressionMeta.Load(*builder));
			}

			LowerMethod(module, constructor, state->classDeclaration, nullptr);
			builder->CreateCall(state->classDeclaration->classDeclarationMeta.methods.at(constructor.get()), arguments);
		}
		else
		{
			STRICT_UNREACHABLE;
		}
	}
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

void LowerToIRPass::LowerOperatorExpression(llvm::Module* module, Ref<OperatorExpression> expression, LowerFunctionToIRState* state)
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
		// TODO: Call the copy constructor
		if (expression->b->expression->expressionMeta.dataType->dataTypeType == DataTypeType::REFERENCE)
		{
			b = builder->CreateLoad(b->getType()->getPointerElementType(), b);
		}
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

void LowerToIRPass::LowerTernaryExpression(llvm::Module* module, Ref<TernaryExpression> expression, LowerFunctionToIRState* state)
{
	LowerExpression(module, expression->condition, state);

	llvm::BasicBlock* entryBlock = state->currentBlock;

	llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*context, "then", state->function);
	state->currentBlock = thenBlock;
	builder->SetInsertPoint(thenBlock);
	LowerExpression(module, expression->thenExpression, state);
	llvm::Value* thenValue = expression->thenExpression->expressionMeta.Load(*builder);

	llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(*context, "else", state->function);
	state->currentBlock = elseBlock;
	builder->SetInsertPoint(elseBlock);
	LowerExpression(module, expression->elseExpression, state);
	llvm::Value* elseValue = expression->elseExpression->expressionMeta.Load(*builder);

	llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*context, "merge", state->function);

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
	phi->addIncoming(thenValue, thenBlock);
	phi->addIncoming(elseValue, elseBlock);
	expression->expressionMeta.ir = phi;
	expression->expressionMeta.pointer = false;
}

void LowerToIRPass::LowerExpression(llvm::Module* module, Ref<Expression> expression, LowerFunctionToIRState* state)
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

void LowerToIRPass::LowerBlockStatement(llvm::Module* module, Ref<BlockStatement> statement, LowerFunctionToIRState* state)
{
	for (auto subStatement : statement->statements)
	{
		LowerStatement(module, subStatement, state);
	}
}

void LowerToIRPass::LowerDeleteStatement(llvm::Module* module, Ref<DeleteStatement> statement, LowerFunctionToIRState* state)
{
	LowerExpression(module, statement->expression, state);
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

void LowerToIRPass::LowerForStatement(llvm::Module* module, Ref<ForStatement> statement, LowerFunctionToIRState* state)
{
	LowerStatement(module, statement->startStatement, state);

	llvm::BasicBlock* entryBlock = state->currentBlock;

	llvm::BasicBlock* conditionBlock = llvm::BasicBlock::Create(*context, "condition", state->function);
	state->currentBlock = conditionBlock;
	builder->SetInsertPoint(conditionBlock);
	LowerExpression(module, statement->condition, state);

	state->currentBlock = entryBlock;
	builder->SetInsertPoint(entryBlock);
	builder->CreateBr(conditionBlock);

	llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(*context, "body", state->function);
	state->currentBlock = bodyBlock;
	builder->SetInsertPoint(bodyBlock);
	LowerStatement(module, statement->bodyStatement, state);

	llvm::BasicBlock* continueBlock = llvm::BasicBlock::Create(*context, "continue", state->function);
	state->currentBlock = continueBlock;
	builder->SetInsertPoint(continueBlock);
	LowerExpression(module, statement->incrementExpression, state);
	builder->CreateBr(conditionBlock);

	llvm::BasicBlock* breakBlock = llvm::BasicBlock::Create(*context, "break", state->function);

	builder->SetInsertPoint(conditionBlock);
	builder->CreateCondBr(statement->condition->expressionMeta.ir, bodyBlock, breakBlock);

	if (!EndsWithJump(statement->bodyStatement))
	{
		builder->SetInsertPoint(bodyBlock);
		builder->CreateBr(continueBlock);
	}

	state->currentBlock = breakBlock;
	builder->SetInsertPoint(breakBlock);
}

void LowerToIRPass::LowerIfStatement(llvm::Module* module, Ref<IfStatement> statement, LowerFunctionToIRState* state)
{
	LowerExpression(module, statement->condition, state);

	llvm::BasicBlock* entryBlock = state->currentBlock;

	llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*context, "then", state->function);
	state->currentBlock = thenBlock;
	builder->SetInsertPoint(thenBlock);
	LowerStatement(module, statement->thenStatement, state);

	llvm::BasicBlock* elseBlock = nullptr;
	if (statement->elseStatement)
	{
		elseBlock = llvm::BasicBlock::Create(*context, "else", state->function);
		state->currentBlock = elseBlock;
		builder->SetInsertPoint(elseBlock);
		LowerStatement(module, statement->elseStatement, state);
	}

	llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*context, "merge", state->function);

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

void LowerToIRPass::LowerVariableDeclarationStatement(llvm::Module* module, Ref<VariableDeclarationStatement> statement, LowerFunctionToIRState* state)
{
	LowerDataType(module, statement->declaration->dataType);

	llvm::IRBuilder<> tmpBuilder(&state->function->getEntryBlock(), state->function->getEntryBlock().begin());

	auto variable = tmpBuilder.CreateAlloca(statement->declaration->dataType->dataTypeMeta.ir, nullptr, statement->declaration->name);
	statement->declaration->variableDeclarationMeta.ir = variable;

	if (statement->value)
	{
		LowerExpression(module, statement->value, state);

		builder->CreateStore(statement->value->expressionMeta.ir, variable);
	}
	else if (statement->declaration->dataType->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(statement->declaration->dataType);
		assert(objectType->objectTypeMeta.unit);
		assert(objectType->objectTypeMeta.unit->declaredType->declarationType == UnitDeclarationType::CLASS);

		Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(objectType->objectTypeMeta.unit->declaredType);

		Ref<ConstructorDeclaration> defaultConstructor = classDeclaration->GetDefaultConstructor();
		if (defaultConstructor)
		{
			LowerMethod(module, defaultConstructor, state->classDeclaration, nullptr);
			builder->CreateCall(state->classDeclaration->classDeclarationMeta.methods.at(defaultConstructor.get()), variable);
		}
	}
}

void LowerToIRPass::LowerWhileStatement(llvm::Module* module, Ref<WhileStatement> statement, LowerFunctionToIRState* state)
{
}

void LowerToIRPass::LowerReturnStatement(llvm::Module* module, Ref<ReturnStatement> statement, LowerFunctionToIRState* state)
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

void LowerToIRPass::LowerStatement(llvm::Module* module, Ref<Statement> statement, LowerFunctionToIRState* state)
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

llvm::Function* LowerToIRPass::CreateFunction(llvm::Module* module, Ref<MethodDeclaration> method)
{
	LowerDataType(module, method->dataType);

	Array<llvm::Type*> parameters;
	parameters.push_back(llvm::PointerType::get(method->methodDeclarationMeta.parent->unitDeclarationMeta.thisType->dataTypeMeta.ir, 0));
	for (auto parameter : method->parameters)
	{
		LowerDataType(module, parameter->dataType);
		parameters.push_back(parameter->dataType->dataTypeMeta.ir);
	}

	auto signature = llvm::FunctionType::get(method->dataType->dataTypeMeta.ir, parameters, false);
	return llvm::Function::Create(signature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, method->methodDeclarationMeta.name, *module);
}

PassResultFlags LowerToIRPass::LowerMethod(llvm::Module* module, Ref<MethodDeclaration> method, Ref<ClassDeclaration> classDeclaration,
                                           llvm::legacy::FunctionPassManager* fpm)
{
	auto& methods = classDeclaration->classDeclarationMeta.methods;

	if (methods.find(method.get()) == methods.end())
	{
		methods[method.get()] = CreateFunction(module, method);
	}

	if (method->body && fpm)
	{
		auto function = methods.at(method.get());

		auto block = llvm::BasicBlock::Create(*context, "entry", function);
		builder->SetInsertPoint(block);

		auto thisArgument = function->getArg(0);
#ifdef DEBUG
		thisArgument->setName("this");
#endif

		for (UInt32 parameterIndex = 0; parameterIndex < method->parameters.size(); parameterIndex++)
		{
			auto argument = function->getArg(parameterIndex + 1);
#ifdef DEBUG
			argument->setName(method->parameters[parameterIndex]->name);
#endif
			llvm::IRBuilder<> tmpBuilder(&function->getEntryBlock(), function->getEntryBlock().begin());
			auto argumentVariable = tmpBuilder.CreateAlloca(argument->getType());
#ifdef DEBUG
			argumentVariable->setName("p_" + method->parameters[parameterIndex]->name);
#endif
			builder->CreateStore(argument, argumentVariable);
			method->parameters[parameterIndex]->variableDeclarationMeta.ir = argumentVariable;
		}

		LowerFunctionToIRState state;
		state.currentBlock = block;
		state.method = method.get();
		state.classDeclaration = classDeclaration;
		state.thisPointer = thisArgument;
		state.function = function;

		LowerStatement(module, method->body, &state);

		if (method->dataType->dataTypeType == DataTypeType::PRIMITIVE)
		{
			Ref<PrimitiveType> primitiveType = std::dynamic_pointer_cast<PrimitiveType>(method->dataType);
			if (primitiveType->primitiveType == TokenType::VOID && !EndsWithJump(method->body))
			{
				builder->CreateRetVoid();
			}
		}

#ifdef DEBUG
		if (llvm::verifyFunction(*function, &llvm::outs()))
		{
			llvm::outs() << "\n";
			function->print(llvm::outs());
			llvm::outs() << "\n";

			return PassResultFlags::CRITICAL_ERROR;
		}
#endif

		fpm->run(*function);
	}

	return PassResultFlags::SUCCESS;
}

PassResultFlags LowerToIRPass::LowerClass(Ref<Module> parentModule, Ref<ClassDeclaration> classDeclaration, BuildContext& buildContext)
{
	if (classDeclaration->typeTemplate)
	{
		return PassResultFlags::SUCCESS;
	}

	auto module = AllocateUnique<llvm::Module>(classDeclaration->name, *context);

	auto mallocSignature = llvm::FunctionType::get(llvm::Type::getInt8PtrTy(*context), {llvm::Type::getInt64Ty(*context)}, false);
	classDeclaration->classDeclarationMeta.malloc =
		llvm::Function::Create(mallocSignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "malloc", *module);

	auto freeSignature = llvm::FunctionType::get(llvm::Type::getVoidTy(*context), {llvm::Type::getInt8PtrTy(*context)}, false);
	classDeclaration->classDeclarationMeta.free = llvm::Function::Create(freeSignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "free", *module);

	LowerDataType(module.get(), classDeclaration->unitDeclarationMeta.thisType);

	if (classDeclaration->isSingleton)
	{
		llvm::Constant* initializer = llvm::Constant::getNullValue(classDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir);
		classDeclaration->classDeclarationMeta.singletons[classDeclaration.get()] =
			new llvm::GlobalVariable(*module, classDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir, false,
		                             llvm::GlobalValue::LinkageTypes::ExternalLinkage, initializer, classDeclaration->name);
	}

	if ((classDeclaration->flags & DeclarationFlags::VIRTUAL) == DeclarationFlags::VIRTUAL)
	{
		classDeclaration->classDeclarationMeta.typeId = new llvm::GlobalVariable(
			*module, llvm::Type::getInt32Ty(*context), false, llvm::GlobalValue::LinkageTypes::ExternalLinkage, nullptr, classDeclaration->name + ".typeid");
	}

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
		LowerDataType(module.get(), member->dataType);

		if (member->variableType == VariableDeclarationType::METHOD)
		{
			if (LowerMethod(module.get(), std::dynamic_pointer_cast<MethodDeclaration>(member), classDeclaration, &fpm) != PassResultFlags::SUCCESS)
			{
				return PassResultFlags::CRITICAL_ERROR;
			}
		}
		else if (member->variableType == VariableDeclarationType::MEMBER_VARIABLE)
		{
			Ref<MemberVariableDeclaration> memberVariableDeclaration = std::dynamic_pointer_cast<MemberVariableDeclaration>(member);
			for (auto accessor : memberVariableDeclaration->accessors)
			{
				if (LowerMethod(module.get(), accessor, classDeclaration, &fpm) != PassResultFlags::SUCCESS)
				{
					return PassResultFlags::CRITICAL_ERROR;
				}
			}
		}
	}

#ifdef DEBUG
	if (llvm::verifyModule(*module, &llvm::outs()))
	{
		return PassResultFlags::CRITICAL_ERROR;
	}
#endif

	if (buildContext.dumpIR)
	{
		std::error_code error;
		llvm::raw_fd_ostream dumpIRStream(parentModule->moduleMeta.outputPath + "/" + classDeclaration->name + ".ll", error);
		module->print(dumpIRStream, nullptr);
	}

	classDeclaration->classDeclarationMeta.module = std::move(module);

	return PassResultFlags::SUCCESS;
}

void LowerToIRPass::InitializeSingleton(Ref<llvm::Module> entryModule, Ref<Unit> unit, llvm::IRBuilder<>& entryBuilder,
                                        HashSet<UnitDeclaration*> initializedUnits)
{
	if (unit->declaredType->declarationType != UnitDeclarationType::CLASS)
	{
		return;
	}

	Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType);
	if (!classDeclaration->isSingleton)
	{
		return;
	}

	InitializeSingleton(entryModule, classDeclaration, entryBuilder, initializedUnits);
}

void LowerToIRPass::InitializeSingleton(Ref<llvm::Module> entryModule, Ref<ClassDeclaration> classDeclaration, llvm::IRBuilder<>& entryBuilder,
                                        HashSet<UnitDeclaration*> initializedUnits)
{
	if (initializedUnits.find(classDeclaration.get()) != initializedUnits.end())
	{
		return;
	}

	initializedUnits.insert(classDeclaration.get());

	assert(classDeclaration->unitDeclarationMeta.parent);

	for (auto dependency : classDeclaration->unitDeclarationMeta.parent->unitMeta.dependencies)
	{
		InitializeSingleton(entryModule, dependency, entryBuilder, initializedUnits);
	}

	Ref<ConstructorDeclaration> defaultConstructor = classDeclaration->GetDefaultConstructor();
	if (defaultConstructor)
	{
		entryBuilder.CreateCall(CreateFunction(entryModule.get(), defaultConstructor),
		                        {new llvm::GlobalVariable(*entryModule, classDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir, false,
		                                                  llvm::GlobalValue::LinkageTypes::ExternalLinkage, nullptr, classDeclaration->name)});
	}
}

PassResultFlags LowerToIRPass::LowerModule(Ref<Module> module, BuildContext& buildContext)
{
	auto entryModule = Allocate<llvm::Module>(module->name, *context);
	module->moduleMeta.module = entryModule;

	auto entrySignature = llvm::FunctionType::get(llvm::Type::getVoidTy(*context), {}, false);
	auto entryFunction = llvm::Function::Create(entrySignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, module->name + ".EntryPoint", *entryModule);
	module->moduleMeta.entryPoint = entryFunction;

	auto entryBlock = llvm::BasicBlock::Create(*context, "entry", entryFunction);
	llvm::IRBuilder<> entryBuilder(entryBlock);

	if (module->moduleType == ModuleType::EXECUTABLE)
	{
		auto mainSignature = llvm::FunctionType::get(llvm::Type::getInt32Ty(*context), {}, false);
		auto mainFunction = llvm::Function::Create(mainSignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "main", *entryModule);
		module->moduleMeta.main = mainFunction;

		auto block = llvm::BasicBlock::Create(*context, "entry", mainFunction);
		llvm::IRBuilder<> mainBuilder(block);

		mainBuilder.CreateCall(entryFunction);
		mainBuilder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0));
	}

	for (auto& specialization : module->moduleMeta.templateSpecializations)
	{
		if (LowerClass(module, specialization.second, buildContext) != PassResultFlags::SUCCESS)
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

		if (LowerClass(module, std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType), buildContext) != PassResultFlags::SUCCESS)
		{
			return PassResultFlags::CRITICAL_ERROR;
		}
	}

	HashSet<UnitDeclaration*> initializedUnits;

	for (auto& specialization : module->moduleMeta.templateSpecializations)
	{
		if (!specialization.second->isSingleton)
		{
			continue;
		}

		InitializeSingleton(entryModule, specialization.second, entryBuilder, initializedUnits);
	}

	for (auto unit : module->units)
	{
		InitializeSingleton(entryModule, unit, entryBuilder, initializedUnits);
	}

	entryBuilder.CreateRetVoid();

	if (buildContext.dumpIR)
	{
		std::error_code error;
		llvm::raw_fd_ostream dumpIRStream(module->moduleMeta.outputPath + "/module.ll", error);
		entryModule->print(dumpIRStream, nullptr);
	}

	return PassResultFlags::SUCCESS;
}

PassResultFlags LowerToIRPass::Run(PrintFunction print, BuildContext& context)
{
	for (auto module : context.GetModules())
	{
		if (LowerModule(module, context) != PassResultFlags::SUCCESS)
		{
			return PassResultFlags::CRITICAL_ERROR;
		}
	}

	return PassResultFlags::SUCCESS;
}
