
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

void LowerToIRPass::LowerDataType(Ref<DataType> type, const LowerUnitToIRState* state)
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

		if (state->diBuilder)
		{
			switch (primitive->primitiveType)
			{
			case TokenType::VOID:
				type->dataTypeMeta.di = state->diBuilder->createUnspecifiedType("void");
				break;
			case TokenType::BOOL:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("Bool", 1, llvm::dwarf::DW_ATE_boolean);
				break;
			case TokenType::INT8:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("Int8", 8, llvm::dwarf::DW_ATE_signed);
				break;
			case TokenType::UINT8:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("UInt8", 8, llvm::dwarf::DW_ATE_unsigned);
				break;
			case TokenType::INT16:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("Int16", 16, llvm::dwarf::DW_ATE_signed);
				break;
			case TokenType::UINT16:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("UInt16", 16, llvm::dwarf::DW_ATE_unsigned);
				break;
			case TokenType::INT32:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("Int32", 32, llvm::dwarf::DW_ATE_signed);
				break;
			case TokenType::UINT32:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("UInt32", 32, llvm::dwarf::DW_ATE_unsigned);
				break;
			case TokenType::INT64:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("Int64", 64, llvm::dwarf::DW_ATE_signed);
				break;
			case TokenType::UINT64:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("UInt64", 64, llvm::dwarf::DW_ATE_unsigned);
				break;
			case TokenType::FLOAT32:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("Float32", 32, llvm::dwarf::DW_ATE_float);
				break;
			case TokenType::FLOAT64:
				type->dataTypeMeta.di =
					state->diBuilder->createBasicType("Float64", 64, llvm::dwarf::DW_ATE_float);
				break;
			default:
				STRICT_UNREACHABLE;
			}
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
			type->dataTypeMeta = unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta;
			return;
		}

		if (unitDeclaration->declarationType == UnitDeclarationType::ERROR)
		{
			unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir =
				llvm::Type::getInt32Ty(*context);

			if (state->diBuilder)
			{
				type->dataTypeMeta.di = state->diBuilder->createBasicType(
					objectType->name, 32, llvm::dwarf::DW_ATE_signed);
			}
		}
		else
		{
			Ref<ClassDeclaration> classDeclaration =
				std::dynamic_pointer_cast<ClassDeclaration>(unitDeclaration);
			Array<llvm::Type*> elements;
			for (auto superType : classDeclaration->superTypes)
			{
				LowerDataType(superType, state);
				elements.push_back(superType->dataTypeMeta.ir);
			}
			for (auto member : classDeclaration->members)
			{
				if (member->variableType != VariableDeclarationType::MEMBER_VARIABLE)
				{
					continue;
				}

				LowerDataType(member->dataType, state);
				elements.push_back(member->dataType->dataTypeMeta.ir);
			}
			unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir =
				llvm::StructType::create(*context, elements, objectType->name);

			if (state->diBuilder)
			{
				// WIP
				Unit* unit = unitDeclaration->unitDeclarationMeta.parent;
				llvm::DIFile* diFile = state->diBuilder->createFile(
					unit->name + ".strict", unit->unitMeta.parent->moduleMeta.path);
				UInt32 lineNumber =
					0; // TODO: use characterIndex and source to determine (see ErrorStream)
				llvm::DINodeArray diElements;
				unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.di =
					state->diBuilder->createClassType(diFile, objectType->name, diFile, lineNumber,
				                                      0, 0, 0, llvm::DINode::DIFlags::FlagPublic,
				                                      nullptr, diElements);
			}
		}

		type->dataTypeMeta = unitDeclaration->unitDeclarationMeta.thisType->dataTypeMeta;
	}
	else if (type->dataTypeType == DataTypeType::POINTER ||
	         type->dataTypeType == DataTypeType::REFERENCE ||
	         type->dataTypeType == DataTypeType::ARRAY)
	{
		Ref<PointerType> pointerType = std::dynamic_pointer_cast<PointerType>(type);

		LowerDataType(pointerType->value, state);
		type->dataTypeMeta.ir = llvm::PointerType::get(pointerType->value->dataTypeMeta.ir, 0);

		if (state->diBuilder)
		{
			type->dataTypeMeta.di =
				state->diBuilder->createPointerType(pointerType->value->dataTypeMeta.di, 64);
		}
	}
	else
	{
		STRICT_UNREACHABLE;
	}
}

static bool FindSuperDeclaration(Ref<TypeDeclaration> type, TypeDeclaration* declarationParent,
                                 Array<UInt32>& target)
{
	for (UInt32 index = 0; index < type->superTypes.size(); index++)
	{
		assert(type->superTypes[index]->dataTypeType == DataTypeType::OBJECT);

		Ref<ObjectType> superObject =
			std::dynamic_pointer_cast<ObjectType>(type->superTypes[index]);
		assert(superObject->objectTypeMeta.unit);
		assert(superObject->objectTypeMeta.unit->declaredType->IsType());

		Ref<TypeDeclaration> superTypeDeclaration = std::dynamic_pointer_cast<TypeDeclaration>(
			superObject->objectTypeMeta.unit->declaredType);

		if (superTypeDeclaration.get() == declarationParent ||
		    FindSuperDeclaration(superTypeDeclaration, declarationParent, target))
		{
			target.push_back(index);
			return true;
		}
	}

	return false;
}

void LowerToIRPass::LowerCallExpression(Ref<CallExpression> expression,
                                        LowerFunctionToIRState* state)
{
	assert(expression->callExpressionMeta.destination);

	if (expression->callExpressionMeta.destination->variableType == VariableDeclarationType::METHOD)
	{
		Ref<MethodDeclaration> method = std::dynamic_pointer_cast<MethodDeclaration>(
			expression->callExpressionMeta.destination);
		LowerMethod(method, nullptr, state->parent);

		Array<llvm::Value*> arguments;
		if ((method->flags & DeclarationFlags::EXTERNAL) != DeclarationFlags::EXTERNAL)
		{
			if (expression->callExpressionMeta.context)
			{
				Ref<Expression> context = expression->callExpressionMeta.context;
				LowerExpression(context, state);

				assert(context->expressionMeta.pointer);
				assert(context->expressionMeta.dataType->dataTypeType == DataTypeType::OBJECT);

				Ref<ObjectType> contextType =
					std::dynamic_pointer_cast<ObjectType>(context->expressionMeta.dataType);
				assert(contextType->objectTypeMeta.unit);
				assert(contextType->objectTypeMeta.unit->declaredType->IsType());

				Ref<TypeDeclaration> contextTypeDeclaration =
					std::dynamic_pointer_cast<TypeDeclaration>(
						contextType->objectTypeMeta.unit->declaredType);

				if (contextTypeDeclaration.get() != method->variableDeclarationMeta.parentType)
				{
					// This method must be defined by a super type
					Array<UInt32> indices;
					bool found =
						FindSuperDeclaration(contextTypeDeclaration,
					                         method->variableDeclarationMeta.parentType, indices);
					assert(found);

					llvm::Value* thisPointer = context->expressionMeta.ir;
					for (Int32 indexIndex = indices.size() - 1; indexIndex >= 0; indexIndex--)
					{
						Array<llvm::Value*> index = {
							llvm::ConstantInt::get(llvm::Type::getInt32Ty(*this->context), 0),
							llvm::ConstantInt::get(llvm::Type::getInt32Ty(*this->context),
						                           indices[indexIndex])};
						thisPointer = builder->CreateGEP(
							thisPointer->getType()->getPointerElementType(), thisPointer, index);
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
		}

		for (auto argument : expression->arguments)
		{
			LowerExpression(argument, state);
			arguments.push_back(argument->expressionMeta.Load(*builder));
		}

		if ((method->flags & DeclarationFlags::VIRTUAL) == DeclarationFlags::VIRTUAL)
		{
			expression->expressionMeta.ir =
				llvm::Constant::getNullValue(method->dataType->dataTypeMeta.ir);
		}
		else
		{
			expression->expressionMeta.ir = builder->CreateCall(
				state->parent->classDeclaration->classDeclarationMeta.methods.at(method.get()),
				arguments);
		}
	}
	else
	{
		// TODO: Implement function pointers.
		STRICT_UNIMPLEMENTED;
	}
}

llvm::Value* LowerToIRPass::GetMemberPointer(Ref<MemberVariableDeclaration> member,
                                             LowerFunctionToIRState* state)
{
	Array<llvm::Value*> index = {
		llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0),
		llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context),
	                           member->memberVariableDeclarationMeta.index)};
	return builder->CreateGEP(state->thisPointer->getType()->getPointerElementType(),
	                          state->thisPointer, index, member->name);
}

void LowerToIRPass::LowerIdentifierExpression(Ref<IdentifierExpression> expression,
                                              LowerFunctionToIRState* state)
{
	if (expression->identifierExpressionMeta.destination->type == ASTItemType::VARIABLE_DECLARATION)
	{
		Ref<VariableDeclaration> destination = std::dynamic_pointer_cast<VariableDeclaration>(
			expression->identifierExpressionMeta.destination);
		if (destination->variableType == VariableDeclarationType::VARIABLE)
		{
			expression->expressionMeta.ir = destination->variableDeclarationMeta.ir;
			expression->expressionMeta.pointer = true;
		}
		else if (destination->variableType == VariableDeclarationType::MEMBER_VARIABLE)
		{
			Ref<MemberVariableDeclaration> memberVariable =
				std::dynamic_pointer_cast<MemberVariableDeclaration>(destination);

			expression->expressionMeta.ir = GetMemberPointer(memberVariable, state);
			expression->expressionMeta.pointer = true;
		}
		else
		{
			STRICT_UNREACHABLE;
		}
	}
	else if (expression->identifierExpressionMeta.destination->type == ASTItemType::UNIT)
	{
		Ref<Unit> unit =
			std::dynamic_pointer_cast<Unit>(expression->identifierExpressionMeta.destination);
		assert(unit->declaredType->declarationType == UnitDeclarationType::CLASS);

		Ref<ClassDeclaration> classDeclaration =
			std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType);
		assert(classDeclaration->isSingleton);

		auto& singletons = state->parent->classDeclaration->classDeclarationMeta.singletons;
		if (singletons.find(classDeclaration.get()) == singletons.end())
		{
			expression->expressionMeta.ir = new llvm::GlobalVariable(
				*state->parent->module,
				classDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir, false,
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

void LowerToIRPass::LowerLiteralExpression(Ref<LiteralExpression> expression,
                                           LowerFunctionToIRState* state)
{
	assert(expression->expressionMeta.dataType);
	LowerDataType(expression->expressionMeta.dataType, state->parent);

	if (expression->expressionMeta.dataType->dataTypeType == DataTypeType::PRIMITIVE)
	{
		Ref<PrimitiveType> dataType =
			std::dynamic_pointer_cast<PrimitiveType>(expression->expressionMeta.dataType);
		switch (dataType->primitiveType)
		{
		case TokenType::BOOL: {
			expression->expressionMeta.ir = llvm::ConstantInt::getBool(
				dataType->dataTypeMeta.ir, expression->data.data.boolData);
			break;
		}
		case TokenType::INT8:
		case TokenType::INT16:
		case TokenType::INT32:
		case TokenType::INT64: {
			expression->expressionMeta.ir =
				llvm::ConstantInt::get(dataType->dataTypeMeta.ir, expression->data.data.intData);
			break;
		}
		case TokenType::UINT8:
		case TokenType::UINT16:
		case TokenType::UINT32:
		case TokenType::UINT64: {
			expression->expressionMeta.ir =
				llvm::ConstantInt::get(dataType->dataTypeMeta.ir, expression->data.data.uintData);
			break;
		}
		case TokenType::FLOAT32:
		case TokenType::FLOAT64: {
			expression->expressionMeta.ir =
				llvm::ConstantFP::get(dataType->dataTypeMeta.ir, expression->data.data.floatData);
			break;
		}
		default:
			STRICT_UNREACHABLE;
		}
	}
	else if (expression->expressionMeta.dataType->dataTypeType == DataTypeType::ARRAY)
	{
		Ref<PointerType> dataType =
			std::dynamic_pointer_cast<PointerType>(expression->expressionMeta.dataType);
		assert(dataType->value->dataTypeType == DataTypeType::PRIMITIVE);
		assert(dataType->dataTypeMeta.ir);
		assert(expression->data.type == TokenType::STRING_LITERAL);

		llvm::Constant* stringConstant =
			llvm::ConstantDataArray::getString(*context, expression->data.data.stringData);
		llvm::Value* stringGlobal = new llvm::GlobalVariable(
			*state->parent->module, stringConstant->getType(), true,
			llvm::GlobalValue::LinkageTypes::PrivateLinkage, stringConstant);

		Array<llvm::Value*> indices = {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0),
		                               llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0)};
		expression->expressionMeta.ir =
			builder->CreateInBoundsGEP(stringConstant->getType(), stringGlobal, indices);
	}
	else
	{
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerNewExpression(Ref<NewExpression> expression, LowerFunctionToIRState* state)
{
	Ref<DataType> dataType = expression->dataType;
	LowerDataType(dataType, state->parent);

	if (expression->allocationType == AllocationType::HEAP)
	{
		llvm::Value* arrayLength;
		if (dataType->dataTypeType == DataTypeType::ARRAY)
		{
			Ref<PointerType> arrayType = std::dynamic_pointer_cast<PointerType>(dataType);
			LowerExpression(arrayType->arrayLength, state);
			arrayLength = arrayType->arrayLength->expressionMeta.Load(*builder);
			dataType = arrayType->value;
		}
		else
		{
			arrayLength = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 1);
		}

		llvm::Value* null =
			llvm::ConstantPointerNull::get(llvm::PointerType::get(dataType->dataTypeMeta.ir, 0));
		llvm::Value* sizePointer =
			builder->CreateGEP(dataType->dataTypeMeta.ir, null, {arrayLength});
		llvm::Value* size = builder->CreatePtrToInt(sizePointer, llvm::Type::getInt64Ty(*context));

		expression->expressionMeta.ir = builder->CreateCall(
			state->parent->classDeclaration->classDeclarationMeta.malloc, {size});
		// TODO: Handle multidimensional arrays
	}
	else
	{
		llvm::IRBuilder<> tempBuilder(&state->function->getEntryBlock(),
		                              state->function->getEntryBlock().begin());
		auto temp = tempBuilder.CreateAlloca(dataType->dataTypeMeta.ir);

		expression->expressionMeta.ir = temp;
		expression->expressionMeta.pointer = true;
	}

	if (dataType->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(dataType);
		assert(objectType->objectTypeMeta.unit);
		assert(objectType->objectTypeMeta.unit->declaredType->declarationType ==
		       UnitDeclarationType::CLASS);

		Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(
			objectType->objectTypeMeta.unit->declaredType);

		Array<DataType*> parameters(expression->arguments.size());
		for (UInt32 parameterIndex = 0; parameterIndex < expression->arguments.size();
		     parameterIndex++)
		{
			parameters[parameterIndex] =
				expression->arguments[parameterIndex]->expressionMeta.dataType.get();
		}

		Ref<MethodDeclaration> constructor =
			classDeclaration->FindMethod(MethodType::CONSTRUCTOR, &parameters);
		if (constructor)
		{
			Array<llvm::Value*> arguments;
			arguments.push_back(expression->expressionMeta.ir);
			for (auto argument : expression->arguments)
			{
				LowerExpression(argument, state);
				arguments.push_back(argument->expressionMeta.Load(*builder));
			}

			LowerMethod(constructor, nullptr, state->parent);
			builder->CreateCall(
				state->parent->classDeclaration->classDeclarationMeta.methods.at(constructor.get()),
				arguments);
		}
		else
		{
			STRICT_UNREACHABLE;
		}
	}
}

llvm::Value* LowerToIRPass::LowerIntOperator(OperatorType type, llvm::Value* a, llvm::Value* b,
                                             bool isSigned)
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
	case OperatorType::EXPLICIT_CAST:
		return a;
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
	case OperatorType::EXPLICIT_CAST:
		return a;
	default:
		STRICT_UNREACHABLE;
	}
}

llvm::Value* LowerToIRPass::LowerPrimitiveCast(llvm::Value* a, Ref<PrimitiveType> sourceType,
                                               Ref<PrimitiveType> primitiveType)
{
	switch (primitiveType->primitiveType)
	{
	case TokenType::INT8:
	case TokenType::INT16:
	case TokenType::INT32:
	case TokenType::INT64: {
		if (sourceType->IsFloat())
		{
			return builder->CreateCast(llvm::Instruction::CastOps::FPToSI, a,
			                           primitiveType->dataTypeMeta.ir);
		}

		if (sourceType->GetSize() < primitiveType->GetSize())
		{
			return builder->CreateCast(llvm::Instruction::CastOps::SExt, a,
			                           primitiveType->dataTypeMeta.ir);
		}
		else
		{
			return builder->CreateCast(llvm::Instruction::CastOps::Trunc, a,
			                           primitiveType->dataTypeMeta.ir);
		}
	}
	case TokenType::UINT8:
	case TokenType::UINT16:
	case TokenType::UINT32:
	case TokenType::UINT64: {
		if (sourceType->IsFloat())
		{
			return builder->CreateCast(llvm::Instruction::CastOps::FPToUI, a,
			                           primitiveType->dataTypeMeta.ir);
		}

		if (sourceType->GetSize() < primitiveType->GetSize())
		{
			return builder->CreateCast(llvm::Instruction::CastOps::ZExt, a,
			                           primitiveType->dataTypeMeta.ir);
		}
		else
		{
			return builder->CreateCast(llvm::Instruction::CastOps::Trunc, a,
			                           primitiveType->dataTypeMeta.ir);
		}
	}
	case TokenType::FLOAT32:
	case TokenType::FLOAT64: {
		if (!sourceType->IsFloat())
		{
			if (sourceType->IsSigned())
			{
				return builder->CreateCast(llvm::Instruction::CastOps::SIToFP, a,
				                           primitiveType->dataTypeMeta.ir);
			}
			else
			{
				return builder->CreateCast(llvm::Instruction::CastOps::UIToFP, a,
				                           primitiveType->dataTypeMeta.ir);
			}
		}

		if (sourceType->GetSize() < primitiveType->GetSize())
		{
			return builder->CreateCast(llvm::Instruction::CastOps::FPExt, a,
			                           primitiveType->dataTypeMeta.ir);
		}
		else
		{
			return builder->CreateCast(llvm::Instruction::CastOps::FPTrunc, a,
			                           primitiveType->dataTypeMeta.ir);
		}
	}
	default:
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerOperatorExpression(Ref<OperatorExpression> expression,
                                            LowerFunctionToIRState* state)
{
	LowerExpression(expression->a, state);

	bool keepPointer = (expression->operatorType == OperatorType::ACCESS) ||
	                   (expression->operatorType == OperatorType::ASSIGN);

	llvm::Value* a = keepPointer ? expression->a->expressionMeta.ir
	                             : expression->a->expressionMeta.Load(*builder);
	llvm::Value* b = nullptr;

	if (expression->b && expression->b->expression)
	{
		LowerExpression(expression->b->expression, state);
		b = expression->b->expression->expressionMeta.Load(*builder);
	}

	if (expression->operatorType == OperatorType::ASSIGN)
	{
		// TODO: Call the copy constructor
		if (expression->b->expression->expressionMeta.dataType->dataTypeType ==
		    DataTypeType::REFERENCE)
		{
			b = builder->CreateLoad(b->getType()->getPointerElementType(), b);
		}
		builder->CreateStore(b, a);
		expression->expressionMeta = expression->b->expression->expressionMeta;
		return;
	}
	else if (expression->operatorType == OperatorType::ARRAY_ACCESS)
	{
		expression->expressionMeta.ir =
			builder->CreateGEP(a->getType()->getPointerElementType(), a, b);
		expression->expressionMeta.pointer = true;
		return;
	}

	if (expression->expressionMeta.dataType->dataTypeType == DataTypeType::PRIMITIVE)
	{
		Ref<PrimitiveType> primitiveType =
			std::dynamic_pointer_cast<PrimitiveType>(expression->expressionMeta.dataType);
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

		if (expression->operatorType == OperatorType::EXPLICIT_CAST)
		{
			assert(expression->b->dataType);
			assert(expression->b->dataType->dataTypeType == DataTypeType::PRIMITIVE);

			LowerDataType(primitiveType, state->parent);
			expression->expressionMeta.ir = LowerPrimitiveCast(
				a, std::dynamic_pointer_cast<PrimitiveType>(expression->a->expressionMeta.dataType),
				std::dynamic_pointer_cast<PrimitiveType>(expression->b->dataType));
		}
	}
	else if (expression->expressionMeta.dataType->IsPointer())
	{
		if (expression->operatorType == OperatorType::EXPLICIT_CAST)
		{
			STRICT_UNIMPLEMENTED;
		}
	}
	else
	{
		// At this point, operators on object types should be lowered to call expressions.
		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerTernaryExpression(Ref<TernaryExpression> expression,
                                           LowerFunctionToIRState* state)
{
	LowerExpression(expression->condition, state);

	llvm::BasicBlock* entryBlock = state->currentBlock;

	llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*context, "then", state->function);
	state->currentBlock = thenBlock;
	builder->SetInsertPoint(thenBlock);
	LowerExpression(expression->thenExpression, state);
	llvm::Value* thenValue = expression->thenExpression->expressionMeta.Load(*builder);

	llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(*context, "else", state->function);
	state->currentBlock = elseBlock;
	builder->SetInsertPoint(elseBlock);
	LowerExpression(expression->elseExpression, state);
	llvm::Value* elseValue = expression->elseExpression->expressionMeta.Load(*builder);

	llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*context, "merge", state->function);

	builder->SetInsertPoint(entryBlock);
	builder->CreateCondBr(expression->condition->expressionMeta.Load(*builder), thenBlock,
	                      elseBlock ? elseBlock : mergeBlock);

	builder->SetInsertPoint(thenBlock);
	builder->CreateBr(mergeBlock);

	builder->SetInsertPoint(elseBlock);
	builder->CreateBr(mergeBlock);

	state->currentBlock = mergeBlock;
	builder->SetInsertPoint(mergeBlock);

	LowerDataType(expression->expressionMeta.dataType, state->parent);
	auto phi = builder->CreatePHI(expression->expressionMeta.dataType->dataTypeMeta.ir, 2);
	phi->addIncoming(thenValue, thenBlock);
	phi->addIncoming(elseValue, elseBlock);
	expression->expressionMeta.ir = phi;
	expression->expressionMeta.pointer = false;
}

void LowerToIRPass::LowerExpression(Ref<Expression> expression, LowerFunctionToIRState* state)
{
	switch (expression->expressionType)
	{
	case ExpressionType::BRACKET:
		LowerExpression(std::dynamic_pointer_cast<BracketExpression>(expression)->expression,
		                state);
		break;
	case ExpressionType::CALL:
		LowerCallExpression(std::dynamic_pointer_cast<CallExpression>(expression), state);
		break;
	case ExpressionType::IDENTIFIER:
		LowerIdentifierExpression(std::dynamic_pointer_cast<IdentifierExpression>(expression),
		                          state);
		break;
	case ExpressionType::LITERAL:
		LowerLiteralExpression(std::dynamic_pointer_cast<LiteralExpression>(expression), state);
		break;
	case ExpressionType::NEW:
		LowerNewExpression(std::dynamic_pointer_cast<NewExpression>(expression), state);
		break;
	case ExpressionType::OPERATOR:
		LowerOperatorExpression(std::dynamic_pointer_cast<OperatorExpression>(expression), state);
		break;
	case ExpressionType::TERNARY:
		LowerTernaryExpression(std::dynamic_pointer_cast<TernaryExpression>(expression), state);
		break;
	}
}

void LowerToIRPass::LowerBlockStatement(Ref<BlockStatement> statement,
                                        LowerFunctionToIRState* state)
{
	for (auto subStatement : statement->statements)
	{
		LowerStatement(subStatement, state);
	}
}

void LowerToIRPass::LowerDeleteStatement(Ref<DeleteStatement> statement,
                                         LowerFunctionToIRState* state)
{
	LowerExpression(statement->expression, state);
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

void LowerToIRPass::LowerForStatement(Ref<ForStatement> statement, LowerFunctionToIRState* state)
{
	LowerStatement(statement->startStatement, state);

	llvm::BasicBlock* entryBlock = state->currentBlock;

	llvm::BasicBlock* conditionBlock =
		llvm::BasicBlock::Create(*context, "condition", state->function);
	state->currentBlock = conditionBlock;
	builder->SetInsertPoint(conditionBlock);
	LowerExpression(statement->condition, state);

	state->currentBlock = entryBlock;
	builder->SetInsertPoint(entryBlock);
	builder->CreateBr(conditionBlock);

	llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(*context, "body", state->function);
	state->currentBlock = bodyBlock;
	builder->SetInsertPoint(bodyBlock);
	LowerStatement(statement->bodyStatement, state);

	llvm::BasicBlock* continueBlock =
		llvm::BasicBlock::Create(*context, "continue", state->function);
	state->currentBlock = continueBlock;
	builder->SetInsertPoint(continueBlock);
	LowerExpression(statement->incrementExpression, state);
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

void LowerToIRPass::LowerIfStatement(Ref<IfStatement> statement, LowerFunctionToIRState* state)
{
	LowerExpression(statement->condition, state);

	llvm::BasicBlock* entryBlock = state->currentBlock;

	llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*context, "then", state->function);
	llvm::BasicBlock* thenBlock2;
	state->currentBlock = thenBlock;
	builder->SetInsertPoint(thenBlock);
	LowerStatement(statement->thenStatement, state);
	thenBlock2 = state->currentBlock;

	llvm::BasicBlock* elseBlock = nullptr;
	llvm::BasicBlock* elseBlock2;
	if (statement->elseStatement)
	{
		elseBlock = llvm::BasicBlock::Create(*context, "else", state->function);
		state->currentBlock = elseBlock;
		builder->SetInsertPoint(elseBlock);
		LowerStatement(statement->elseStatement, state);
		elseBlock2 = state->currentBlock;
	}

	llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*context, "merge", state->function);

	builder->SetInsertPoint(entryBlock);
	builder->CreateCondBr(statement->condition->expressionMeta.ir, thenBlock,
	                      elseBlock ? elseBlock : mergeBlock);

	if (!EndsWithJump(statement->thenStatement))
	{
		builder->SetInsertPoint(thenBlock2);
		builder->CreateBr(mergeBlock);
	}

	if (statement->elseStatement && !EndsWithJump(statement->elseStatement))
	{
		builder->SetInsertPoint(elseBlock2);
		builder->CreateBr(mergeBlock);
	}

	state->currentBlock = mergeBlock;
	builder->SetInsertPoint(mergeBlock);
}

void LowerToIRPass::LowerVariableDeclarationStatement(Ref<VariableDeclarationStatement> statement,
                                                      LowerFunctionToIRState* state)
{
	LowerDataType(statement->declaration->dataType, state->parent);

	llvm::IRBuilder<> tmpBuilder(&state->function->getEntryBlock(),
	                             state->function->getEntryBlock().begin());

	auto variable = tmpBuilder.CreateAlloca(statement->declaration->dataType->dataTypeMeta.ir,
	                                        nullptr, statement->declaration->name);
	statement->declaration->variableDeclarationMeta.ir = variable;

	if (statement->value)
	{
		LowerExpression(statement->value, state);

		builder->CreateStore(statement->value->expressionMeta.ir, variable);
	}
	else if (statement->declaration->dataType->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType =
			std::dynamic_pointer_cast<ObjectType>(statement->declaration->dataType);
		assert(objectType->objectTypeMeta.unit);
		assert(objectType->objectTypeMeta.unit->declaredType->declarationType ==
		       UnitDeclarationType::CLASS);

		Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(
			objectType->objectTypeMeta.unit->declaredType);

		Ref<ConstructorDeclaration> defaultConstructor = classDeclaration->GetDefaultConstructor();
		if (defaultConstructor)
		{
			LowerMethod(defaultConstructor, nullptr, state->parent);
			builder->CreateCall(state->parent->classDeclaration->classDeclarationMeta.methods.at(
									defaultConstructor.get()),
			                    variable);
		}
	}
}

void LowerToIRPass::LowerWhileStatement(Ref<WhileStatement> statement,
                                        LowerFunctionToIRState* state)
{
	STRICT_UNIMPLEMENTED;
}

void LowerToIRPass::LowerReturnStatement(Ref<ReturnStatement> statement,
                                         LowerFunctionToIRState* state)
{
	LowerExpression(statement->expression, state);
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

void LowerToIRPass::LowerStatement(Ref<Statement> statement, LowerFunctionToIRState* state)
{
	if (state->parent->diBuilder)
	{
		assert(statement->characterIndex != CHARACTER_INDEX_NONE);
		UInt32 lineNumber = GetLineNumber(
			state->parent->classDeclaration->unitDeclarationMeta.parent->unitMeta.lexer.GetSource(),
			statement->characterIndex, 1);

		// TODO: Calculate the character index.
		builder->SetCurrentDebugLocation(
			llvm::DILocation::get(*context, lineNumber, 0, state->diFunction));
	}

	switch (statement->statementType)
	{
	case StatementType::BLOCK:
		LowerBlockStatement(std::dynamic_pointer_cast<BlockStatement>(statement), state);
		break;
	case StatementType::BREAK:
		builder->CreateBr(state->breakBlock);
		break;
	case StatementType::CONTINUE:
		builder->CreateBr(state->continueBlock);
		break;
	case StatementType::DELETE:
		LowerDeleteStatement(std::dynamic_pointer_cast<DeleteStatement>(statement), state);
		break;
	case StatementType::EXPRESSION:
		LowerExpression(std::dynamic_pointer_cast<ExpressionStatement>(statement)->expression,
		                state);
		break;
	case StatementType::FOR:
		LowerForStatement(std::dynamic_pointer_cast<ForStatement>(statement), state);
		break;
	case StatementType::IF:
		LowerIfStatement(std::dynamic_pointer_cast<IfStatement>(statement), state);
		break;
	case StatementType::RETURN:
		LowerReturnStatement(std::dynamic_pointer_cast<ReturnStatement>(statement), state);
		break;
	case StatementType::VARIABLE_DECLARATION:
		LowerVariableDeclarationStatement(
			std::dynamic_pointer_cast<VariableDeclarationStatement>(statement), state);
		break;
	case StatementType::WHILE:
		LowerWhileStatement(std::dynamic_pointer_cast<WhileStatement>(statement), state);
		break;
	}
}

llvm::Function* LowerToIRPass::CreateFunction(Ref<MethodDeclaration> method,
                                              const LowerUnitToIRState* state)
{
	LowerDataType(method->dataType, state);

	Array<llvm::Type*> parameters;

	if ((method->flags & DeclarationFlags::EXTERNAL) != DeclarationFlags::EXTERNAL)
	{
		parameters.push_back(llvm::PointerType::get(
			method->methodDeclarationMeta.parent->unitDeclarationMeta.thisType->dataTypeMeta.ir,
			0));
	}

	for (auto parameter : method->parameters)
	{
		LowerDataType(parameter->dataType, state);
		parameters.push_back(parameter->dataType->dataTypeMeta.ir);
	}

	auto signature = llvm::FunctionType::get(method->dataType->dataTypeMeta.ir, parameters, false);
	return llvm::Function::Create(signature, llvm::GlobalValue::LinkageTypes::ExternalLinkage,
	                              method->methodDeclarationMeta.name, *state->module);
}

PassResultFlags LowerToIRPass::LowerMethod(Ref<MethodDeclaration> method,
                                           llvm::legacy::FunctionPassManager* fpm,
                                           const LowerUnitToIRState* state)
{
	auto& methods = state->classDeclaration->classDeclarationMeta.methods;

	if (methods.find(method.get()) == methods.end())
	{
		methods[method.get()] = CreateFunction(method, state);
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

		LowerFunctionToIRState functionState;
		functionState.parent = state;
		functionState.currentBlock = block;
		functionState.method = method.get();
		functionState.thisPointer = thisArgument;
		functionState.function = function;

		UInt32 lineNumber;
		if (state->diBuilder)
		{
			Array<llvm::Metadata*> diParameters;
			diParameters.push_back(method->dataType->dataTypeMeta.di);

			for (auto parameter : method->parameters)
			{
				diParameters.push_back(parameter->dataType->dataTypeMeta.di);
			}

			auto diFunctionType = state->diBuilder->createSubroutineType(
				state->diBuilder->getOrCreateTypeArray(diParameters));

			assert(method->characterIndex != CHARACTER_INDEX_NONE);
			lineNumber = GetLineNumber(
				state->classDeclaration->unitDeclarationMeta.parent->unitMeta.lexer.GetSource(),
				method->characterIndex, 1);

			auto diFunction = state->diBuilder->createFunction(
				state->diFile, method->name, llvm::StringRef(), state->diFile, lineNumber,
				diFunctionType, lineNumber, llvm::DINode::FlagPrototyped,
				llvm::DISubprogram::SPFlagDefinition);

			function->setSubprogram(diFunction);
			functionState.diFunction = diFunction;

			builder->SetCurrentDebugLocation(
				llvm::DILocation::get(*context, lineNumber, 0, diFunction));
		}

		for (UInt32 parameterIndex = 0; parameterIndex < method->parameters.size();
		     parameterIndex++)
		{
			const String& argumentName = method->parameters[parameterIndex]->name;

			auto argument = function->getArg(parameterIndex + 1);
#ifdef DEBUG
			argument->setName(argumentName);
#endif
			llvm::IRBuilder<> tmpBuilder(&function->getEntryBlock(),
			                             function->getEntryBlock().begin());
			auto argumentVariable = tmpBuilder.CreateAlloca(argument->getType());

			if (state->diBuilder)
			{
				llvm::DILocalVariable* diArgument = state->diBuilder->createParameterVariable(
					functionState.diFunction, argumentName, parameterIndex + 1, state->diFile,
					lineNumber, method->parameters[parameterIndex]->dataType->dataTypeMeta.di,
					true);

				state->diBuilder->insertDeclare(
					argumentVariable, diArgument, state->diBuilder->createExpression(),
					llvm::DILocation::get(*context, lineNumber, 0, functionState.diFunction),
					tmpBuilder.GetInsertBlock());
			}

#ifdef DEBUG
			argumentVariable->setName("p_" + argumentName);
#endif
			builder->CreateStore(argument, argumentVariable);
			method->parameters[parameterIndex]->variableDeclarationMeta.ir = argumentVariable;
		}

		if (method->methodType == MethodType::CONSTRUCTOR)
		{
			for (auto member : state->classDeclaration->members)
			{
				if (member->variableType != VariableDeclarationType::MEMBER_VARIABLE)
				{
					continue;
				}

				Ref<MemberVariableDeclaration> memberVariable =
					std::dynamic_pointer_cast<MemberVariableDeclaration>(member);

				auto memberPointer = GetMemberPointer(memberVariable, &functionState);

				llvm::Constant* initializer =
					llvm::Constant::getNullValue(memberVariable->dataType->dataTypeMeta.ir);
				builder->CreateStore(initializer, memberPointer);
			}
		}

		LowerStatement(method->body, &functionState);

		if (method->dataType->dataTypeType == DataTypeType::PRIMITIVE)
		{
			Ref<PrimitiveType> primitiveType =
				std::dynamic_pointer_cast<PrimitiveType>(method->dataType);
			if (primitiveType->primitiveType == TokenType::VOID && !EndsWithJump(method->body))
			{
				builder->CreateRetVoid();
			}
		}

		fpm->run(*function);
	}

	return PassResultFlags::SUCCESS;
}

PassResultFlags LowerToIRPass::LowerClass(Ref<Module> parentModule,
                                          Ref<ClassDeclaration> classDeclaration,
                                          BuildContext& buildContext)
{
	if (classDeclaration->typeTemplate)
	{
		return PassResultFlags::SUCCESS;
	}

	auto module = AllocateUnique<llvm::Module>(classDeclaration->name, *context);

	llvm::DIBuilder* diBuilder = nullptr;
	llvm::DIFile* diFile = nullptr;
	llvm::DICompileUnit* diCU = nullptr;
	if (buildContext.optimizationLevel == OptimizationLevel::DEBUGGING)
	{
		diBuilder = new llvm::DIBuilder(*module);

		Unit* unit =
			classDeclaration->classDeclarationMeta.sourceClass
				? classDeclaration->classDeclarationMeta.sourceClass->unitDeclarationMeta.parent
				: classDeclaration->unitDeclarationMeta.parent;
		diFile =
			diBuilder->createFile(unit->name + ".strict", unit->unitMeta.parent->moduleMeta.path);

		diCU = diBuilder->createCompileUnit(llvm::dwarf::DW_LANG_C, diFile, "Strict LLVM Compiler",
		                                    false, "", 0);
	}

	LowerUnitToIRState state;
	state.classDeclaration = classDeclaration;
	state.module = module.get();
	state.diBuilder = diBuilder;
	state.diFile = diFile;
	state.diCU = diCU;

	auto mallocSignature = llvm::FunctionType::get(llvm::Type::getInt8PtrTy(*context),
	                                               {llvm::Type::getInt64Ty(*context)}, false);
	classDeclaration->classDeclarationMeta.malloc = llvm::Function::Create(
		mallocSignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "malloc", *module);

	auto freeSignature = llvm::FunctionType::get(llvm::Type::getVoidTy(*context),
	                                             {llvm::Type::getInt8PtrTy(*context)}, false);
	classDeclaration->classDeclarationMeta.free = llvm::Function::Create(
		freeSignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "free", *module);

	LowerDataType(classDeclaration->unitDeclarationMeta.thisType, &state);

	if (classDeclaration->isSingleton)
	{
		classDeclaration->classDeclarationMeta.singletons[classDeclaration.get()] =
			new llvm::GlobalVariable(
				*module, classDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir, false,
				llvm::GlobalValue::LinkageTypes::ExternalLinkage, nullptr, classDeclaration->name);
	}

	if ((classDeclaration->flags & DeclarationFlags::VIRTUAL) == DeclarationFlags::VIRTUAL)
	{
		classDeclaration->classDeclarationMeta.typeId =
			new llvm::GlobalVariable(*module, llvm::Type::getInt32Ty(*context), false,
		                             llvm::GlobalValue::LinkageTypes::ExternalLinkage, nullptr,
		                             classDeclaration->name + ".typeid");
	}

	// TODO: Swith to the new pass manager once llvm 15 is released
	llvm::legacy::FunctionPassManager fpm = llvm::legacy::FunctionPassManager(module.get());
	if (buildContext.optimizationLevel != OptimizationLevel::DEBUGGING)
	{
		fpm.add(llvm::createPromoteMemoryToRegisterPass());
		fpm.add(llvm::createInstructionCombiningPass());
		fpm.add(llvm::createReassociatePass());
		fpm.add(llvm::createGVNPass());
		fpm.add(llvm::createCFGSimplificationPass());
	}
	fpm.doInitialization();

	for (auto member : classDeclaration->members)
	{
		LowerDataType(member->dataType, &state);

		if (member->variableType == VariableDeclarationType::METHOD)
		{
			if (LowerMethod(std::dynamic_pointer_cast<MethodDeclaration>(member), &fpm, &state) !=
			    PassResultFlags::SUCCESS)
			{
				return PassResultFlags::CRITICAL_ERROR;
			}
		}
		else if (member->variableType == VariableDeclarationType::MEMBER_VARIABLE)
		{
			Ref<MemberVariableDeclaration> memberVariableDeclaration =
				std::dynamic_pointer_cast<MemberVariableDeclaration>(member);
			for (auto accessor : memberVariableDeclaration->accessors)
			{
				if (LowerMethod(accessor, &fpm, &state) != PassResultFlags::SUCCESS)
				{
					return PassResultFlags::CRITICAL_ERROR;
				}
			}
		}
	}

	if (diBuilder)
	{
		diBuilder->finalize();
	}

#ifdef DEBUG
	if (llvm::verifyModule(*module, &llvm::outs()))
	{
		std::cout << std::endl;
		return PassResultFlags::CRITICAL_ERROR;
	}
#endif

	if (buildContext.dumpIR)
	{
		std::error_code error;
		llvm::raw_fd_ostream dumpIRStream(
			parentModule->moduleMeta.outputPath + "/" + classDeclaration->name + ".ll", error);
		module->print(dumpIRStream, nullptr);
	}

	classDeclaration->classDeclarationMeta.module = std::move(module);

	return PassResultFlags::SUCCESS;
}

void LowerToIRPass::InitializeSingleton(Ref<llvm::Module> entryModule, Ref<Unit> unit,
                                        llvm::IRBuilder<>& entryBuilder,
                                        HashSet<UnitDeclaration*> initializedUnits)
{
	if (unit->declaredType->declarationType != UnitDeclarationType::CLASS)
	{
		return;
	}

	Ref<ClassDeclaration> classDeclaration =
		std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType);
	if (!classDeclaration->isSingleton)
	{
		return;
	}

	InitializeSingleton(entryModule, classDeclaration, entryBuilder, initializedUnits);
}

void LowerToIRPass::InitializeSingleton(Ref<llvm::Module> entryModule,
                                        Ref<ClassDeclaration> classDeclaration,
                                        llvm::IRBuilder<>& entryBuilder,
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
		if (dependency->unitMeta.parent !=
		    classDeclaration->unitDeclarationMeta.parent->unitMeta.parent)
		{
			continue;
		}

		InitializeSingleton(entryModule, dependency, entryBuilder, initializedUnits);
	}

	llvm::Constant* initializer = llvm::Constant::getNullValue(
		classDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir);
	llvm::Value* instance = new llvm::GlobalVariable(
		*entryModule, classDeclaration->unitDeclarationMeta.thisType->dataTypeMeta.ir, false,
		llvm::GlobalValue::LinkageTypes::ExternalLinkage, initializer, classDeclaration->name);

	Ref<ConstructorDeclaration> defaultConstructor = classDeclaration->GetDefaultConstructor();
	if (defaultConstructor)
	{
		LowerUnitToIRState state;
		state.module = entryModule.get();
		entryBuilder.CreateCall(CreateFunction(defaultConstructor, &state), {instance});
	}
}

PassResultFlags LowerToIRPass::LowerModule(Ref<Module> module, BuildContext& buildContext)
{
	auto entryModule = Allocate<llvm::Module>(module->name, *context);
	module->moduleMeta.module = entryModule;

	auto entrySignature = llvm::FunctionType::get(llvm::Type::getVoidTy(*context), {}, false);
	auto entryFunction =
		llvm::Function::Create(entrySignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage,
	                           module->name + ".EntryPoint", *entryModule);
	module->moduleMeta.entryPoint = entryFunction;

	auto entryBlock = llvm::BasicBlock::Create(*context, "entry", entryFunction);
	llvm::IRBuilder<> entryBuilder(entryBlock);

	if (module->moduleType == ModuleType::EXECUTABLE)
	{
		auto mainSignature = llvm::FunctionType::get(llvm::Type::getInt32Ty(*context), {}, false);
		auto mainFunction = llvm::Function::Create(
			mainSignature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "main", *entryModule);
		module->moduleMeta.main = mainFunction;

		auto block = llvm::BasicBlock::Create(*context, "entry", mainFunction);
		llvm::IRBuilder<> mainBuilder(block);

		mainBuilder.CreateCall(entryFunction);
		mainBuilder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0));
	}

	for (auto unit : module->units)
	{
		if (unit->declaredType->declarationType != UnitDeclarationType::CLASS)
		{
			continue;
		}

		if (LowerClass(module, std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType),
		               buildContext) != PassResultFlags::SUCCESS)
		{
			return PassResultFlags::CRITICAL_ERROR;
		}
	}

	HashSet<UnitDeclaration*> initializedUnits;

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
