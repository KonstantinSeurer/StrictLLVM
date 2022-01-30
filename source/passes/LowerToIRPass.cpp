
#include "LowerToIRPass.h"

LowerToIRPass::LowerToIRPass()
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

void LowerToIRPass::LowerExpression(Ref<llvm::Module> module, Ref<Expression> expression)
{
}

void LowerToIRPass::LowerDeleteStatement(Ref<llvm::Module> module, Ref<DeleteStatement> statement, LowerFunctionToIRState* state)
{
}

void LowerToIRPass::LowerForStatement(Ref<llvm::Module> module, Ref<ForStatement> statement, LowerFunctionToIRState* state)
{
}

void LowerToIRPass::LowerIfStatement(Ref<llvm::Module> module, Ref<IfStatement> statement, LowerFunctionToIRState* state)
{
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
		LowerExpression(module, expressionStatement->expression);
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
		LowerExpression(module, returnStatement->expression);
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

	if (method->body)
	{
		auto block = llvm::BasicBlock::Create(*context, "entry", method->methodDeclarationMeta.ir);
		builder->SetInsertPoint(block);

		LowerFunctionToIRState state;
		state.currentBlock = block;

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
