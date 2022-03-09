
#include "GatherInformationPass.h"

#include <iostream>

void GatherInformationPass::TryToInsertTemplatedObjectType(HashMap<ObjectType, Array<ObjectType*>>& target, DataType& dataType)
{
	if ((flags & GatherInformationFlags::USED_TEMPLATES) != GatherInformationFlags::USED_TEMPLATES)
	{
		return;
	}

	if (dataType.dataTypeType == DataTypeType::OBJECT)
	{
		ObjectType& objectType = (ObjectType&)dataType;

		if (objectType.typeTemplate)
		{
			if (target.find(objectType) == target.end())
			{
				target[objectType] = {&objectType};
			}
			else
			{
				target.at(objectType).push_back(&objectType);
			}
		}

		return;
	}

	if (dataType.IsPointer())
	{
		TryToInsertTemplatedObjectType(target, *((PointerType&)dataType).value);
	}
}

void GatherInformationPass::GatherInformation(Ref<TypeDeclaration> type, Ref<Expression> expression, Statement* parentStatement)
{
	if (!expression)
	{
		return;
	}

	if ((flags & GatherInformationFlags::PARENT) == GatherInformationFlags::PARENT)
	{
		expression->expressionMeta.parentStatement = parentStatement;
	}

	switch (expression->expressionType)
	{
	case ExpressionType::BRACKET: {
		Ref<BracketExpression> bracketExpression = std::dynamic_pointer_cast<BracketExpression>(expression);
		GatherInformation(type, bracketExpression->expression, parentStatement);
		if ((flags & GatherInformationFlags::PARENT) == GatherInformationFlags::PARENT)
		{
			bracketExpression->expression->expressionMeta.parentExpression = bracketExpression.get();
		}
		break;
	}
	case ExpressionType::CALL: {
		Ref<CallExpression> callExpression = std::dynamic_pointer_cast<CallExpression>(expression);
		GatherInformation(type, callExpression->method, parentStatement);
		for (auto argument : callExpression->arguments)
		{
			GatherInformation(type, argument, parentStatement);
		}
		if ((flags & GatherInformationFlags::PARENT) == GatherInformationFlags::PARENT)
		{
			callExpression->method->expressionMeta.parentExpression = callExpression.get();
			for (auto argument : callExpression->arguments)
			{
				argument->expressionMeta.parentExpression = callExpression.get();
			}
		}
		break;
	}
	case ExpressionType::NEW: {
		Ref<NewExpression> newExpression = std::dynamic_pointer_cast<NewExpression>(expression);
		TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *newExpression->dataType);
		for (auto argument : newExpression->arguments)
		{
			GatherInformation(type, argument, parentStatement);
		}
		if ((flags & GatherInformationFlags::PARENT) == GatherInformationFlags::PARENT)
		{
			for (auto argument : newExpression->arguments)
			{
				argument->expressionMeta.parentExpression = newExpression.get();
			}
		}
		break;
	}
	case ExpressionType::OPERATOR: {
		Ref<OperatorExpression> operatorExpression = std::dynamic_pointer_cast<OperatorExpression>(expression);

		GatherInformation(type, operatorExpression->a, parentStatement);
		operatorExpression->a->expressionMeta.parentExpression = operatorExpression.get();

		if (operatorExpression->b)
		{
			if (operatorExpression->b->dataType)
			{
				TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *operatorExpression->b->dataType);
			}
			if (operatorExpression->b->expression)
			{
				GatherInformation(type, operatorExpression->b->expression, parentStatement);
				operatorExpression->b->expression->expressionMeta.parentExpression = operatorExpression.get();
			}
		}
		break;
	}
	case ExpressionType::TERNARY: {
		Ref<TernaryExpression> ternaryExpression = std::dynamic_pointer_cast<TernaryExpression>(expression);

		GatherInformation(type, ternaryExpression->condition, parentStatement);
		ternaryExpression->condition->expressionMeta.parentExpression = ternaryExpression.get();

		GatherInformation(type, ternaryExpression->thenExpression, parentStatement);
		ternaryExpression->thenExpression->expressionMeta.parentExpression = ternaryExpression.get();

		GatherInformation(type, ternaryExpression->elseExpression, parentStatement);
		ternaryExpression->elseExpression->expressionMeta.parentExpression = ternaryExpression.get();
		break;
	}
	default:
		break;
	}
}

void GatherInformationPass::GatherInformation(Ref<TypeDeclaration> type, Ref<Statement> statement, Statement* parent)
{
	if (!statement)
	{
		return;
	}

	if ((flags & GatherInformationFlags::PARENT) == GatherInformationFlags::PARENT)
	{
		statement->statementMeta.parent = parent;
	}

	switch (statement->statementType)
	{
	case StatementType::BLOCK: {
		Ref<BlockStatement> block = std::dynamic_pointer_cast<BlockStatement>(statement);
		for (auto subStatement : block->statements)
		{
			GatherInformation(type, subStatement, block.get());
		}
		break;
	}
	case StatementType::DELETE: {
		Ref<DeleteStatement> deleteStatement = std::dynamic_pointer_cast<DeleteStatement>(statement);
		GatherInformation(type, deleteStatement->expression, statement.get());
		break;
	}
	case StatementType::EXPRESSION: {
		Ref<ExpressionStatement> deleteStatement = std::dynamic_pointer_cast<ExpressionStatement>(statement);
		GatherInformation(type, deleteStatement->expression, statement.get());
		break;
	}
	case StatementType::FOR: {
		Ref<ForStatement> forStatement = std::dynamic_pointer_cast<ForStatement>(statement);
		GatherInformation(type, forStatement->startStatement, parent);
		GatherInformation(type, forStatement->condition, statement.get());
		GatherInformation(type, forStatement->incrementExpression, statement.get());
		GatherInformation(type, forStatement->bodyStatement, forStatement.get());
		break;
	}
	case StatementType::IF: {
		Ref<IfStatement> ifStatement = std::dynamic_pointer_cast<IfStatement>(statement);
		GatherInformation(type, ifStatement->condition, statement.get());
		GatherInformation(type, ifStatement->thenStatement, parent);
		GatherInformation(type, ifStatement->elseStatement, parent);
		break;
	}
	case StatementType::WHILE: {
		Ref<WhileStatement> whileStatement = std::dynamic_pointer_cast<WhileStatement>(statement);
		GatherInformation(type, whileStatement->condition, statement.get());
		GatherInformation(type, whileStatement->bodyStatement, parent);
		break;
	}
	case StatementType::RETURN: {
		Ref<ReturnStatement> returnStatement = std::dynamic_pointer_cast<ReturnStatement>(statement);
		GatherInformation(type, returnStatement->expression, statement.get());
		break;
	}
	case StatementType::VARIABLE_DECLARATION: {
		Ref<VariableDeclarationStatement> variableDeclarationStatement = std::dynamic_pointer_cast<VariableDeclarationStatement>(statement);
		GatherInformation(type, variableDeclarationStatement->value, statement.get());
		TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *variableDeclarationStatement->declaration->dataType);
		break;
	}
	default:
		break;
	}
}

void GatherInformationPass::GatherInformation(Ref<TypeDeclaration> type, Ref<MethodDeclaration> method)
{
	if ((flags & GatherInformationFlags::METHOD_NAME) == GatherInformationFlags::METHOD_NAME)
	{
		String name = type->name + ".";
		switch (method->methodType)
		{
		case MethodType::CONSTRUCTOR:
			name += type->name;
			break;
		case MethodType::DESTRUCTOR:
			name += "~" + type->name;
			break;
		case MethodType::GETTER:
			name += "get";
			break;
		case MethodType::METHOD:
			name += method->name;
			break;
		case MethodType::OPERATOR: {
			Ref<OperatorDeclaration> operatorDeclaration = std::dynamic_pointer_cast<OperatorDeclaration>(method);
			name += ToString(operatorDeclaration->operatorType);
			break;
		}
		case MethodType::SETTER:
			name += "set";
			break;
		default:
			STRICT_UNREACHABLE;
		}
		method->methodDeclarationMeta.name = name;
	}

	if (method->body)
	{
		GatherInformation(type, method->body, nullptr);
	}
}

void GatherInformationPass::GatherInformation(Ref<TypeDeclaration> type, Ref<MemberVariableDeclaration> variable, UInt32 index)
{
	if ((flags & GatherInformationFlags::MEMBER_INDEX) == GatherInformationFlags::MEMBER_INDEX)
	{
		variable->memberVariableDeclarationMeta.index = index;
	}

	for (auto accessor : variable->accessors)
	{
		GatherInformation(type, accessor);
	}
}

void GatherInformationPass::GatherInformation(Ref<TypeDeclaration> type)
{
	if ((flags & GatherInformationFlags::USED_TEMPLATES) != GatherInformationFlags::USED_TEMPLATES)
	{
		type->typeDeclarationMeta.usedTemplateTypes.clear();
	}

	UInt32 memberIndex = type->superTypes.size();

	for (auto member : type->members)
	{
		if ((flags & GatherInformationFlags::PARENT) == GatherInformationFlags::PARENT)
		{
			member->variableDeclarationMeta.parentType = type.get();
		}

		TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *member->dataType);

		if (member->variableType == VariableDeclarationType::MEMBER_VARIABLE)
		{
			GatherInformation(type, std::dynamic_pointer_cast<MemberVariableDeclaration>(member), memberIndex);
			memberIndex++;
		}
		else if (member->variableType == VariableDeclarationType::METHOD)
		{
			GatherInformation(type, std::dynamic_pointer_cast<MethodDeclaration>(member));
		}
	}

	for (auto superType : type->superTypes)
	{
		TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *superType);
	}

	if (type->declarationType == UnitDeclarationType::CLASS && (flags & GatherInformationFlags::THIS) == GatherInformationFlags::THIS)
	{
		Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(type);

		classDeclaration->classDeclarationMeta.thisDeclaration = Allocate<VariableDeclaration>();
		classDeclaration->classDeclarationMeta.thisDeclaration->dataType = type->unitDeclarationMeta.thisType;
		classDeclaration->classDeclarationMeta.thisDeclaration->name = "this";
	}
}

void GatherInformationPass::GatherInformation(BuildContext& context, Ref<Unit> unit)
{
	if ((flags & GatherInformationFlags::THIS) == GatherInformationFlags::THIS)
	{
		unit->declaredType->unitDeclarationMeta.thisType = Allocate<ObjectType>();
		unit->declaredType->unitDeclarationMeta.thisType->name = unit->name;
		unit->declaredType->unitDeclarationMeta.thisType->flags = DeclarationFlags::PRIVATE;
		unit->declaredType->unitDeclarationMeta.thisType->objectTypeMeta.unit = unit;
	}

	if ((flags & GatherInformationFlags::PARENT) == GatherInformationFlags::PARENT)
	{
		unit->declaredType->unitDeclarationMeta.parent = unit.get();
	}

	if (unit->declaredType->IsType())
	{
		GatherInformation(std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType));
	}

	for (const String& dependencyName : unit->dependencyNames)
	{
		unit->unitMeta.dependencies.push_back(context.ResolveUnit(dependencyName));
	}
}

PassResultFlags GatherInformationPass::Run(PrintFunction print, BuildContext& context)
{
	for (auto module : context.GetModules())
	{
		for (auto unit : module->units)
		{
			GatherInformation(context, unit);
		}
	}

	return PassResultFlags::SUCCESS;
}
