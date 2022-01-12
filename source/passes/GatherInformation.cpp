
#include "GatherInformation.h"

#include <iostream>

static void TryToInsertTemplatedObjectType(HashSet<ObjectType>& target, const DataType& dataType)
{
	if (dataType.dataTypeType == DataTypeType::OBJECT)
	{
		const ObjectType& objectType = (const ObjectType&)dataType;

		if (objectType.typeTemplate)
		{
			target.insert(objectType);
		}

		return;
	}

	if (dataType.IsPointer())
	{
		TryToInsertTemplatedObjectType(target, *((const PointerType&)dataType).value);
	}
}

static void GatherInformation(Ref<TypeDeclaration> type, Ref<Expression> expression, Statement* parentStatement)
{
	if (!expression)
	{
		return;
	}

	expression->expressionMeta.parentStatement = parentStatement;

	switch (expression->expressionType)
	{
	case ExpressionType::BRACKET: {
		Ref<BracketExpression> bracketExpression = std::dynamic_pointer_cast<BracketExpression>(expression);
		GatherInformation(type, bracketExpression->expression, parentStatement);
		break;
	}
	case ExpressionType::CALL: {
		Ref<CallExpression> callExpression = std::dynamic_pointer_cast<CallExpression>(expression);
		GatherInformation(type, callExpression->method, parentStatement);
		for (auto argument : callExpression->arguments)
		{
			GatherInformation(type, argument, parentStatement);
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
		break;
	}
	case ExpressionType::OPERATOR: {
		Ref<OperatorExpression> operatorExpression = std::dynamic_pointer_cast<OperatorExpression>(expression);
		GatherInformation(type, operatorExpression->a, parentStatement);
		if (operatorExpression->b)
		{
			if (operatorExpression->b->dataType)
			{
				TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *operatorExpression->b->dataType);
			}
			else
			{
				GatherInformation(type, operatorExpression->b->expression, parentStatement);
			}
		}
		break;
	}
	case ExpressionType::TERNARY: {
		Ref<TernaryExpression> ternaryExpression = std::dynamic_pointer_cast<TernaryExpression>(expression);
		GatherInformation(type, ternaryExpression->condition, parentStatement);
		GatherInformation(type, ternaryExpression->thenExpression, parentStatement);
		GatherInformation(type, ternaryExpression->elseExpression, parentStatement);
		break;
	}
	default:
		break;
	}
}

static void GatherInformation(Ref<TypeDeclaration> type, Ref<Statement> statement, Statement* parent)
{
	if (!statement)
	{
		return;
	}

	statement->statementMeta.parent = parent;

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

static void GatherInformation(Ref<TypeDeclaration> type, Ref<MethodDeclaration> method)
{
	if (method->body)
	{
		GatherInformation(type, method->body, nullptr);
	}
}

static void GatherInformation(Ref<TypeDeclaration> type, Ref<MemberVariableDeclaration> variable)
{
	for (auto accessor : variable->accessors)
	{
		GatherInformation(type, accessor);
	}
}

static void GatherInformation(Ref<TypeDeclaration> type)
{
	for (auto member : type->members)
	{
		TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *member->dataType);

		if (member->variableType == VariableDeclarationType::MEMBER_VARIABLE)
		{
			GatherInformation(type, std::dynamic_pointer_cast<MemberVariableDeclaration>(member));
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

	if (type->declarationType == UnitDeclarationType::CLASS)
	{
		Ref<ClassDeclaration> classDeclaration = std::dynamic_pointer_cast<ClassDeclaration>(type);

		classDeclaration->classDeclarationMeta.thisDeclaration = Allocate<VariableDeclaration>();
		classDeclaration->classDeclarationMeta.thisDeclaration->dataType = type->unitDeclarationMeta.thisType;
		classDeclaration->classDeclarationMeta.thisDeclaration->name = "this";
	}
}

static void GatherInformation(BuildContext& context, Ref<Unit> unit)
{
	unit->declaredType->unitDeclarationMeta.thisType = Allocate<ObjectType>();
	unit->declaredType->unitDeclarationMeta.thisType->name = unit->name;
	unit->declaredType->unitDeclarationMeta.thisType->flags = DeclarationFlags::PRIVATE;
	unit->declaredType->unitDeclarationMeta.thisType->objectTypeMeta.unit = unit->declaredType;

	if (unit->declaredType->IsType())
	{
		GatherInformation(std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType));
	}

	for (const String& dependencyName : unit->dependencyNames)
	{
		unit->unitMeta.dependencies.push_back(context.ResolveUnit(dependencyName));
	}
}

PassResultFlags GatherInformation(PrintFunction print, BuildContext& context)
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
