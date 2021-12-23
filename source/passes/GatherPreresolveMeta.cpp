
#include "GatherPreresolveMeta.h"

#include <iostream>

static void TryToInsertTemplatedObjectType(HashSet<ObjectType> &target, const DataType &dataType)
{
	if (dataType.dataTypeType == DataTypeType::OBJECT)
	{
		const ObjectType &objectType = (const ObjectType &)dataType;

		if (objectType.typeTemplate)
		{
			target.insert(objectType);
		}

		return;
	}

	if (dataType.IsPointer())
	{
		TryToInsertTemplatedObjectType(target, *((const PointerType &)dataType).value);
	}
}

static void GatherPreresolveMeta(Ref<TypeDeclaration> type, Ref<Expression> expression)
{
	if (!expression)
	{
		return;
	}

	switch (expression->expressionType)
	{
	case ExpressionType::BRACKET:
	{
		Ref<BracketExpression> bracketExpression = std::dynamic_pointer_cast<BracketExpression>(expression);
		GatherPreresolveMeta(type, bracketExpression->expression);
		break;
	}
	case ExpressionType::CALL:
	{
		Ref<CallExpression> callExpression = std::dynamic_pointer_cast<CallExpression>(expression);
		GatherPreresolveMeta(type, callExpression->method);
		for (auto argument : callExpression->arguments)
		{
			GatherPreresolveMeta(type, argument);
		}
		break;
	}
	case ExpressionType::NEW:
	{
		Ref<NewExpression> newExpression = std::dynamic_pointer_cast<NewExpression>(expression);
		TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *newExpression->dataType);
		for (auto argument : newExpression->arguments)
		{
			GatherPreresolveMeta(type, argument);
		}
		break;
	}
	case ExpressionType::OPERATOR:
	{
		Ref<OperatorExpression> operatorExpression = std::dynamic_pointer_cast<OperatorExpression>(expression);
		GatherPreresolveMeta(type, operatorExpression->a);
		if (operatorExpression->b)
		{
			if (operatorExpression->b->dataType)
			{
				TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *operatorExpression->b->dataType);
			}
			else
			{
				GatherPreresolveMeta(type, operatorExpression->b->expression);
			}
		}
		break;
	}
	case ExpressionType::TERNARY:
	{
		Ref<TernaryExpression> ternaryExpression = std::dynamic_pointer_cast<TernaryExpression>(expression);
		GatherPreresolveMeta(type, ternaryExpression->condition);
		GatherPreresolveMeta(type, ternaryExpression->thenExpression);
		GatherPreresolveMeta(type, ternaryExpression->elseExpression);
		break;
	}
	default:
		break;
	}
}

static void GatherPreresolveMeta(Ref<TypeDeclaration> type, Ref<Statement> statement)
{
	if (!statement)
	{
		return;
	}

	switch (statement->statementType)
	{
	case StatementType::BLOCK:
	{
		Ref<BlockStatement> block = std::dynamic_pointer_cast<BlockStatement>(statement);
		for (auto subStatement : block->statements)
		{
			GatherPreresolveMeta(type, subStatement);
		}
		break;
	}
	case StatementType::DELETE:
	{
		Ref<DeleteStatement> deleteStatement = std::dynamic_pointer_cast<DeleteStatement>(statement);
		GatherPreresolveMeta(type, deleteStatement->expression);
		break;
	}
	case StatementType::EXPRESSION:
	{
		Ref<ExpressionStatement> deleteStatement = std::dynamic_pointer_cast<ExpressionStatement>(statement);
		GatherPreresolveMeta(type, deleteStatement->expression);
		break;
	}
	case StatementType::FOR:
	{
		Ref<ForStatement> forStatement = std::dynamic_pointer_cast<ForStatement>(statement);
		GatherPreresolveMeta(type, forStatement->startStatement);
		GatherPreresolveMeta(type, forStatement->condition);
		GatherPreresolveMeta(type, forStatement->incrementExpression);
		GatherPreresolveMeta(type, forStatement->bodyStatement);
		break;
	}
	case StatementType::IF:
	{
		Ref<IfStatement> ifStatement = std::dynamic_pointer_cast<IfStatement>(statement);
		GatherPreresolveMeta(type, ifStatement->condition);
		GatherPreresolveMeta(type, ifStatement->thenStatement);
		GatherPreresolveMeta(type, ifStatement->elseStatement);
		break;
	}
	case StatementType::WHILE:
	{
		Ref<WhileStatement> whileStatement = std::dynamic_pointer_cast<WhileStatement>(statement);
		GatherPreresolveMeta(type, whileStatement->condition);
		GatherPreresolveMeta(type, whileStatement->bodyStatement);
		break;
	}
	case StatementType::RETURN:
	{
		Ref<ReturnStatement> returnStatement = std::dynamic_pointer_cast<ReturnStatement>(statement);
		GatherPreresolveMeta(type, returnStatement->expression);
		break;
	}
	case StatementType::VARIABLE_DECLARATION:
	{
		Ref<VariableDeclarationStatement> variableDeclarationStatement = std::dynamic_pointer_cast<VariableDeclarationStatement>(statement);
		GatherPreresolveMeta(type, variableDeclarationStatement->value);
		TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *variableDeclarationStatement->declaration->dataType);
		break;
	}
	default:
		break;
	}
}

static void GatherPreresolveMeta(Ref<TypeDeclaration> type, Ref<MethodDeclaration> method)
{
	if (method->body)
	{
		GatherPreresolveMeta(type, method->body);
	}
}

static void GatherPreresolveMeta(Ref<TypeDeclaration> type, Ref<MemberVariableDeclaration> variable)
{
	for (auto accessor : variable->accessors)
	{
		GatherPreresolveMeta(type, accessor);
	}
}

static void GatherPreresolveMeta(Ref<TypeDeclaration> type)
{
	for (auto member : type->members)
	{
		TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *member->dataType);

		if (member->variableType == VariableDeclarationType::MEMBER_VARIABLE)
		{
			GatherPreresolveMeta(type, std::dynamic_pointer_cast<MemberVariableDeclaration>(member));
		}
		else if (member->variableType == VariableDeclarationType::METHOD)
		{
			GatherPreresolveMeta(type, std::dynamic_pointer_cast<MethodDeclaration>(member));
		}
	}

	for (auto superType : type->superTypes)
	{
		TryToInsertTemplatedObjectType(type->typeDeclarationMeta.usedTemplateTypes, *superType);
	}
}

PassResultFlags GatherPreresolveMeta(PrintFunction print, BuildContext &context)
{
	for (auto module : context.GetModules())
	{
		for (auto unit : module->units)
		{
			if (unit->declaredType->IsType())
			{
				GatherPreresolveMeta(std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType));
			}
		}
	}
	// TODO: TypeDeclarationMeta::usedObjectTypes
	return PassResultFlags::SUCCESS;
}
