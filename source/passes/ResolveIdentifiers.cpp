
#include "ResolveIdentifiers.h"

Ref<UnitDeclaration> ResolveContext::ResolveType(const String& name)
{
	if (name == unit->name)
	{
		return unit->declaredType;
	}

	Ref<TypeDeclaration> declaration = std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType);

	if (declaration->typeTemplate)
	{
		for (auto parameter : declaration->typeTemplate->parameters)
		{
			if (parameter->name != name)
			{
				continue;
			}

			if (parameter->dataType->dataTypeType == DataTypeType::OBJECT)
			{
				Ref<UnitDeclaration> result = std::dynamic_pointer_cast<ObjectType>(parameter->dataType)->objectTypeMeta.unit;

				if (result->declarationType != UnitDeclarationType::TYPE)
				{
					continue;
				}

				return result;
			}
			else if (parameter->dataType->dataTypeType == DataTypeType::TYPE)
			{
				return nullptr;
			}
		}
	}

	for (const auto& dependency : unit->dependencyNames)
	{
		if (dependency.find_last_of(name) == dependency.length() - 1 && dependency[dependency.length() - name.length() - 1] == '.')
		{
			return context->ResolveUnit(dependency)->declaredType;
		}
	}

	err.PrintError("Unable to resolve type '" + name + "'!");
	return nullptr;
}

PassResultFlags ResolveContext::ResolveTemplate(Ref<Template> typeTemplate)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto argument : typeTemplate->arguments)
	{
		if (argument.dataType)
		{
			result = result | ResolveDataType(argument.dataType);
		}
	}

	return result;
}

PassResultFlags ResolveContext::ResolveDataType(Ref<DataType> type)
{
	if (type->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(type);
		objectType->objectTypeMeta.unit = ResolveType(objectType->name);
		PassResultFlags result = (objectType->objectTypeMeta.unit == nullptr) ? PassResultFlags::CRITICAL_ERROR : PassResultFlags::SUCCESS;

		if (objectType->typeTemplate)
		{
			result = result | ResolveTemplate(objectType->typeTemplate);
		}

		return result;
	}

	if (type->dataTypeType == DataTypeType::ARRAY || type->dataTypeType == DataTypeType::REFERENCE || type->dataTypeType == DataTypeType::POINTER)
	{
		Ref<PointerType> pointerType = std::dynamic_pointer_cast<PointerType>(type);
		return ResolveDataType(pointerType->value);
	}

	return PassResultFlags::SUCCESS;
}

PassResultFlags ResolveContext::ResolveTemplateDeclaration(Ref<TemplateDeclaration> declaration)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto parameter : declaration->parameters)
	{
		result = result | ResolveDataType(parameter->dataType);
	}

	return result;
}

PassResultFlags ResolveContext::ResolveSuperTypes()
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto superType : type->superTypes)
	{
		result = result | ResolveDataType(superType);
	}

	return result;
}

PassResultFlags ResolveContext::ResolveExpression(Ref<Expression> expression)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	return result;
}

PassResultFlags ResolveContext::ResolveStatement(Ref<Statement> statement)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	switch (statement->statementType)
	{
	case StatementType::BLOCK: {
		Ref<BlockStatement> blockStatement = std::dynamic_pointer_cast<BlockStatement>(statement);
		for (auto subStatement : blockStatement->statements)
		{
			result = result | ResolveStatement(subStatement);
		}
		break;
	}
	case StatementType::DELETE: {
		Ref<DeleteStatement> deleteStatement = std::dynamic_pointer_cast<DeleteStatement>(statement);
		result = result | ResolveExpression(deleteStatement->expression);
		break;
	}
	case StatementType::EXPRESSION: {
		Ref<ExpressionStatement> expressionStatement = std::dynamic_pointer_cast<ExpressionStatement>(statement);
		result = result | ResolveExpression(expressionStatement->expression);
		break;
	}
	case StatementType::FOR: {
		Ref<ForStatement> forStatement = std::dynamic_pointer_cast<ForStatement>(statement);
		result = result | ResolveStatement(forStatement->startStatement);
		result = result | ResolveExpression(forStatement->condition);
		result = result | ResolveExpression(forStatement->incrementExpression);
		result = result | ResolveStatement(forStatement->bodyStatement);
		break;
	}
	case StatementType::IF: {
		Ref<IfStatement> ifStatement = std::dynamic_pointer_cast<IfStatement>(statement);
		result = result | ResolveExpression(ifStatement->condition);
		result = result | ResolveStatement(ifStatement->thenStatement);
		if (ifStatement->elseStatement)
		{
			result = result | ResolveStatement(ifStatement->elseStatement);
		}
		break;
	}
	case StatementType::RETURN: {
		Ref<ReturnStatement> returnStatement = std::dynamic_pointer_cast<ReturnStatement>(statement);
		result = result | ResolveExpression(returnStatement->expression);
		break;
	}
	case StatementType::VARIABLE_DECLARATION: {
		Ref<VariableDeclarationStatement> variableDeclarationStatement = std::dynamic_pointer_cast<VariableDeclarationStatement>(statement);
		result = result | ResolveDataType(variableDeclarationStatement->declaration->dataType);
		if (variableDeclarationStatement->value)
		{
			result = result | ResolveExpression(variableDeclarationStatement->value);
		}
		break;
	}
	case StatementType::WHILE: {
		Ref<WhileStatement> whileStatement = std::dynamic_pointer_cast<WhileStatement>(statement);
		result = result | ResolveExpression(whileStatement->condition);
		result = result | ResolveStatement(whileStatement->bodyStatement);
		break;
	}
	}

	return result;
}

PassResultFlags ResolveContext::ResolveMethodDeclaration(Ref<MethodDeclaration> method)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto parameter : method->parameters)
	{
		result = result | ResolveDataType(parameter->dataType);
	}

	if (method->body)
	{
		result = result | ResolveStatement(method->body);
	}

	return result;
}

PassResultFlags ResolveContext::ResolveMemberVariableDeclaration(Ref<MemberVariableDeclaration> variable)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto accessor : variable->accessors)
	{
		result = result | ResolveMethodDeclaration(accessor);
	}

	if (variable->value)
	{
		result = result | ResolveExpression(variable->value);
	}

	return result;
}

PassResultFlags ResolveContext::ResolveVariableDeclaration(Ref<VariableDeclaration> variable)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	result = result | ResolveDataType(variable->dataType);

	if (variable->variableType == VariableDeclarationType::METHOD)
	{
		result = result | ResolveMethodDeclaration(std::dynamic_pointer_cast<MethodDeclaration>(variable));
	}
	else if (variable->variableType == VariableDeclarationType::MEMBER_VARIABLE)
	{
		result = result | ResolveMemberVariableDeclaration(std::dynamic_pointer_cast<MemberVariableDeclaration>(variable));
	}

	return result;
}

PassResultFlags ResolveContext::ResolveMembers()
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto member : type->members)
	{
		result = result | ResolveVariableDeclaration(member);
	}

	return result;
}

PassResultFlags ResolveContext::ResolveIdentifiers()
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	if (type->typeTemplate)
	{
		result = result | ResolveTemplateDeclaration(type->typeTemplate);
	}

	result = result | ResolveSuperTypes();
	result = result | ResolveMembers();

	return result;
}

PassResultFlags ResolveIdentifiers(PrintFunction print, BuildContext& context)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto module : context.GetModules())
	{
		for (auto unit : module->units)
		{
			if (unit->declaredType->IsType())
			{
				ResolveContext resolve(&context, unit->name, print, nullptr);
				resolve.unit = unit;
				resolve.type = std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType);
				resolve.ResolveIdentifiers();
			}
		}
	}

	return result;
}
