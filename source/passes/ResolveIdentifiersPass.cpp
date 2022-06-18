
#include "ResolveIdentifiersPass.h"

bool ResolveContext::IsTraversalRequired(TraversalLevel level) const
{
	return (level & requiredTraversalLevel) == level;
}

static bool HasEnding(const String& fullString, const String& ending)
{
	if (fullString.length() >= ending.length())
	{
		return (0 ==
		        fullString.compare(fullString.length() - ending.length(), ending.length(), ending));
	}
	else
	{
		return false;
	}
}

Ref<Unit> ResolveContext::ResolveType(const String& name, bool optional, UInt32 characterIndex)
{
	if (name == unit->name)
	{
		return unit;
	}

	Ref<TypeDeclaration> declaration =
		std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType);

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
				Ref<Unit> result =
					std::dynamic_pointer_cast<ObjectType>(parameter->dataType)->objectTypeMeta.unit;

				if (result->declaredType->declarationType != UnitDeclarationType::TYPE)
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
		if (HasEnding(dependency, "." + name))
		{
			return context->ResolveUnit(dependency);
		}
	}

	if (!optional)
	{
		err.PrintError(characterIndex, "Unable to resolve type '" + name + "'!");
	}

	return nullptr;
}

PassResultFlags ResolveContext::ResolveTemplate(Ref<MethodDeclaration> method,
                                                Ref<Template> typeTemplate)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto& argument : typeTemplate->arguments)
	{
		if (argument.dataType)
		{
			result = result | ResolveDataType(method, argument.dataType);
		}
		if (argument.expression)
		{
			result = result | ResolveExpression(method, &argument.expression);

			if (pass == ResolvePass::DATA_TYPES)
			{
				Ref<DataType> dataType = ConvertExpressionToDataType(argument.expression);
				if (dataType)
				{
					argument.dataType = dataType;
					argument.expression = nullptr;
				}
			}
		}
	}

	return result;
}

PassResultFlags ResolveContext::ResolveDataType(Ref<MethodDeclaration> method, Ref<DataType> type)
{
	if (type->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(type);
		objectType->objectTypeMeta.unit =
			ResolveType(objectType->name, false, type->characterIndex);
		PassResultFlags result =
			err.HasErrorOccured() ? PassResultFlags::CRITICAL_ERROR : PassResultFlags::SUCCESS;

		if (objectType->typeTemplate)
		{
			result = result | ResolveTemplate(method, objectType->typeTemplate);
		}

		return result;
	}

	if (type->dataTypeType == DataTypeType::ARRAY ||
	    type->dataTypeType == DataTypeType::REFERENCE ||
	    type->dataTypeType == DataTypeType::POINTER)
	{
		Ref<PointerType> pointerType = std::dynamic_pointer_cast<PointerType>(type);
		PassResultFlags result = ResolveDataType(method, pointerType->value);
		if (pointerType->arrayLength)
		{
			result = result | ResolveExpression(method, &pointerType->arrayLength);
		}
		return result;
	}

	return PassResultFlags::SUCCESS;
}

PassResultFlags ResolveContext::ResolveTemplateDeclaration(Ref<TemplateDeclaration> declaration)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto parameter : declaration->parameters)
	{
		result = result | ResolveDataType(nullptr, parameter->dataType);
	}

	return result;
}

PassResultFlags ResolveContext::ResolveSuperTypes()
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto superType : type->superTypes)
	{
		result = result | ResolveDataType(nullptr, superType);
	}

	return result;
}

static Ref<PrimitiveType> GetPrecedingPrimitiveType(Ref<PrimitiveType> a, Ref<PrimitiveType> b)
{
	if (a->primitiveType == b->primitiveType)
	{
		return a;
	}

	const bool aFloat = a->IsFloat();
	const bool bFloat = b->IsFloat();

	if ((aFloat && !bFloat) || (!aFloat && !bFloat))
	{
		return a;
	}

	if (bFloat && !aFloat)
	{
		return b;
	}

	return (a->GetSize() < b->GetSize()) ? b : a;
}

static Ref<DataType> ResolveOperatorDataType(Ref<OperatorExpression> expression)
{
	if (expression->a->expressionMeta.dataType->dataTypeType == DataTypeType::PRIMITIVE)
	{
		if (!expression->b)
		{
			return expression->a->expressionMeta.dataType;
		}
		if (expression->b->dataType)
		{
			return expression->b->dataType;
		}

		if (expression->b->expression->expressionMeta.dataType->dataTypeType ==
		    DataTypeType::PRIMITIVE)
		{
			Ref<PrimitiveType> aType =
				std::dynamic_pointer_cast<PrimitiveType>(expression->a->expressionMeta.dataType);
			Ref<PrimitiveType> bType = std::dynamic_pointer_cast<PrimitiveType>(
				expression->b->expression->expressionMeta.dataType);

			return GetPrecedingPrimitiveType(aType, bType);
		}
	}

	if (expression->a->expressionMeta.dataType->IsPointer())
	{
		Ref<PointerType> aType =
			std::dynamic_pointer_cast<PointerType>(expression->a->expressionMeta.dataType);

		if (expression->operatorType == OperatorType::DEREFERENCE ||
		    expression->operatorType == OperatorType::ARRAY_ACCESS)
		{
			return aType->value;
		}
	}

	return nullptr;
}

PassResultFlags ResolveContext::ResolveOperatorExpression(Ref<MethodDeclaration> method,
                                                          Ref<OperatorExpression> expression)
{
	PassResultFlags result = ResolveExpression(method, &expression->a);
	if (expression->b)
	{
		if (expression->b->expression)
		{
			if (expression->operatorType == OperatorType::ACCESS &&
			    expression->b->expression->expressionType == ExpressionType::IDENTIFIER)
			{
				if (pass != ResolvePass::EXPRESSION)
				{
					return result;
				}

				Ref<IdentifierExpression> identifierExpression =
					std::dynamic_pointer_cast<IdentifierExpression>(expression->b->expression);
				Ref<ObjectType> leftType = std::dynamic_pointer_cast<ObjectType>(
					GetReferencedType(expression->a->expressionMeta.dataType));
				result = result |
				         ResolveIdentifierExpression(leftType, method, identifierExpression, true);
			}
			else
			{
				result = result | ResolveExpression(method, &expression->b->expression);
			}
		}
		if (expression->b->dataType)
		{
			result = result | ResolveDataType(method, expression->b->dataType);
		}
		if (expression->b->typeTemplate)
		{
			result = result | ResolveTemplate(method, expression->b->typeTemplate);
		}
	}

	if (pass == ResolvePass::EXPRESSION)
	{
		expression->expressionMeta.dataType = ResolveOperatorDataType(expression);
	}

	return result;
}

static Ref<VariableDeclaration> SearchVariableDeclaration(const Statement* statement,
                                                          const String& name)
{
	if (statement->statementType == StatementType::BLOCK)
	{
		const BlockStatement* blockStatement = (const BlockStatement*)statement;
		for (auto subStatement : blockStatement->statements)
		{
			if (subStatement.get() == statement)
			{
				// Don't consider statements afther the current statement
				break;
			}

			if (subStatement->statementType != StatementType::VARIABLE_DECLARATION)
			{
				continue;
			}

			Ref<VariableDeclarationStatement> variableDeclarationStatement =
				std::dynamic_pointer_cast<VariableDeclarationStatement>(subStatement);
			if (variableDeclarationStatement->declaration->name == name)
			{
				return variableDeclarationStatement->declaration;
			}
		}
	}
	else if (statement->statementType == StatementType::FOR)
	{
		const ForStatement* forStatement = (const ForStatement*)statement;
		if (forStatement->startStatement->statementType == StatementType::VARIABLE_DECLARATION)
		{
			Ref<VariableDeclarationStatement> variableDeclarationStatement =
				std::dynamic_pointer_cast<VariableDeclarationStatement>(
					forStatement->startStatement);
			if (variableDeclarationStatement->declaration->name == name)
			{
				return variableDeclarationStatement->declaration;
			}
		}
	}

	if (statement->statementMeta.parent)
	{
		return SearchVariableDeclaration(statement->statementMeta.parent, name);
	}

	return nullptr;
}

PassResultFlags ResolveContext::ResolveIdentifierExpression(Ref<ObjectType> context,
                                                            Ref<MethodDeclaration> method,
                                                            Ref<IdentifierExpression> expression,
                                                            bool required)
{
	Ref<Unit> unit = ResolveType(expression->name, true, expression->characterIndex);
	if (unit)
	{
		expression->identifierExpressionMeta.destination = unit;
		expression->expressionMeta.dataType = unit->declaredType->unitDeclarationMeta.thisType;
		return PassResultFlags::SUCCESS;
	}

	if (method)
	{
		for (auto parameter : method->parameters)
		{
			if (parameter->name != expression->name)
			{
				continue;
			}

			expression->identifierExpressionMeta.destination = parameter;
			expression->expressionMeta.dataType = parameter->dataType;
			return PassResultFlags::SUCCESS;
		}
	}

	if (context)
	{
		Ref<TypeDeclaration> typeDeclaration =
			std::dynamic_pointer_cast<TypeDeclaration>(context->objectTypeMeta.unit->declaredType);
		for (auto member : typeDeclaration->members)
		{
			if (member->name != expression->name)
			{
				continue;
			}

			expression->identifierExpressionMeta.destination = member;
			expression->expressionMeta.dataType = member->dataType;
			return PassResultFlags::SUCCESS;
		}

		for (auto superType : typeDeclaration->superTypes)
		{
			if (ResolveIdentifierExpression(superType, nullptr, expression, false) ==
			    PassResultFlags::SUCCESS)
			{
				return PassResultFlags::SUCCESS;
			}
		}

		if (typeDeclaration->typeTemplate)
		{
			for (auto parameter : typeDeclaration->typeTemplate->parameters)
			{
				if (parameter->name != expression->name)
				{
					continue;
				}
				expression->identifierExpressionMeta.destination = parameter;
				expression->expressionMeta.dataType = parameter->dataType;
				return PassResultFlags::SUCCESS;
			}
		}
	}

	if (expression->expressionMeta.parentStatement)
	{
		Ref<VariableDeclaration> declaration =
			SearchVariableDeclaration(expression->expressionMeta.parentStatement, expression->name);
		if (declaration)
		{
			expression->identifierExpressionMeta.destination = declaration;
			expression->expressionMeta.dataType = declaration->dataType;
			return PassResultFlags::SUCCESS;
		}
	}

	if (required)
	{
		err.PrintError(expression->characterIndex,
		               "Unable to resolve identifier '" + expression->name + "'!");
	}
	return PassResultFlags::CRITICAL_ERROR;
}

void ResolveContext::ResolveLiteralExpression(Ref<LiteralExpression> expression)
{
	if (expression->data.type == TokenType::INT_LITERAL)
	{
		if (expression->data.data.intData >= INT8_MIN && expression->data.data.intData <= INT8_MAX)
		{
			expression->expressionMeta.dataType = Allocate<PrimitiveType>(TokenType::INT8);
		}
		else if (expression->data.data.intData >= INT16_MIN &&
		         expression->data.data.intData <= INT16_MAX)
		{
			expression->expressionMeta.dataType = Allocate<PrimitiveType>(TokenType::INT16);
		}
		else if (expression->data.data.intData >= INT32_MIN &&
		         expression->data.data.intData <= INT32_MAX)
		{
			expression->expressionMeta.dataType = Allocate<PrimitiveType>(TokenType::INT32);
		}
		else
		{
			expression->expressionMeta.dataType = Allocate<PrimitiveType>(TokenType::INT64);
		}
	}
	else if (expression->data.type == TokenType::UINT_LITERAL)
	{
		if (expression->data.data.uintData <= UINT8_MAX)
		{
			expression->expressionMeta.dataType = Allocate<PrimitiveType>(TokenType::UINT8);
		}
		else if (expression->data.data.uintData <= UINT16_MAX)
		{
			expression->expressionMeta.dataType = Allocate<PrimitiveType>(TokenType::UINT16);
		}
		else if (expression->data.data.uintData <= UINT32_MAX)
		{
			expression->expressionMeta.dataType = Allocate<PrimitiveType>(TokenType::UINT32);
		}
		else
		{
			expression->expressionMeta.dataType = Allocate<PrimitiveType>(TokenType::UINT64);
		}
	}
	else if (expression->data.type == TokenType::FLOAT_LITERAL)
	{
		expression->expressionMeta.dataType = Allocate<PrimitiveType>(TokenType::FLOAT64);
	}
	else if (expression->data.type == TokenType::STRING_LITERAL)
	{
		Ref<PointerType> type = Allocate<PointerType>();
		type->dataTypeType = DataTypeType::ARRAY;
		type->value = Allocate<PrimitiveType>(TokenType::INT8);
		expression->expressionMeta.dataType = type;
	}
	else
	{
		STRICT_UNREACHABLE;
	}
}

PassResultFlags ResolveContext::ResolveBracketExpression(Ref<MethodDeclaration> method,
                                                         Ref<BracketExpression> expression)
{
	PassResultFlags result = ResolveExpression(method, &expression->expression);

	if (pass != ResolvePass::EXPRESSION)
	{
		return result;
	}

	expression->expressionMeta.dataType = expression->expression->expressionMeta.dataType;

	return result;
}

static Ref<Expression> GetRightMostExpression(Ref<Expression> expression)
{
	if (expression->expressionType == ExpressionType::OPERATOR)
	{
		Ref<OperatorExpression> operatorExpression =
			std::dynamic_pointer_cast<OperatorExpression>(expression);
		if (!operatorExpression->b)
		{
			return nullptr;
		}
		if (operatorExpression->b->dataType)
		{
			return GetRightMostExpression(operatorExpression->a);
		}
		return GetRightMostExpression(operatorExpression->b->expression);
	}
	return expression;
}

Ref<DataType> ResolveContext::ConvertExpressionToDataType(Ref<Expression> expression)
{
	if (expression->expressionType == ExpressionType::IDENTIFIER)
	{
		Ref<IdentifierExpression> identifierExpression =
			std::dynamic_pointer_cast<IdentifierExpression>(expression);

		if (identifierExpression->identifierExpressionMeta.destination->type != ASTItemType::UNIT)
		{
			return nullptr;
		}

		Ref<Unit> destination = std::dynamic_pointer_cast<Unit>(
			identifierExpression->identifierExpressionMeta.destination);

		Ref<ObjectType> result = Allocate<ObjectType>();
		result->objectTypeMeta.unit = destination;
		result->name = destination->name;
		return result;
	}

	if (expression->expressionType == ExpressionType::OPERATOR)
	{
		Ref<OperatorExpression> operatorExpression =
			std::dynamic_pointer_cast<OperatorExpression>(expression);

		if (operatorExpression->operatorType == OperatorType::ARRAY_ACCESS ||
		    operatorExpression->operatorType == OperatorType::EXPLICIT_CAST ||
		    operatorExpression->operatorType == OperatorType::TEMPLATE)
		{
			Ref<DataType> dataType = ConvertExpressionToDataType(operatorExpression->a);
			if (!dataType)
			{
				return nullptr;
			}

			if (dataType->dataTypeType != DataTypeType::OBJECT)
			{
				err.PrintError(operatorExpression->a->characterIndex, "Expected an object type!");
				return nullptr;
			}

			if (operatorExpression->operatorType == OperatorType::ARRAY_ACCESS)
			{
				Ref<PointerType> result = Allocate<PointerType>();
				result->dataTypeType = DataTypeType::ARRAY;
				result->value = dataType;
				result->arrayLength = operatorExpression->b->expression;
				return result;
			}

			Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(dataType);

			if (operatorExpression->operatorType == OperatorType::EXPLICIT_CAST)
			{
				objectType->typeTemplate = Allocate<Template>();
				TemplateArgument argument;
				argument.dataType = operatorExpression->b->dataType;
				objectType->typeTemplate->arguments.push_back(argument);
			}
			else
			{
				objectType->typeTemplate = operatorExpression->b->typeTemplate;
			}

			return objectType;
		}
		else if (operatorExpression->operatorType == OperatorType::POST_AND ||
		         operatorExpression->operatorType == OperatorType::POST_STAR)
		{
			Ref<DataType> dataType = ConvertExpressionToDataType(operatorExpression->a);
			if (!dataType)
			{
				return nullptr;
			}

			Ref<PointerType> result = Allocate<PointerType>();
			result->dataTypeType = (operatorExpression->operatorType == OperatorType::POST_AND)
			                           ? DataTypeType::REFERENCE
			                           : DataTypeType::POINTER;
			result->value = dataType;
			return result;
		}
	}

	return nullptr;
}

PassResultFlags ResolveContext::ResolveCallExpression(Ref<MethodDeclaration> method,
                                                      Ref<Expression>* expression)
{
	Ref<CallExpression> callExpression = std::dynamic_pointer_cast<CallExpression>(*expression);

	PassResultFlags result = ResolveExpression(method, &callExpression->method);

	for (auto& argument : callExpression->arguments)
	{
		result = result | ResolveExpression(method, &argument);
	}

	if ((result & PassResultFlags::CRITICAL_ERROR) == PassResultFlags::CRITICAL_ERROR)
	{
		return result;
	}

	if (pass == ResolvePass::NEW)
	{
		Ref<DataType> dataType = ConvertExpressionToDataType(callExpression->method);
		if (dataType)
		{
			Ref<NewExpression> newExpression = Allocate<NewExpression>();
			newExpression->expressionMeta.parentStatement =
				callExpression->expressionMeta.parentStatement;
			newExpression->expressionMeta.dataType = dataType;
			newExpression->allocationType = AllocationType::STACK;
			newExpression->dataType = dataType;
			newExpression->arguments = callExpression->arguments;

			*expression = newExpression;

			return result;
		}
	}

	if (pass != ResolvePass::EXPRESSION)
	{
		return result;
	}

	// TODO: Consider overloading
	Ref<Expression> actualMethod = GetRightMostExpression(callExpression->method);
	if (!actualMethod || actualMethod->expressionType != ExpressionType::IDENTIFIER)
	{
		err.PrintError(callExpression->characterIndex,
		               "Cannot call method expression:\n" + callExpression->method->ToString(1));
		result = result | PassResultFlags::CRITICAL_ERROR;
	}
	else
	{
		Ref<IdentifierExpression> methodIdentifier =
			std::dynamic_pointer_cast<IdentifierExpression>(actualMethod);
		Ref<ASTItem> destinationItem = methodIdentifier->identifierExpressionMeta.destination;
		if (destinationItem->type == ASTItemType::VARIABLE_DECLARATION)
		{
			Ref<VariableDeclaration> destination =
				std::dynamic_pointer_cast<VariableDeclaration>(destinationItem);
			callExpression->callExpressionMeta.destination = destination;

			if (destination->variableType == VariableDeclarationType::METHOD)
			{
				callExpression->expressionMeta.dataType = destination->dataType;

				Expression* parentExpression = actualMethod->expressionMeta.parentExpression;

				if (parentExpression &&
				    parentExpression->expressionType == ExpressionType::OPERATOR &&
				    ((OperatorExpression*)parentExpression)->operatorType == OperatorType::ACCESS)
				{
					OperatorExpression* parentOperator = (OperatorExpression*)parentExpression;
					assert(parentOperator->b->expression == actualMethod);
					callExpression->callExpressionMeta.context = parentOperator->a;
				}
				else
				{
					// If this call does not look like 'a.b()', the destination method must be a
					// member of the current type.
					assert(std::find(type->members.begin(), type->members.end(), destination) !=
					       type->members.end());
				}
			}
		}
	}

	return result;
}

PassResultFlags ResolveContext::ResolveNewExpression(Ref<MethodDeclaration> method,
                                                     Ref<NewExpression> expression)
{
	PassResultFlags result = ResolveDataType(method, expression->dataType);

	for (auto& argument : expression->arguments)
	{
		result = result | ResolveExpression(method, &argument);
	}

	if (pass != ResolvePass::EXPRESSION)
	{
		return result;
	}

	if (expression->allocationType == AllocationType::HEAP)
	{
		Ref<PointerType> pointer = Allocate<PointerType>();
		if (expression->dataType->dataTypeType == DataTypeType::ARRAY)
		{
			pointer->dataTypeType = DataTypeType::ARRAY;
		}
		else
		{
			pointer->dataTypeType = DataTypeType::POINTER;
		}
		pointer->value = expression->dataType;
		expression->expressionMeta.dataType = pointer;
	}
	else
	{
		expression->expressionMeta.dataType = expression->dataType;
	}

	if (expression->dataType->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(expression->dataType);
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

		expression->newExpressionMeta.destination =
			std::dynamic_pointer_cast<ConstructorDeclaration>(
				classDeclaration->FindMethod(MethodType::CONSTRUCTOR, &parameters));
	}

	return result;
}

PassResultFlags ResolveContext::ResolveTernaryExpression(Ref<MethodDeclaration> method,
                                                         Ref<TernaryExpression> expression)
{
	PassResultFlags result = ResolveExpression(method, &expression->condition);
	result = result | ResolveExpression(method, &expression->thenExpression);
	result = result | ResolveExpression(method, &expression->elseExpression);

	if (pass != ResolvePass::EXPRESSION)
	{
		return result;
	}

	assert(expression->thenExpression->expressionMeta.dataType &&
	       expression->elseExpression->expressionMeta.dataType);
	// TODO: check for the possibility to perform an implicit cast
	if (*expression->thenExpression->expressionMeta.dataType ==
	    *expression->elseExpression->expressionMeta.dataType)
	{
		expression->expressionMeta.dataType = expression->thenExpression->expressionMeta.dataType;
	}
	else
	{
		err.PrintError(expression->thenExpression->characterIndex,
		               "Cannot match the types of the two ternary operator cases!");
		result = result | PassResultFlags::CRITICAL_ERROR;
	}
	return result;
}

PassResultFlags ResolveContext::ResolveExpression(Ref<MethodDeclaration> method,
                                                  Ref<Expression>* expression)
{
	if (!IsTraversalRequired(TraversalLevel::EXPRESSION))
	{
		return PassResultFlags::SUCCESS;
	}

	switch ((*expression)->expressionType)
	{
	case ExpressionType::LITERAL:
		ResolveLiteralExpression(std::dynamic_pointer_cast<LiteralExpression>(*expression));
		return PassResultFlags::SUCCESS;
	case ExpressionType::BRACKET:
		return ResolveBracketExpression(method,
		                                std::dynamic_pointer_cast<BracketExpression>(*expression));
	case ExpressionType::CALL:
		return ResolveCallExpression(method, expression);
	case ExpressionType::NEW:
		return ResolveNewExpression(method, std::dynamic_pointer_cast<NewExpression>(*expression));
	case ExpressionType::OPERATOR:
		return ResolveOperatorExpression(
			method, std::dynamic_pointer_cast<OperatorExpression>(*expression));
	case ExpressionType::TERNARY:
		return ResolveTernaryExpression(method,
		                                std::dynamic_pointer_cast<TernaryExpression>(*expression));
	case ExpressionType::IDENTIFIER: {
		if (pass != ResolvePass::EXPRESSION)
		{
			return PassResultFlags::SUCCESS;
		}
		Ref<IdentifierExpression> identifierExpression =
			std::dynamic_pointer_cast<IdentifierExpression>(*expression);
		Ref<ClassDeclaration> classDeclaration =
			std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType);
		Ref<ObjectType> thisType = std::dynamic_pointer_cast<ObjectType>(
			classDeclaration->classDeclarationMeta.thisDeclaration->dataType);
		return ResolveIdentifierExpression(thisType, method, identifierExpression, true);
	}
	}

	return PassResultFlags::SUCCESS;
}

PassResultFlags ResolveContext::ResolveStatement(Ref<MethodDeclaration> method,
                                                 Ref<Statement> statement)
{
	if (!IsTraversalRequired(TraversalLevel::STATEMENT))
	{
		return PassResultFlags::SUCCESS;
	}

	PassResultFlags result = PassResultFlags::SUCCESS;

	switch (statement->statementType)
	{
	case StatementType::BLOCK: {
		Ref<BlockStatement> blockStatement = std::dynamic_pointer_cast<BlockStatement>(statement);
		for (auto subStatement : blockStatement->statements)
		{
			result = result | ResolveStatement(method, subStatement);
		}
		break;
	}
	case StatementType::DELETE: {
		Ref<DeleteStatement> deleteStatement =
			std::dynamic_pointer_cast<DeleteStatement>(statement);
		result = result | ResolveExpression(method, &deleteStatement->expression);
		break;
	}
	case StatementType::EXPRESSION: {
		Ref<ExpressionStatement> expressionStatement =
			std::dynamic_pointer_cast<ExpressionStatement>(statement);
		result = result | ResolveExpression(method, &expressionStatement->expression);
		break;
	}
	case StatementType::FOR: {
		Ref<ForStatement> forStatement = std::dynamic_pointer_cast<ForStatement>(statement);
		result = result | ResolveStatement(method, forStatement->startStatement);
		result = result | ResolveExpression(method, &forStatement->condition);
		result = result | ResolveExpression(method, &forStatement->incrementExpression);
		result = result | ResolveStatement(method, forStatement->bodyStatement);
		break;
	}
	case StatementType::IF: {
		Ref<IfStatement> ifStatement = std::dynamic_pointer_cast<IfStatement>(statement);
		result = result | ResolveExpression(method, &ifStatement->condition);
		result = result | ResolveStatement(method, ifStatement->thenStatement);
		if (ifStatement->elseStatement)
		{
			result = result | ResolveStatement(method, ifStatement->elseStatement);
		}
		break;
	}
	case StatementType::RETURN: {
		Ref<ReturnStatement> returnStatement =
			std::dynamic_pointer_cast<ReturnStatement>(statement);
		if (returnStatement->expression)
		{
			result = result | ResolveExpression(method, &returnStatement->expression);
		}
		break;
	}
	case StatementType::VARIABLE_DECLARATION: {
		Ref<VariableDeclarationStatement> variableDeclarationStatement =
			std::dynamic_pointer_cast<VariableDeclarationStatement>(statement);
		result =
			result | ResolveDataType(method, variableDeclarationStatement->declaration->dataType);
		if (variableDeclarationStatement->value)
		{
			result = result | ResolveExpression(method, &variableDeclarationStatement->value);
		}
		break;
	}
	case StatementType::WHILE: {
		Ref<WhileStatement> whileStatement = std::dynamic_pointer_cast<WhileStatement>(statement);
		result = result | ResolveExpression(method, &whileStatement->condition);
		result = result | ResolveStatement(method, whileStatement->bodyStatement);
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
		result = result | ResolveDataType(method, parameter->dataType);
	}

	if (method->body)
	{
		result = result | ResolveStatement(method, method->body);
	}

	if (method->methodType == MethodType::CONSTRUCTOR)
	{
		Ref<ConstructorDeclaration> constructor =
			std::dynamic_pointer_cast<ConstructorDeclaration>(method);
		for (auto& initializer : constructor->initializers)
		{
			result = result | ResolveExpression(method, &initializer.value);
		}
	}

	return result;
}

PassResultFlags ResolveContext::ResolveMemberVariableDeclaration(
	Ref<MemberVariableDeclaration> variable)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto accessor : variable->accessors)
	{
		result = result | ResolveMethodDeclaration(accessor);
	}

	if (variable->value)
	{
		result = result | ResolveExpression(nullptr, &variable->value);
	}

	return result;
}

PassResultFlags ResolveContext::ResolveVariableDeclaration(Ref<VariableDeclaration> variable)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	result = result | ResolveDataType(nullptr, variable->dataType);

	if (variable->variableType == VariableDeclarationType::METHOD)
	{
		result = result |
		         ResolveMethodDeclaration(std::dynamic_pointer_cast<MethodDeclaration>(variable));
	}
	else if (variable->variableType == VariableDeclarationType::MEMBER_VARIABLE)
	{
		result = result | ResolveMemberVariableDeclaration(
							  std::dynamic_pointer_cast<MemberVariableDeclaration>(variable));
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

void ResolveContext::SetPass(ResolvePass pass)
{
	this->pass = pass;

	switch (pass)
	{
	case ResolvePass::NONE:
		requiredTraversalLevel = TraversalLevel::NONE;
		break;
	case ResolvePass::DATA_TYPES:
	case ResolvePass::EXPRESSION:
	case ResolvePass::NEW:
		requiredTraversalLevel = TraversalLevel::EXPRESSION;
		break;
	default:
		STRICT_UNREACHABLE;
	}
}

PassResultFlags ResolveContext::ResolveIdentifiers()
{
	if (!IsTraversalRequired(TraversalLevel::DECLARATION))
	{
		return PassResultFlags::SUCCESS;
	}

	PassResultFlags result = PassResultFlags::SUCCESS;

	if (pass == ResolvePass::DATA_TYPES)
	{
		if (type->typeTemplate)
		{
			result = result | ResolveTemplateDeclaration(type->typeTemplate);
		}

		result = result | ResolveSuperTypes();
	}

	result = result | ResolveMembers();

	return result;
}

PassResultFlags ResolveUnitIdentifiers(PrintFunction print, BuildContext& context, Ref<Unit> unit)
{
	if (!unit->declaredType->IsType())
	{
		return PassResultFlags::SUCCESS;
	}

	ResolveContext resolve(&context, unit->name, print, &unit->unitMeta.lexer);
	resolve.unit = unit;
	resolve.type = std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType);

	resolve.SetPass(ResolvePass::DATA_TYPES);
	PassResultFlags result = resolve.ResolveIdentifiers();

	resolve.SetPass(ResolvePass::EXPRESSION);
	result = result | resolve.ResolveIdentifiers();

	resolve.SetPass(ResolvePass::NEW);
	result = result | resolve.ResolveIdentifiers();

	return result;
}

PassResultFlags ResolveIdentifiersPass::Run(PrintFunction print, BuildContext& context)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto module : context.GetModules())
	{
		for (auto unit : module->units)
		{
			result = result | ResolveUnitIdentifiers(print, context, unit);
		}
	}

	return result;
}
