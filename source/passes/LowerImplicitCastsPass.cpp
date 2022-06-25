
#include "LowerImplicitCastsPass.h"

void LowerImplicitCastsPass::LowerExpression(MethodDeclaration* method,
                                             Ref<Expression>* pExpression,
                                             Ref<DataType> expectedType)
{
	Ref<Expression> expression = *pExpression;

	if (expectedType && expression->expressionMeta.dataType &&
	    !TypeEquals(expression->expressionMeta.dataType.get(), expectedType.get()))
	{
		assert(CanCast(expression->expressionMeta.dataType.get(),
		               GetReferencedType(expectedType.get())));

		Ref<OperatorExpression> casts = Allocate<OperatorExpression>();
		casts->expressionMeta.dataType = expectedType;
		casts->expressionMeta.parentExpression = expression->expressionMeta.parentExpression;
		casts->expressionMeta.parentStatement = expression->expressionMeta.parentStatement;
		casts->operatorType = OperatorType::EXPLICIT_CAST;
		casts->a = expression;
		casts->b = SecondOperand();
		casts->b->dataType = expectedType;

		*pExpression = casts;
		expression->expressionMeta.parentExpression = casts.get();
	}

	switch (expression->expressionType)
	{
	case ExpressionType::BRACKET: {
		Ref<BracketExpression> bracketExpression =
			std::dynamic_pointer_cast<BracketExpression>(expression);
		LowerExpression(method, &bracketExpression->expression, nullptr);
		break;
	}
	case ExpressionType::CALL: {
		Ref<CallExpression> callExpression = std::dynamic_pointer_cast<CallExpression>(expression);
		LowerExpression(method, &callExpression->method, nullptr);
		if (callExpression->callExpressionMeta.destination->variableType ==
		    VariableDeclarationType::METHOD)
		{
			Ref<MethodDeclaration> callMethod = std::dynamic_pointer_cast<MethodDeclaration>(
				callExpression->callExpressionMeta.destination);

			for (UInt32 argumentIndex = 0; argumentIndex < callExpression->arguments.size();
			     argumentIndex++)
			{
				LowerExpression(method, &callExpression->arguments[argumentIndex],
				                callMethod->parameters[argumentIndex]->dataType);
			}
		}
		else
		{
			STRICT_UNREACHABLE;
		}
		break;
	}
	case ExpressionType::NEW: {
		Ref<NewExpression> newExpression = std::dynamic_pointer_cast<NewExpression>(expression);
		if (newExpression->newExpressionMeta.destination)
		{
			for (UInt32 argumentIndex = 0; argumentIndex < newExpression->arguments.size();
			     argumentIndex++)
			{
				LowerExpression(
					method, &newExpression->arguments[argumentIndex],
					newExpression->newExpressionMeta.destination->parameters[argumentIndex]
						->dataType);
			}
		}
		break;
	}
	case ExpressionType::OPERATOR: {
		Ref<OperatorExpression> operatorExpression =
			std::dynamic_pointer_cast<OperatorExpression>(expression);
		if (operatorExpression->operatorType != OperatorType::ARRAY_ACCESS &&
		    operatorExpression->operatorType != OperatorType::ACCESS)
		{
			LowerExpression(method, &operatorExpression->a,
			                operatorExpression->expressionMeta.dataType);
			if (operatorExpression->b && operatorExpression->b->expression)
			{
				LowerExpression(method, &operatorExpression->b->expression,
				                operatorExpression->expressionMeta.dataType);
			}
		}
		break;
	}
	case ExpressionType::TERNARY: {
		Ref<TernaryExpression> ternaryExpression =
			std::dynamic_pointer_cast<TernaryExpression>(expression);
		LowerExpression(method, &ternaryExpression->condition,
		                ternaryExpression->expressionMeta.dataType);
		LowerExpression(method, &ternaryExpression->thenExpression,
		                ternaryExpression->expressionMeta.dataType);
		LowerExpression(method, &ternaryExpression->elseExpression,
		                ternaryExpression->expressionMeta.dataType);
		break;
	}
	}
}

void LowerImplicitCastsPass::LowerStatement(MethodDeclaration* method, Statement* statement)
{
	switch (statement->statementType)
	{
	case StatementType::BLOCK: {
		BlockStatement* blockStatement = (BlockStatement*)statement;
		for (auto subStatement : blockStatement->statements)
		{
			LowerStatement(method, subStatement.get());
		}
		break;
	}
	case StatementType::DELETE: {
		DeleteStatement* deleteStatement = (DeleteStatement*)statement;
		LowerExpression(method, &deleteStatement->expression, nullptr);
		break;
	}
	case StatementType::EXPRESSION: {
		ExpressionStatement* expressionStatement = (ExpressionStatement*)statement;
		LowerExpression(method, &expressionStatement->expression, nullptr);
		break;
	}
	case StatementType::FOR: {
		ForStatement* forStatement = (ForStatement*)statement;
		LowerStatement(method, forStatement->startStatement.get());
		LowerExpression(method, &forStatement->condition, nullptr);
		LowerExpression(method, &forStatement->incrementExpression, nullptr);
		LowerStatement(method, forStatement->bodyStatement.get());
		break;
	}
	case StatementType::IF: {
		IfStatement* ifStatement = (IfStatement*)statement;
		LowerExpression(method, &ifStatement->condition, nullptr);
		LowerStatement(method, ifStatement->thenStatement.get());
		if (ifStatement->elseStatement)
		{
			LowerStatement(method, ifStatement->elseStatement.get());
		}
		break;
	}
	case StatementType::RETURN: {
		ReturnStatement* returnStatement = (ReturnStatement*)statement;
		if (returnStatement->expression)
		{
			LowerExpression(method, &returnStatement->expression, nullptr);
		}
		break;
	}
	case StatementType::VARIABLE_DECLARATION: {
		VariableDeclarationStatement* variableDeclarationStatement =
			(VariableDeclarationStatement*)statement;
		if (variableDeclarationStatement->value)
		{
			LowerExpression(method, &variableDeclarationStatement->value, nullptr);
		}
		break;
	}
	case StatementType::WHILE: {
		WhileStatement* whileStatement = (WhileStatement*)statement;
		LowerExpression(method, &whileStatement->condition, nullptr);
		LowerStatement(method, whileStatement->bodyStatement.get());
		break;
	}
	}
}

void LowerImplicitCastsPass::LowerMethodDeclaration(MethodDeclaration* method)
{
	if (method->body)
	{
		LowerStatement(method, method->body.get());
	}
}

void LowerImplicitCastsPass::LowerMemberVariableDeclaration(MemberVariableDeclaration* variable)
{
	for (auto accessor : variable->accessors)
	{
		LowerMethodDeclaration(accessor.get());
	}
}

void LowerImplicitCastsPass::LowerVariableDeclaration(VariableDeclaration* variable)
{
	if (variable->variableType == VariableDeclarationType::MEMBER_VARIABLE)
	{
		LowerMemberVariableDeclaration((MemberVariableDeclaration*)variable);
	}
	else if (variable->variableType == VariableDeclarationType::METHOD)
	{
		LowerMethodDeclaration((MethodDeclaration*)variable);
	}
}

void LowerImplicitCastsPass::LowerClass(ClassDeclaration* classDeclaration)
{
	if (classDeclaration->typeTemplate)
	{
		return;
	}

	for (auto member : classDeclaration->members)
	{
		LowerVariableDeclaration(member.get());
	}
}

PassResultFlags LowerImplicitCastsPass::Run(PrintFunction print, BuildContext& context)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto module : context.GetModules())
	{
		for (auto& specialization : module->moduleMeta.templateSpecializations)
		{
			LowerClass((ClassDeclaration*)specialization.second->declaredType.get());
		}

		for (auto unit : module->units)
		{
			if (unit->declaredType->declarationType != UnitDeclarationType::CLASS)
			{
				continue;
			}

			LowerClass((ClassDeclaration*)unit->declaredType.get());
		}
	}

	return result;
}