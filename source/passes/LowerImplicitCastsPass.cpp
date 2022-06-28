
#include "LowerImplicitCastsPass.h"

void LowerImplicitCastsPass::LowerExpression(Ref<Expression>* pExpression,
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
		LowerExpression(&bracketExpression->expression, nullptr);
		break;
	}
	case ExpressionType::CALL: {
		Ref<CallExpression> callExpression = std::dynamic_pointer_cast<CallExpression>(expression);
		LowerExpression(&callExpression->method, nullptr);
		if (callExpression->callExpressionMeta.destination->variableType ==
		    VariableDeclarationType::METHOD)
		{
			Ref<MethodDeclaration> callMethod = std::dynamic_pointer_cast<MethodDeclaration>(
				callExpression->callExpressionMeta.destination);

			for (UInt32 argumentIndex = 0; argumentIndex < callExpression->arguments.size();
			     argumentIndex++)
			{
				LowerExpression(&callExpression->arguments[argumentIndex],
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
					&newExpression->arguments[argumentIndex],
					newExpression->newExpressionMeta.destination->parameters[argumentIndex]
						->dataType);
			}
		}
		break;
	}
	case ExpressionType::OPERATOR: {
		Ref<OperatorExpression> operatorExpression =
			std::dynamic_pointer_cast<OperatorExpression>(expression);

		Ref<DataType> expectedType = operatorExpression->expressionMeta.dataType;
		if (operatorExpression->operatorType == OperatorType::ARRAY_ACCESS ||
		    operatorExpression->operatorType == OperatorType::ACCESS)
		{
			expectedType = nullptr;
		}

		LowerExpression(&operatorExpression->a, expectedType);
		if (operatorExpression->b && operatorExpression->b->expression)
		{
			LowerExpression(&operatorExpression->b->expression, expectedType);
		}
		break;
	}
	case ExpressionType::TERNARY: {
		Ref<TernaryExpression> ternaryExpression =
			std::dynamic_pointer_cast<TernaryExpression>(expression);
		LowerExpression(&ternaryExpression->condition, ternaryExpression->expressionMeta.dataType);
		LowerExpression(&ternaryExpression->thenExpression,
		                ternaryExpression->expressionMeta.dataType);
		LowerExpression(&ternaryExpression->elseExpression,
		                ternaryExpression->expressionMeta.dataType);
		break;
	}
	}
}

void LowerImplicitCastsPass::LowerStatement(Statement* statement)
{
	switch (statement->statementType)
	{
	case StatementType::BLOCK: {
		BlockStatement* blockStatement = (BlockStatement*)statement;
		for (auto subStatement : blockStatement->statements)
		{
			LowerStatement(subStatement.get());
		}
		break;
	}
	case StatementType::DELETE: {
		DeleteStatement* deleteStatement = (DeleteStatement*)statement;
		LowerExpression(&deleteStatement->expression, nullptr);
		break;
	}
	case StatementType::EXPRESSION: {
		ExpressionStatement* expressionStatement = (ExpressionStatement*)statement;
		LowerExpression(&expressionStatement->expression, nullptr);
		break;
	}
	case StatementType::FOR: {
		ForStatement* forStatement = (ForStatement*)statement;
		LowerStatement(forStatement->startStatement.get());
		LowerExpression(&forStatement->condition, nullptr);
		LowerExpression(&forStatement->incrementExpression, nullptr);
		LowerStatement(forStatement->bodyStatement.get());
		break;
	}
	case StatementType::IF: {
		IfStatement* ifStatement = (IfStatement*)statement;
		LowerExpression(&ifStatement->condition, nullptr);
		LowerStatement(ifStatement->thenStatement.get());
		if (ifStatement->elseStatement)
		{
			LowerStatement(ifStatement->elseStatement.get());
		}
		break;
	}
	case StatementType::RETURN: {
		ReturnStatement* returnStatement = (ReturnStatement*)statement;
		if (returnStatement->expression)
		{
			LowerExpression(&returnStatement->expression, nullptr);
		}
		break;
	}
	case StatementType::VARIABLE_DECLARATION: {
		VariableDeclarationStatement* variableDeclarationStatement =
			(VariableDeclarationStatement*)statement;
		if (variableDeclarationStatement->value)
		{
			LowerExpression(&variableDeclarationStatement->value,
			                variableDeclarationStatement->declaration->dataType);
		}
		break;
	}
	case StatementType::WHILE: {
		WhileStatement* whileStatement = (WhileStatement*)statement;
		LowerExpression(&whileStatement->condition, nullptr);
		LowerStatement(whileStatement->bodyStatement.get());
		break;
	}
	}
}

void LowerImplicitCastsPass::LowerMethodDeclaration(MethodDeclaration* method)
{
	if (method->body)
	{
		LowerStatement(method->body.get());
	}
}

void LowerImplicitCastsPass::LowerMemberVariableDeclaration(MemberVariableDeclaration* variable)
{
	if (variable->value)
	{
		LowerExpression(&variable->value, variable->dataType);
	}

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