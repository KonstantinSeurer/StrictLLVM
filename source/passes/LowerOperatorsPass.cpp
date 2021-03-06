
#include "LowerOperatorsPass.h"

LowerOperatorsPass::LowerOperatorsPass() : Pass("LowerOperatorsPass")
{
}

static bool IsMutatingBinaryOperator(OperatorType op)
{
	switch (op)
	{
	case OperatorType::PLUS_EQUAL:
	case OperatorType::MINUS_EQUAL:
	case OperatorType::MULTIPLY_EQUAL:
	case OperatorType::DIVIDE_EQUAL:
	case OperatorType::AND_EQUAL:
	case OperatorType::OR_EQUAL:
	case OperatorType::XOR_EQUAL:
	case OperatorType::SHIFT_LEFT_EQUAL:
	case OperatorType::SHIFT_RIGHT_EQUAL:
		return true;
	}
	return false;
}

static bool IsMutatingUnaryOperator(OperatorType op)
{
	switch (op)
	{
	case OperatorType::INCREMENT:
	case OperatorType::DECREMENT:
		return true;
	}
	return false;
}

void LowerOperatorsPass::LowerOperatorExpression(Ref<MethodDeclaration> method,
                                                 Ref<Expression>* pExpression)
{
	Ref<OperatorExpression> expression =
		std::dynamic_pointer_cast<OperatorExpression>(*pExpression);
	LowerExpression(method, &expression->a);
	if (expression->b && expression->b->expression)
	{
		LowerExpression(method, &expression->b->expression);
	}

	if (expression->operatorType == OperatorType::ASSIGN)
	{
		Ref<IdentifierExpression> identifier;
		Ref<Expression> context;

		if (expression->a->expressionType == ExpressionType::IDENTIFIER)
		{
			identifier = std::dynamic_pointer_cast<IdentifierExpression>(expression->a);
		}

		if (expression->a->expressionType == ExpressionType::OPERATOR)
		{
			Ref<OperatorExpression> op =
				std::dynamic_pointer_cast<OperatorExpression>(expression->a);
			if (op->operatorType == OperatorType::ACCESS)
			{
				identifier =
					std::dynamic_pointer_cast<IdentifierExpression>(expression->b->expression);
				context = expression->a;
			}
		}

		if (identifier)
		{
			assert(identifier->identifierExpressionMeta.destination);
			assert(identifier->identifierExpressionMeta.destination->type ==
			       ASTItemType::VARIABLE_DECLARATION);

			Ref<VariableDeclaration> destination = std::dynamic_pointer_cast<VariableDeclaration>(
				identifier->identifierExpressionMeta.destination);

			if (destination->variableType != VariableDeclarationType::MEMBER_VARIABLE)
			{
				return;
			}

			Ref<MemberVariableDeclaration> memberVariable =
				std::dynamic_pointer_cast<MemberVariableDeclaration>(destination);
			for (auto accessor : memberVariable->accessors)
			{
				if (accessor->methodType != MethodType::SETTER)
				{
					continue;
				}

				if (method == accessor)
				{
					continue;
				}

				assert(accessor->parameters.size() == 1);

				// TODO: Implement overloading -> don't always select the first one.

				Ref<CallExpression> call = Allocate<CallExpression>();
				call->callExpressionMeta.destination = accessor;
				call->callExpressionMeta.context = nullptr;
				call->method = identifier;
				call->arguments = {expression->b->expression};

				*pExpression = call;
				return;
			}
		}

		return;
	}

	if (!expression->expressionMeta.dataType)
	{
		return;
	}

	if (expression->expressionMeta.dataType->dataTypeType == DataTypeType::PRIMITIVE)
	{
		if (IsMutatingUnaryOperator(expression->operatorType))
		{
			Ref<LiteralExpression> one = Allocate<LiteralExpression>();
			one->data.type = TokenType::UINT_LITERAL;
			one->data.data.uintData = 1;
			one->expressionMeta.dataType = expression->expressionMeta.dataType;

			Ref<OperatorExpression> replacement = Allocate<OperatorExpression>();
			replacement->expressionMeta = expression->expressionMeta;
			replacement->a = expression->a;
			replacement->b = SecondOperand();
			replacement->b->expression = one;

			switch (expression->operatorType)
			{
			case OperatorType::INCREMENT:
				replacement->operatorType = OperatorType::PLUS_EQUAL;
				break;
			case OperatorType::DECREMENT:
				replacement->operatorType = OperatorType::MINUS_EQUAL;
				break;
			}

			Ref<Expression> replacementExpression = replacement;
			LowerOperatorExpression(method, &replacementExpression);
			*pExpression = replacementExpression;
			return;
		}

		if (IsMutatingBinaryOperator(expression->operatorType))
		{
			Ref<OperatorExpression> operation = Allocate<OperatorExpression>();
			operation->expressionMeta = expression->expressionMeta;
			operation->a = expression->a;
			operation->b = SecondOperand();
			operation->b->expression = expression->b->expression;

			switch (expression->operatorType)
			{
			case OperatorType::PLUS_EQUAL:
				operation->operatorType = OperatorType::PLUS;
				break;
			case OperatorType::MINUS_EQUAL:
				operation->operatorType = OperatorType::MINUS;
				break;
			case OperatorType::MULTIPLY_EQUAL:
				operation->operatorType = OperatorType::MULTIPLY;
				break;
			case OperatorType::DIVIDE_EQUAL:
				operation->operatorType = OperatorType::DIVIDE;
				break;
			case OperatorType::AND_EQUAL:
				operation->operatorType = OperatorType::AND;
				break;
			case OperatorType::OR_EQUAL:
				operation->operatorType = OperatorType::OR;
				break;
			case OperatorType::XOR_EQUAL:
				operation->operatorType = OperatorType::XOR;
				break;
			case OperatorType::SHIFT_LEFT_EQUAL:
				operation->operatorType = OperatorType::SHIFT_LEFT;
				break;
			case OperatorType::SHIFT_RIGHT_EQUAL:
				operation->operatorType = OperatorType::SHIFT_RIGHT;
				break;
			}

			Ref<Expression> operationExpression = operation;
			LowerOperatorExpression(method, &operationExpression);

			Ref<OperatorExpression> assignment = Allocate<OperatorExpression>();
			assignment->expressionMeta = expression->expressionMeta;
			assignment->operatorType = OperatorType::ASSIGN;
			assignment->a = expression->a;
			assignment->b = SecondOperand();
			assignment->b->expression = operationExpression;

			Ref<Expression> assignmentExpression = assignment;
			LowerOperatorExpression(method, &assignmentExpression);
			*pExpression = assignmentExpression;
			return;
		}
	}
}

void LowerOperatorsPass::LowerExpression(Ref<MethodDeclaration> method,
                                         Ref<Expression>* pExpression)
{
	Ref<Expression> expression = *pExpression;
	switch (expression->expressionType)
	{
	case ExpressionType::BRACKET: {
		Ref<BracketExpression> bracketExpression =
			std::dynamic_pointer_cast<BracketExpression>(expression);
		LowerExpression(method, &bracketExpression->expression);
		break;
	}
	case ExpressionType::CALL: {
		Ref<CallExpression> callExpression = std::dynamic_pointer_cast<CallExpression>(expression);
		LowerExpression(method, &callExpression->method);
		for (auto& argument : callExpression->arguments)
		{
			LowerExpression(method, &argument);
		}
		break;
	}
	case ExpressionType::NEW: {
		Ref<NewExpression> newExpression = std::dynamic_pointer_cast<NewExpression>(expression);
		for (auto& argument : newExpression->arguments)
		{
			LowerExpression(method, &argument);
		}
		break;
	}
	case ExpressionType::OPERATOR: {
		LowerOperatorExpression(method, pExpression);
		break;
	}
	case ExpressionType::TERNARY: {
		Ref<TernaryExpression> ternaryExpression =
			std::dynamic_pointer_cast<TernaryExpression>(expression);
		LowerExpression(method, &ternaryExpression->condition);
		LowerExpression(method, &ternaryExpression->thenExpression);
		LowerExpression(method, &ternaryExpression->elseExpression);
		break;
	}
	}
}

void LowerOperatorsPass::LowerStatement(Ref<MethodDeclaration> method, Ref<Statement> statement)
{
	switch (statement->statementType)
	{
	case StatementType::BLOCK: {
		Ref<BlockStatement> blockStatement = std::dynamic_pointer_cast<BlockStatement>(statement);
		for (auto subStatement : blockStatement->statements)
		{
			LowerStatement(method, subStatement);
		}
		break;
	}
	case StatementType::DELETE: {
		Ref<DeleteStatement> deleteStatement =
			std::dynamic_pointer_cast<DeleteStatement>(statement);
		LowerExpression(method, &deleteStatement->expression);
		break;
	}
	case StatementType::EXPRESSION: {
		Ref<ExpressionStatement> expressionStatement =
			std::dynamic_pointer_cast<ExpressionStatement>(statement);
		LowerExpression(method, &expressionStatement->expression);
		break;
	}
	case StatementType::FOR: {
		Ref<ForStatement> forStatement = std::dynamic_pointer_cast<ForStatement>(statement);
		LowerStatement(method, forStatement->startStatement);
		LowerExpression(method, &forStatement->condition);
		LowerExpression(method, &forStatement->incrementExpression);
		LowerStatement(method, forStatement->bodyStatement);
		break;
	}
	case StatementType::IF: {
		Ref<IfStatement> ifStatement = std::dynamic_pointer_cast<IfStatement>(statement);
		LowerExpression(method, &ifStatement->condition);
		LowerStatement(method, ifStatement->thenStatement);
		if (ifStatement->elseStatement)
		{
			LowerStatement(method, ifStatement->elseStatement);
		}
		break;
	}
	case StatementType::RETURN: {
		Ref<ReturnStatement> returnStatement =
			std::dynamic_pointer_cast<ReturnStatement>(statement);
		if (returnStatement->expression)
		{
			LowerExpression(method, &returnStatement->expression);
		}
		break;
	}
	case StatementType::VARIABLE_DECLARATION: {
		Ref<VariableDeclarationStatement> variableDeclarationStatement =
			std::dynamic_pointer_cast<VariableDeclarationStatement>(statement);
		if (variableDeclarationStatement->value)
		{
			LowerExpression(method, &variableDeclarationStatement->value);
		}
		break;
	}
	case StatementType::WHILE: {
		Ref<WhileStatement> whileStatement = std::dynamic_pointer_cast<WhileStatement>(statement);
		LowerExpression(method, &whileStatement->condition);
		LowerStatement(method, whileStatement->bodyStatement);
		break;
	}
	}
}

void LowerOperatorsPass::LowerMethodDeclaration(Ref<MethodDeclaration> method)
{
	if (method->body)
	{
		LowerStatement(method, method->body);
	}
}

void LowerOperatorsPass::LowerMemberVariableDeclaration(Ref<MemberVariableDeclaration> variable)
{
	for (auto accessor : variable->accessors)
	{
		LowerMethodDeclaration(accessor);
	}
}

void LowerOperatorsPass::LowerVariableDeclaration(Ref<VariableDeclaration> variable)
{
	if (variable->variableType == VariableDeclarationType::MEMBER_VARIABLE)
	{
		LowerMemberVariableDeclaration(
			std::dynamic_pointer_cast<MemberVariableDeclaration>(variable));
	}
	else if (variable->variableType == VariableDeclarationType::METHOD)
	{
		LowerMethodDeclaration(std::dynamic_pointer_cast<MethodDeclaration>(variable));
	}
}

void LowerOperatorsPass::LowerClass(Ref<ClassDeclaration> classDeclaration)
{
	if (classDeclaration->typeTemplate)
	{
		return;
	}

	for (auto member : classDeclaration->members)
	{
		LowerVariableDeclaration(member);
	}
}

PassResultFlags LowerOperatorsPass::Run(PrintFunction print, BuildContext& context)
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

			LowerClass(std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType));
		}
	}

	return result;
}
