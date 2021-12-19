
#include "AST.h"

static String Indentation(UInt32 indentation)
{
	String result;
	for (UInt32 index = 0; index < indentation; index++)
	{
		result += '\t';
	}
	return result;
}

String ASTItem::ToString(UInt32 indentation) const
{
	return ::ToString(type) + ":\n" + ToStringImplementation(indentation + 1);
}

String ASTItem::ToStringImplementation(UInt32 indentation) const
{
	return "\n";
}

void ASTItem::CloneImplementation(Ref<ASTItem> target) const
{
	target->type = type;
}

static String ToString(const Token &token)
{
	String result = ToString(token.type);

	switch (token.type)
	{
	case TokenType::STRING_LITERAL:
	case TokenType::IDENTIFIER:
		result += String("(") + token.data.stringData + ")";
		break;
	case TokenType::INT_LITERAL:
		result += "(" + std::to_string(token.data.intData) + ")";
		break;
	case TokenType::UINT_LITERAL:
		result += "(" + std::to_string(token.data.uintData) + ")";
		break;
	case TokenType::FLOAT_LITERAL:
		result += "(" + std::to_string(token.data.floatData) + ")";
		break;
	default:
		break;
	}

	return result;
}

#define ENUM_VAR(indentation, name) Indentation(indentation) + #name + " = " + ::ToString(name) + "\n"
#define STRING_VAR(indentation, name) Indentation(indentation) + #name + " = " + name + "\n"
#define AST_VAR(indentation, name) Indentation(indentation) + #name + " = " + (name ? name->ToString(indentation) : "null\n")
#define PRIMITIVE_VAR(indentation, name) Indentation(indentation) + #name + " = " + std::to_string(name)
#define TOKEN_VAR(indentation, name) Indentation(indentation) + #name + " = " + ::ToString(name) + "\n"

#define CLONE_METHOD(Type, ...)                         \
	Ref<ASTItem> Type::Clone() const                    \
	{                                                   \
		Ref<Type> result = Allocate<Type>(__VA_ARGS__); \
		Type::CloneImplementation(result);              \
		return result;                                  \
	}

String DataType::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, dataTypeType) + ENUM_VAR(indentation, flags);
}

CLONE_METHOD(DataType)

void DataType::CloneImplementation(Ref<DataType> target) const
{
	ASTItem::CloneImplementation(target);
	target->dataTypeType = dataTypeType;
	target->flags = flags;
}

String PrimitiveType::ToStringImplementation(UInt32 indentation) const
{
	return DataType::ToStringImplementation(indentation) + ENUM_VAR(indentation, primitiveType);
}

CLONE_METHOD(PrimitiveType)

void PrimitiveType::CloneImplementation(Ref<PrimitiveType> target) const
{
	DataType::CloneImplementation(target);
	target->primitiveType = primitiveType;
}

String ObjectType::ToStringImplementation(UInt32 indentation) const
{
	return DataType::ToStringImplementation(indentation) + STRING_VAR(indentation, name) + AST_VAR(indentation, typeTemplate);
}

CLONE_METHOD(ObjectType)

void ObjectType::CloneImplementation(Ref<ObjectType> target) const
{
	DataType::CloneImplementation(target);
	target->name = name;
	if (typeTemplate)
	{
		target->typeTemplate = std::dynamic_pointer_cast<Template>(typeTemplate->Clone());
	}
}

String PointerType::ToStringImplementation(UInt32 indentation) const
{
	String result = DataType::ToStringImplementation(indentation) + AST_VAR(indentation, value);
	if (dataTypeType == DataTypeType::ARRAY)
	{
		result += AST_VAR(indentation, arrayLength);
	}
	return result;
}

CLONE_METHOD(PointerType)

void PointerType::CloneImplementation(Ref<PointerType> target) const
{
	DataType::CloneImplementation(target);
	target->value = std::dynamic_pointer_cast<DataType>(value->Clone());
	if (arrayLength)
	{
		target->arrayLength = std::dynamic_pointer_cast<Expression>(arrayLength->Clone());
	}
}

String Expression::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, expressionType);
}

CLONE_METHOD(Expression, expressionType)

void Expression::CloneImplementation(Ref<Expression> target) const
{
	ASTItem::CloneImplementation(target);
	target->expressionType = expressionType;
}

String LiteralExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + TOKEN_VAR(indentation, data);
}

CLONE_METHOD(LiteralExpression)

void LiteralExpression::CloneImplementation(Ref<LiteralExpression> target) const
{
	Expression::CloneImplementation(target);
	target->data = data;
}

String BracketExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

CLONE_METHOD(BracketExpression)

void BracketExpression::CloneImplementation(Ref<BracketExpression> target) const
{
	Expression::CloneImplementation(target);
	target->expression = std::dynamic_pointer_cast<Expression>(expression->Clone());
}

String VariableExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + STRING_VAR(indentation, name);
}

CLONE_METHOD(VariableExpression)

void VariableExpression::CloneImplementation(Ref<VariableExpression> target) const
{
	Expression::CloneImplementation(target);
	target->name = name;
}

String OperatorExpression::ToStringImplementation(UInt32 indentation) const
{
	String result = Expression::ToStringImplementation(indentation) + ENUM_VAR(indentation, operatorType) + AST_VAR(indentation, a) + Indentation(indentation) + "b:\n";
	if (b)
	{
		result += AST_VAR(indentation + 1, b->expression) + AST_VAR(indentation + 1, b->dataType);
	}
	else
	{
		result += Indentation(indentation + 1) + "null\n";
	}
	return result;
}

CLONE_METHOD(OperatorExpression)

void OperatorExpression::CloneImplementation(Ref<OperatorExpression> target) const
{
	Expression::CloneImplementation(target);
	target->operatorType = operatorType;
	target->a = std::dynamic_pointer_cast<Expression>(a->Clone());
	if (b)
	{
		target->b = Allocate<SecondOperand>();
		if (b->dataType)
		{
			target->b->dataType = std::dynamic_pointer_cast<DataType>(b->dataType->Clone());
		}
		if (b->expression)
		{
			target->b->expression = std::dynamic_pointer_cast<Expression>(b->expression->Clone());
		}
	}
}

String CallExpression::ToStringImplementation(UInt32 indentation) const
{
	String result = Expression::ToStringImplementation(indentation) + AST_VAR(indentation, method) + Indentation(indentation) + "arguments = [\n";
	for (const auto &argument : arguments)
	{
		result += Indentation(indentation + 1) + argument->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(CallExpression)

void CallExpression::CloneImplementation(Ref<CallExpression> target) const
{
	Expression::CloneImplementation(target);
	target->method = std::dynamic_pointer_cast<Expression>(method->Clone());
	target->arguments.resize(arguments.size());
	for (UInt64 argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
	{
		target->arguments[argumentIndex] = std::dynamic_pointer_cast<Expression>(arguments[argumentIndex]->Clone());
	}
}

String TernaryExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + AST_VAR(indentation, condition) + AST_VAR(indentation, thenExpression) + AST_VAR(indentation, elseExpression);
}

CLONE_METHOD(TernaryExpression)

void TernaryExpression::CloneImplementation(Ref<TernaryExpression> target) const
{
	Expression::CloneImplementation(target);
	target->condition = std::dynamic_pointer_cast<Expression>(condition->Clone());
	target->thenExpression = std::dynamic_pointer_cast<Expression>(thenExpression->Clone());
	target->elseExpression = std::dynamic_pointer_cast<Expression>(elseExpression->Clone());
}

String NewExpression::ToStringImplementation(UInt32 indentation) const
{
	String result = Expression::ToStringImplementation(indentation) + AST_VAR(indentation, dataType) + Indentation(indentation) + "arguments = [\n";
	for (const auto &argument : arguments)
	{
		result += Indentation(indentation + 1) + argument->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(NewExpression)

void NewExpression::CloneImplementation(Ref<NewExpression> target) const
{
	Expression::CloneImplementation(target);
	target->dataType = std::dynamic_pointer_cast<DataType>(dataType->Clone());
	target->arguments.resize(arguments.size());
	for (UInt64 argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
	{
		target->arguments[argumentIndex] = std::dynamic_pointer_cast<Expression>(arguments[argumentIndex]->Clone());
	}
}

String Statement::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, statementType);
}

CLONE_METHOD(Statement, statementType)

void Statement::CloneImplementation(Ref<Statement> target) const
{
	ASTItem::CloneImplementation(target);
	target->statementType = statementType;
}

String BlockStatement::ToStringImplementation(UInt32 indentation) const
{
	String result = Statement::ToStringImplementation(indentation) + Indentation(indentation) + "statements = [\n";
	for (const auto &statement : statements)
	{
		result += Indentation(indentation + 1) + statement->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(BlockStatement)

void BlockStatement::CloneImplementation(Ref<BlockStatement> target) const
{
	Statement::CloneImplementation(target);
	target->statements.resize(statements.size());
	for (UInt64 statementIndex = 0; statementIndex < statements.size(); statementIndex++)
	{
		target->statements[statementIndex] = std::dynamic_pointer_cast<Statement>(statements[statementIndex]->Clone());
	}
}

String ExpressionStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

CLONE_METHOD(ExpressionStatement)

void ExpressionStatement::CloneImplementation(Ref<ExpressionStatement> target) const
{
	Statement::CloneImplementation(target);
	target->expression = std::dynamic_pointer_cast<Expression>(expression->Clone());
}

String IfStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, condition) + AST_VAR(indentation, thenStatement) + AST_VAR(indentation, elseStatement);
}

CLONE_METHOD(IfStatement)

void IfStatement::CloneImplementation(Ref<IfStatement> target) const
{
	Statement::CloneImplementation(target);
	target->condition = std::dynamic_pointer_cast<Expression>(condition->Clone());
	target->thenStatement = std::dynamic_pointer_cast<Statement>(thenStatement->Clone());
	if (elseStatement)
	{
		target->elseStatement = std::dynamic_pointer_cast<Statement>(elseStatement->Clone());
	}
}

String ForStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, startStatement) + AST_VAR(indentation, condition) + AST_VAR(indentation, incrementExpression) + AST_VAR(indentation, bodyStatement);
}

CLONE_METHOD(ForStatement)

void ForStatement::CloneImplementation(Ref<ForStatement> target) const
{
	Statement::CloneImplementation(target);
	target->startStatement = std::dynamic_pointer_cast<Statement>(startStatement->Clone());
	target->condition = std::dynamic_pointer_cast<Expression>(condition->Clone());
	target->incrementExpression = std::dynamic_pointer_cast<Expression>(incrementExpression->Clone());
	target->bodyStatement = std::dynamic_pointer_cast<Statement>(bodyStatement->Clone());
}

String WhileStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, condition) + AST_VAR(indentation, bodyStatement) + PRIMITIVE_VAR(indentation, checkAfterBody);
}

CLONE_METHOD(WhileStatement)

void WhileStatement::CloneImplementation(Ref<WhileStatement> target) const
{
	Statement::CloneImplementation(target);
	target->condition = std::dynamic_pointer_cast<Expression>(condition->Clone());
	target->bodyStatement = std::dynamic_pointer_cast<Statement>(bodyStatement->Clone());
	target->checkAfterBody = checkAfterBody;
}

String ReturnStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

CLONE_METHOD(ReturnStatement)

void ReturnStatement::CloneImplementation(Ref<ReturnStatement> target) const
{
	Statement::CloneImplementation(target);
	target->expression = std::dynamic_pointer_cast<Expression>(expression->Clone());
}

String VariableDeclarationStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, declaration) + AST_VAR(indentation, value);
}

CLONE_METHOD(VariableDeclarationStatement)

void VariableDeclarationStatement::CloneImplementation(Ref<VariableDeclarationStatement> target) const
{
	Statement::CloneImplementation(target);
	target->declaration = std::dynamic_pointer_cast<VariableDeclaration>(declaration->Clone());
	target->value = std::dynamic_pointer_cast<Expression>(value->Clone());
}

String DeleteStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

CLONE_METHOD(DeleteStatement)

void DeleteStatement::CloneImplementation(Ref<DeleteStatement> target) const
{
	Statement::CloneImplementation(target);
	target->expression = std::dynamic_pointer_cast<Expression>(expression->Clone());
}

String Template::ToStringImplementation(UInt32 indentation) const
{
	String result = Indentation(indentation) + "arguments = [\n";
	for (const auto &argument : arguments)
	{
		result += Indentation(indentation + 1) + argument->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(Template)

void Template::CloneImplementation(Ref<Template> target) const
{
	ASTItem::CloneImplementation(target);
	target->arguments.resize(arguments.size());
	for (UInt64 argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
	{
		target->arguments[argumentIndex] = arguments[argumentIndex]->Clone();
	}
}

String UnitDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, declarationType) + ENUM_VAR(indentation, flags);
}

CLONE_METHOD(UnitDeclaration, declarationType)

void UnitDeclaration::CloneImplementation(Ref<UnitDeclaration> target) const
{
	ASTItem::CloneImplementation(target);
	target->flags = flags;
	target->declarationType = declarationType;
}

String ErrorDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return UnitDeclaration::ToStringImplementation(indentation) + (hasValue ? PRIMITIVE_VAR(indentation, value) : "");
}

CLONE_METHOD(ErrorDeclaration)

void ErrorDeclaration::CloneImplementation(Ref<ErrorDeclaration> target) const
{
	UnitDeclaration::CloneImplementation(target);
	target->value = value;
	target->hasValue = hasValue;
}

String VariableDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, flags) + ENUM_VAR(indentation, variableType) + STRING_VAR(indentation, name) + AST_VAR(indentation, dataType);
}

CLONE_METHOD(VariableDeclaration)

void VariableDeclaration::CloneImplementation(Ref<VariableDeclaration> target) const
{
	ASTItem::CloneImplementation(target);
	target->flags = flags;
	target->variableType = variableType;
	target->name = name;
	target->dataType = std::dynamic_pointer_cast<DataType>(dataType->Clone());
}

String MethodDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = VariableDeclaration::ToStringImplementation(indentation) + ENUM_VAR(indentation, methodType) + Indentation(indentation) + "parameters = [\n";
	for (const auto &parameter : parameters)
	{
		result += Indentation(indentation + 1) + parameter->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n" + AST_VAR(indentation, body);
}

CLONE_METHOD(MethodDeclaration, methodType)

void MethodDeclaration::CloneImplementation(Ref<MethodDeclaration> target) const
{
	VariableDeclaration::CloneImplementation(target);
	target->methodType = methodType;
	target->parameters.resize(parameters.size());
	for (UInt64 parameterIndex = 0; parameterIndex < parameters.size(); parameterIndex++)
	{
		target->parameters[parameterIndex] = std::dynamic_pointer_cast<VariableDeclaration>(parameters[parameterIndex]->Clone());
	}
	if (body)
	{
		target->body = std::dynamic_pointer_cast<Statement>(body->Clone());
	}
}

String ConstructorDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = MethodDeclaration::ToStringImplementation(indentation) + Indentation(indentation) + "initializers = [\n";
	for (const auto &initializer : initializers)
	{
		result += Indentation(indentation + 1) + initializer.name + ": " + (initializer.value ? initializer.value->ToString(indentation + 1) : "null\n");
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(ConstructorDeclaration)

void ConstructorDeclaration::CloneImplementation(Ref<ConstructorDeclaration> target) const
{
	MethodDeclaration::CloneImplementation(target);
	target->initializers.resize(initializers.size());
	for (UInt64 initializerIndex = 0; initializerIndex < initializers.size(); initializerIndex++)
	{
		target->initializers[initializerIndex].name = initializers[initializerIndex].name;
		target->initializers[initializerIndex].value = std::dynamic_pointer_cast<Expression>(initializers[initializerIndex].value->Clone());
	}
}

String OperatorDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return MethodDeclaration::ToStringImplementation(indentation) + ENUM_VAR(indentation, operatorType);
}

CLONE_METHOD(OperatorDeclaration)

void OperatorDeclaration::CloneImplementation(Ref<OperatorDeclaration> target) const
{
	MethodDeclaration::CloneImplementation(target);
	target->operatorType = operatorType;
}

String MemberVariableDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = VariableDeclaration::ToStringImplementation(indentation) + AST_VAR(indentation, value) + Indentation(indentation) + "accessors = [\n";
	for (const auto &accessor : accessors)
	{
		result += Indentation(indentation + 1) + accessor->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(MemberVariableDeclaration)

void MemberVariableDeclaration::CloneImplementation(Ref<MemberVariableDeclaration> target) const
{
	VariableDeclaration::CloneImplementation(target);
	target->accessors.resize(accessors.size());
	for (UInt64 accessorIndex = 0; accessorIndex < accessors.size(); accessorIndex++)
	{
		target->accessors[accessorIndex] = std::dynamic_pointer_cast<MethodDeclaration>(accessors[accessorIndex]->Clone());
	}
	if (value)
	{
		target->value = std::dynamic_pointer_cast<Expression>(value->Clone());
	}
}

String TemplateDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = Indentation(indentation) + "parameters = [\n";
	for (const auto &parameter : parameters)
	{
		result += Indentation(indentation + 1) + parameter->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(TemplateDeclaration)

void TemplateDeclaration::CloneImplementation(Ref<TemplateDeclaration> target) const
{
	ASTItem::CloneImplementation(target);
	target->parameters.resize(parameters.size());
	for (UInt64 parameterIndex = 0; parameterIndex < parameters.size(); parameterIndex++)
	{
		target->parameters[parameterIndex] = std::dynamic_pointer_cast<VariableDeclaration>(parameters[parameterIndex]->Clone());
	}
}

String TypeDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = Indentation(indentation) + "members = [\n";
	for (const auto &member : members)
	{
		result += Indentation(indentation + 1) + member->ToString(indentation + 1);
	}
	result += Indentation(indentation) + "]\n" + AST_VAR(indentation, typeTemplate) + Indentation(indentation) + "superTypes = [\n";
	for (const auto &superType : superTypes)
	{
		result += Indentation(indentation + 1) + superType->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(TypeDeclaration)

void TypeDeclaration::CloneImplementation(Ref<TypeDeclaration> target) const
{
	UnitDeclaration::CloneImplementation(target);
	target->members.resize(members.size());
	for (UInt64 memberIndex = 0; memberIndex < members.size(); memberIndex++)
	{
		target->members[memberIndex] = std::dynamic_pointer_cast<VariableDeclaration>(members[memberIndex]->Clone());
	}
	if (typeTemplate)
	{
		target->typeTemplate = std::dynamic_pointer_cast<TemplateDeclaration>(typeTemplate->Clone());
	}
	target->superTypes.resize(superTypes.size());
	for (UInt64 superTypeIndex = 0; superTypeIndex < superTypes.size(); superTypeIndex++)
	{
		target->superTypes[superTypeIndex] = std::dynamic_pointer_cast<ObjectType>(superTypes[superTypeIndex]->Clone());
	}
}

String ClassDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return TypeDeclaration::ToStringImplementation(indentation) + PRIMITIVE_VAR(indentation, isSingleton);
}

CLONE_METHOD(ClassDeclaration)

void ClassDeclaration::CloneImplementation(Ref<ClassDeclaration> target) const
{
	TypeDeclaration::CloneImplementation(target);
	target->isSingleton = isSingleton;
}

Unit::Unit(const JSON &structureJSON)
	: ASTItem(ASTItemType::UNIT)
{
	name = structureJSON["name"];

	for (const JSON &dependencyName : structureJSON["dependencies"])
	{
		dependencyNames.push_back(String(dependencyName));
	}
}

JSON Unit::GetStructureJSON() const
{
	JSON result;
	JSON dependenciesJSON(JSON::value_t::array);

	for (UInt64 dependencyIndex = 0; dependencyIndex < dependencyNames.size(); dependencyIndex++)
	{
		dependenciesJSON[dependencyIndex] = dependencyNames[dependencyIndex];
	}

	result["dependencies"] = dependenciesJSON;
	result["name"] = name;

	return result;
}

String Unit::ToStringImplementation(UInt32 indentation) const
{
	String result = Indentation(indentation) + "dependencyNames = [\n";
	for (const auto &dependencyName : dependencyNames)
	{
		result += Indentation(indentation + 1) + dependencyName + "\n";
	}
	return result + Indentation(indentation) + "]\n" + STRING_VAR(indentation, name) + AST_VAR(indentation, declaredType);
}

CLONE_METHOD(Unit)

void Unit::CloneImplementation(Ref<Unit> target) const
{
	ASTItem::CloneImplementation(target);
	target->dependencyNames = dependencyNames;
	target->name = name;
	target->declaredType = std::dynamic_pointer_cast<UnitDeclaration>(declaredType->Clone());
}

bool IsTargetActive(TargetFlags target, TargetFlags buildTarget)
{
	return (target & buildTarget) == target; // target must be a subset of buildTarget to be active
}

TargetFlags JSONToTargetFlags(const JSON &json)
{
	TargetFlags flags = TargetFlags::NONE;

	for (const auto &flag : json)
	{
		flags = flags | StringToTargetFlags(String(flag));
	}

	return flags;
}

String Module::ToStringImplementation(UInt32 indentation) const
{
	return "";
}

CLONE_METHOD(Module, name)

void Module::CloneImplementation(Ref<Module> target) const
{
	ASTItem::CloneImplementation(target);
	target->name = name;
	target->units.resize(units.size());
	for (UInt64 unitIndex = 0; unitIndex < units.size(); unitIndex++)
	{
		target->units[unitIndex] = std::dynamic_pointer_cast<Unit>(units[unitIndex]->Clone());
	}
	target->dependencies.resize(dependencies.size());
	for (UInt64 dependencyIndex = 0; dependencyIndex < dependencies.size(); dependencyIndex++)
	{
		target->dependencies[dependencyIndex] = std::dynamic_pointer_cast<Module>(dependencies[dependencyIndex]->Clone());
	}
}
