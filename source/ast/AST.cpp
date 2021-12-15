
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

String DataType::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, dataTypeType) + ENUM_VAR(indentation, flags);
}

String PrimitiveType::ToStringImplementation(UInt32 indentation) const
{
	return DataType::ToStringImplementation(indentation) + ENUM_VAR(indentation, primitiveType);
}

String ObjectType::ToStringImplementation(UInt32 indentation) const
{
	return DataType::ToStringImplementation(indentation) + STRING_VAR(indentation, name) + AST_VAR(indentation, typeTemplate);
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

String Expression::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, expressionType);
}

String LiteralExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + TOKEN_VAR(indentation, data);
}

String BracketExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

String VariableExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + STRING_VAR(indentation, name);
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

String CallExpression::ToStringImplementation(UInt32 indentation) const
{
	String result = Expression::ToStringImplementation(indentation) + AST_VAR(indentation, method) + Indentation(indentation) + "arguments = [\n";
	for (const auto &argument : arguments)
	{
		result += Indentation(indentation + 1) + argument->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

String TernaryExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + AST_VAR(indentation, condition) + AST_VAR(indentation, thenExpression) + AST_VAR(indentation, elseExpression);
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

String Statement::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, statementType);
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

String ExpressionStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

String IfStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, condition) + AST_VAR(indentation, thenStatement) + AST_VAR(indentation, elseStatement);
}

String ForStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, startStatement) + AST_VAR(indentation, condition) + AST_VAR(indentation, incrementExpression) + AST_VAR(indentation, bodyStatement);
}

String WhileStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, condition) + AST_VAR(indentation, bodyStatement) + PRIMITIVE_VAR(indentation, checkAfterBody);
}

String ReturnStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

String VariableDeclarationStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, declaration) + AST_VAR(indentation, value);
}

String DeleteStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
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

String UnitDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, flags);
}

String ErrorDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return UnitDeclaration::ToStringImplementation(indentation) + (hasValue ? PRIMITIVE_VAR(indentation, value) : "");
}

String VariableDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, flags) + ENUM_VAR(indentation, variableType) + STRING_VAR(indentation, name) + AST_VAR(indentation, dataType);
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

String ConstructorDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = MethodDeclaration::ToStringImplementation(indentation) + Indentation(indentation) + "initializers = [\n";
	for (const auto &initializer : initializers)
	{
		result += Indentation(indentation + 1) + initializer.name + ": " + (initializer.value ? initializer.value->ToString(indentation + 1) : "null\n");
	}
	return result + Indentation(indentation) + "]\n";
}

String OperatorDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return MethodDeclaration::ToStringImplementation(indentation) + ENUM_VAR(indentation, operatorType);
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

String TemplateDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = Indentation(indentation) + "parameters = [\n";
	for (const auto &parameter : parameters)
	{
		result += Indentation(indentation + 1) + parameter->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
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

String ClassDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return TypeDeclaration::ToStringImplementation(indentation) + PRIMITIVE_VAR(indentation, isSingleton);
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

Ref<Module> Module::Create(const JSON &json)
{
	return nullptr;
}

String Module::ToStringImplementation(UInt32 indentation) const
{
	return "";
}
