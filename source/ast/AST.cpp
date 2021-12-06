
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

#define ENUM_VAR(indentation, name) Indentation(indentation) + #name + " = " + ::ToString(name) + "\n"
#define STRING_VAR(indentation, name) Indentation(indentation) + #name + " = " + name + "\n"
#define AST_VAR(indentation, name) Indentation(indentation) + #name + " = " + (name ? name->ToString(indentation) : "null\n")
#define PRIMITIVE_VAR(indentation, name) Indentation(indentation) + #name + " = " + std::to_string(name)

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
	return DataType::ToStringImplementation(indentation) + AST_VAR(indentation, value);
}

String Expression::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, expressionType);
}

String Statement::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, statementType);
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
	return VariableDeclaration::ToStringImplementation(indentation);
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
