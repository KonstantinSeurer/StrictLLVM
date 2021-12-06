#ifndef SOURCE_AST_AST
#define SOURCE_AST_AST

#include "../Base.h"
#include "../Lexer.h"
#include "../JSON.h"

STRICT_ENUM(ASTItemType,
			NONE,
			MODULE,
			UNIT,
			CLASS_DECLARATION,
			ERROR_DECLARATION,
			TYPE_DECLARATION,
			TEMPLATE_DECLARATION,
			TEMPLATE,
			VARIABLE_DECLARATION,
			METHOD_DECLARATION,
			DATA_TYPE,
			EXPRESSION,
			STATEMENT)

class ASTItem
{
public:
	ASTItemType type;

public:
	ASTItem(ASTItemType type)
		: type(type)
	{
	}
};

STRICT_ENUM(DeclarationFlags,
			PRIVATE = 0,
			INTERNAL = 1,
			PROTECTED = 2,
			PUBLIC = 7,
			MUT = 8,
			IMPURE = 16)

STRICT_ENUM(DataTypeType,
			TYPE,
			PRIMITIVE,
			OBJECT,
			REFERENCE,
			POINTER,
			ARRAY)

class Template;

class DataType : public ASTItem
{
public:
	DataTypeType dataTypeType;
	DeclarationFlags flags;

public:
	DataType()
		: ASTItem(ASTItemType::DATA_TYPE)
	{
	}

	virtual ~DataType()
	{
	}
};

class PrimitiveType : public DataType
{
public:
	TokenType primitiveType;

public:
	PrimitiveType()
		: DataType()
	{
		dataTypeType = DataTypeType::PRIMITIVE;
	}

	PrimitiveType(TokenType primitiveType)
		: DataType(), primitiveType(primitiveType)
	{
		dataTypeType = DataTypeType::PRIMITIVE;
	}
};

class ObjectType : public DataType
{
public:
	String name;
	Ref<Template> typeTemplate;

public:
	ObjectType()
		: DataType()
	{
		dataTypeType = DataTypeType::OBJECT;
	}
};

class PointerType : public DataType
{
public:
	Ref<DataType> value;

public:
	PointerType()
		: DataType()
	{
	}
};

STRICT_ENUM(ExpressionType,
			LITERAL)

class Expression : public ASTItem
{
public:
	ExpressionType expressionType;

public:
	Expression()
		: ASTItem(ASTItemType::EXPRESSION)
	{
	}

	virtual ~Expression()
	{
	}
};

STRICT_ENUM(StatementType,
			NOP,
			BLOCK)

class Statement : public ASTItem
{
public:
	StatementType statementType;

public:
	Statement()
		: ASTItem(ASTItemType::STATEMENT)
	{
	}

	virtual ~Statement()
	{
	}
};

class Template : public ASTItem
{
public:
	Array<Ref<ASTItem>> arguments;

public:
	Template()
		: ASTItem(ASTItemType::TEMPLATE)
	{
	}
};

class UnitDeclaration : public ASTItem
{
public:
	DeclarationFlags flags;

public:
	UnitDeclaration(ASTItemType type)
		: ASTItem(type)
	{
	}

public:
	virtual JSON GetStructureJSON() const
	{
		return JSON(JSON::value_t::array);
	}
};

class ErrorDeclaration : public UnitDeclaration
{
public:
	Int32 value;
	bool hasValue;

public:
	ErrorDeclaration()
		: UnitDeclaration(ASTItemType::ERROR_DECLARATION)
	{
	}

public:
};

STRICT_ENUM(VariableDeclarationType,
			VARIABLE,
			MEMBER_VARIABLE,
			METHOD)

class VariableDeclaration : public ASTItem
{
public:
	DeclarationFlags flags;
	VariableDeclarationType variableType;
	String name;
	Ref<DataType> dataType;

public:
	VariableDeclaration()
		: ASTItem(ASTItemType::VARIABLE_DECLARATION)
	{
	}

	virtual ~VariableDeclaration()
	{
	}
};

STRICT_ENUM(MethodType,
			METHOD,
			CONSTRUCTOR,
			DESTRUCTOR,
			OPERATOR)

class MethodDeclaration : public VariableDeclaration
{
public:
	MethodType methodType;
	Array<Ref<VariableDeclaration>> parameters;

	Ref<Statement> body;
	Lexer tempBody;

public:
	MethodDeclaration()
		: VariableDeclaration()
	{
		variableType = VariableDeclarationType::METHOD;
	}
};

class ConstructorInitializer
{
public:
	String name;

	Ref<Expression> value;
	Lexer tempValue;
};

class ConstructorDeclaration : public MethodDeclaration
{
public:
	Array<ConstructorInitializer> initializers;

public:
	ConstructorDeclaration()
		: MethodDeclaration()
	{
		variableType = VariableDeclarationType::METHOD;
		methodType = MethodType::CONSTRUCTOR;
	}
};

class DestructorDeclaration : public MethodDeclaration
{
public:
public:
	DestructorDeclaration()
		: MethodDeclaration()
	{
		variableType = VariableDeclarationType::METHOD;
		methodType = MethodType::DESTRUCTOR;
	}
};

STRICT_ENUM(OperatorType,
			// Non mutating binary operators
			PLUS,
			MINUS,
			MULTIPLY,
			DIVIDE,
			AND,
			OR,
			XOR,
			GREATER,
			LESS,
			EQUAL,
			NOT_EQUAL,
			GREATER_EQUAL,
			LESS_EQUAL,
			// Mutating binary operators
			PLUS_EQUAL,
			MINUS_EQUAL,
			MULTIPLY_EQUAL,
			DIVIDE_EQUAL,
			AND_EQUAL,
			OR_EQUAL,
			XOR_EQUAL,
			// Non mutating unary operators
			NEGATIVE,
			NOT,
			INVERSE,
			CAST,
			// Mutating unary operators
			INCREMENT,
			DECREMENT)

class OperatorDeclaration : public MethodDeclaration
{
public:
	OperatorType operatorType;

public:
	OperatorDeclaration()
		: MethodDeclaration()
	{
		variableType = VariableDeclarationType::METHOD;
		methodType = MethodType::OPERATOR;
	}
};

class MemberVariableDeclaration : public VariableDeclaration
{
public:
public:
	MemberVariableDeclaration()
		: VariableDeclaration()
	{
		variableType = VariableDeclarationType::MEMBER_VARIABLE;
	}
};

class TemplateDeclaration : public ASTItem
{
public:
	Array<Ref<VariableDeclaration>> parameters;

public:
	TemplateDeclaration()
		: ASTItem(ASTItemType::TEMPLATE_DECLARATION)
	{
	}
};

class TypeDeclaration : public UnitDeclaration
{
public:
	Array<Ref<VariableDeclaration>> members;
	Ref<TemplateDeclaration> typeTemplate;
	Array<Ref<ObjectType>> superTypes;

public:
	TypeDeclaration()
		: UnitDeclaration(ASTItemType::TYPE_DECLARATION)
	{
	}

	TypeDeclaration(ASTItemType type)
		: UnitDeclaration(type)
	{
	}

public:
};

class ClassDeclaration : public TypeDeclaration
{
public:
	bool isSingleton;

public:
	ClassDeclaration()
		: TypeDeclaration(ASTItemType::CLASS_DECLARATION)
	{
	}

public:
};

class Unit : public ASTItem
{
public:
	Array<String> dependencyNames;
	String name;
	Ref<UnitDeclaration> declaredType;

public:
	Unit()
		: ASTItem(ASTItemType::UNIT)
	{
	}

	Unit(const JSON &structureJSON);

public:
	JSON GetStructureJSON() const;
};

STRICT_ENUM(ModuleType,
			STATIC,
			DYNAMIC,
			INLINE)

STRICT_ENUM(TargetFlags,
			NONE = 0,
			BIT32 = 1,
			BIT64 = 2,
			X86 = 4,
			LINUX = 8)

bool IsTargetActive(TargetFlags target, TargetFlags buildTarget);
TargetFlags JSONToTargetFlags(const JSON &json);

class Module
{
private:
	String name;
	Array<Ref<Unit>> units;
	Array<Ref<Module>> dependencies;

public:
	Module(const String &name, const Array<Ref<Unit>> &units, const Array<Ref<Module>> &dependencies)
		: name(name), units(units), dependencies(dependencies)
	{
	}

	Ref<Module> Create(const JSON &json);

	const String &getName() const
	{
		return name;
	}

	const Array<Ref<Unit>> &getUnits() const
	{
		return units;
	}

	Ref<Unit> getUnit(const String &name) const
	{
		for (auto unit : units)
		{
			if (unit->name == name)
			{
				return unit;
			}
		}

		return nullptr;
	}
};

#endif /* SOURCE_AST_AST */
