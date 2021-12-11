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

	virtual ~ASTItem()
	{
	}

	String ToString(UInt32 indentation) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

STRICT_FLAGS(DeclarationFlags,
			 PRIVATE = 0,
			 INTERNAL = 1,
			 PROTECTED = 2,
			 PUBLIC = 7,
			 MUT = 8,
			 IMPURE = 16,
			 VIRTUAL = 32)

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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class Expression;
class Statement;

class Template : public ASTItem
{
public:
	Array<Ref<ASTItem>> arguments;

public:
	Template()
		: ASTItem(ASTItemType::TEMPLATE)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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
		: ASTItem(ASTItemType::VARIABLE_DECLARATION), variableType(VariableDeclarationType::VARIABLE), flags(DeclarationFlags::PRIVATE)
	{
	}

	virtual ~VariableDeclaration()
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

STRICT_ENUM(MethodType,
			METHOD,
			CONSTRUCTOR,
			DESTRUCTOR,
			OPERATOR,
			GETTER,
			SETTER)

class MethodDeclaration : public VariableDeclaration
{
public:
	MethodType methodType;
	Array<Ref<VariableDeclaration>> parameters;

	Ref<Statement> body;
	Optional<Lexer> tempBody;

public:
	MethodDeclaration()
		: VariableDeclaration()
	{
		variableType = VariableDeclarationType::METHOD;
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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
			SHIFT_LEFT,
			SHIFT_RIGHT,
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
			// Misc binary operators
			ARRAY_ACCESS,
			// Non mutating unary operators
			NEGATIVE,
			NOT,
			INVERSE,
			IMPLICIT_CAST,
			EXPLICIT_CAST,
			// Mutating unary operators
			INCREMENT,
			DECREMENT,
			// Internal operators
			ACCESS)

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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class MemberVariableDeclaration : public VariableDeclaration
{
public:
	Array<Ref<MethodDeclaration>> accessors;
	Ref<Expression> value;

public:
	MemberVariableDeclaration()
		: VariableDeclaration()
	{
		variableType = VariableDeclarationType::MEMBER_VARIABLE;
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

	ClassDeclaration(bool isSingleton)
		: TypeDeclaration(ASTItemType::CLASS_DECLARATION), isSingleton(isSingleton)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

STRICT_ENUM(ModuleType,
			STATIC,
			DYNAMIC,
			INLINE)

STRICT_FLAGS(TargetFlags,
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

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

STRICT_ENUM(ExpressionType,
			LITERAL,
			VARIABLE,
			OPERATOR,
			CALL,
			TERNARY)

class Expression : public ASTItem
{
public:
	ExpressionType expressionType;

public:
	Expression(ExpressionType expressionType)
		: ASTItem(ASTItemType::EXPRESSION), expressionType(expressionType)
	{
	}

	virtual ~Expression()
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class LiteralExpression : public Expression
{
public:
	Token data;

public:
	LiteralExpression()
		: Expression(ExpressionType::LITERAL)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class VariableExpression : public Expression
{
public:
	String name;

public:
	VariableExpression()
		: Expression(ExpressionType::VARIABLE)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

struct SecondOperand
{
public:
	Ref<Expression> expression;
	Ref<DataType> dataType;
};

class OperatorExpression : public Expression
{
public:
	Ref<Expression> a;
	Ref<SecondOperand> b; // optional

	OperatorType operatorType;

public:
	OperatorExpression()
		: Expression(ExpressionType::OPERATOR)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class CallExpression : public Expression
{
public:
	Ref<Expression> method;
	Array<Ref<Expression>> arguments;

public:
	CallExpression()
		: Expression(ExpressionType::CALL)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class TernaryExpression : public Expression
{
public:
	Ref<Expression> condition;
	Ref<Expression> thenExpression;
	Ref<Expression> elseExpression;

public:
	TernaryExpression()
		: Expression(ExpressionType::TERNARY)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

STRICT_ENUM(StatementType,
			NOP,
			BLOCK,
			EXPRESSION,
			IF,
			FOR,
			WHILE,
			RETURN,
			BREAK,
			CONTINUE,
			VARIABLE_DECLARATION)

class Statement : public ASTItem
{
public:
	StatementType statementType;

public:
	Statement(StatementType statementType)
		: ASTItem(ASTItemType::STATEMENT), statementType(statementType)
	{
	}

	virtual ~Statement()
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class BlockStatement : public Statement
{
public:
	Array<Ref<Statement>> statements;

public:
	BlockStatement()
		: Statement(StatementType::BLOCK)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class ExpressionStatement : public Statement
{
public:
	Ref<Expression> expression;

public:
	ExpressionStatement()
		: Statement(StatementType::EXPRESSION)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class IfStatement : public Statement
{
public:
	Ref<Expression> condition;
	Ref<Statement> thenStatement;
	Ref<Statement> elseStatement;

public:
	IfStatement()
		: Statement(StatementType::IF)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class ForStatement : public Statement
{
public:
	Ref<Statement> startStatement;
	Ref<Expression> condition;
	Ref<Expression> incrementExpression;
	Ref<Statement> bodyStatement;

public:
	ForStatement()
		: Statement(StatementType::FOR)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class WhileStatement : public Statement
{
public:
	Ref<Expression> condition;
	Ref<Statement> bodyStatement;
	bool checkAfterBody;

public:
	WhileStatement()
		: Statement(StatementType::WHILE)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class ReturnStatement : public Statement
{
public:
	Ref<Expression> expression;

public:
	ReturnStatement()
		: Statement(StatementType::RETURN)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class VariableDeclarationStatement : public Statement
{
public:
	Ref<VariableDeclaration> declaration;
	Ref<Expression> value;

public:
	VariableDeclarationStatement()
		: Statement(StatementType::VARIABLE_DECLARATION)
	{
	}

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

#endif /* SOURCE_AST_AST */
