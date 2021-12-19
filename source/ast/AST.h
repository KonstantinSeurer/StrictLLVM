#ifndef SOURCE_AST_AST
#define SOURCE_AST_AST

#include "../Base.h"
#include "../Lexer.h"
#include "../JSON.h"

class Template;
class UnitDeclaration;
class Expression;
class Statement;

STRICT_ENUM(ASTItemType,
			NONE,
			MODULE,
			UNIT,
			UNIT_DECLARATION,
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

public:
	String ToString(UInt32 indentation) const;

	virtual Ref<ASTItem> Clone() const = 0;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ASTItem> target) const;
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

class DataType : public ASTItem
{
public:
	DataTypeType dataTypeType;
	DeclarationFlags flags;

public:
	DataType()
		: ASTItem(ASTItemType::DATA_TYPE), flags(DeclarationFlags::PRIVATE)
	{
	}

	virtual ~DataType()
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<DataType> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<PrimitiveType> target) const;
};

class ObjectType : public DataType
{
public:
	String name;
	Ref<Template> typeTemplate;

public:
	Ref<UnitDeclaration> unit;

public:
	ObjectType()
		: DataType()
	{
		dataTypeType = DataTypeType::OBJECT;
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ObjectType> target) const;
};

class PointerType : public DataType
{
public:
	Ref<DataType> value;
	Ref<Expression> arrayLength;

public:
	PointerType()
		: DataType()
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<PointerType> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Template> target) const;
};

STRICT_ENUM(UnitDeclarationType,
			ERROR,
			TYPE,
			CLASS)

class UnitDeclaration : public ASTItem
{
public:
	DeclarationFlags flags;
	UnitDeclarationType declarationType;

public:
	UnitDeclaration(UnitDeclarationType declarationType)
		: ASTItem(ASTItemType::UNIT_DECLARATION), declarationType(declarationType)
	{
	}

public:
	virtual JSON GetStructureJSON() const
	{
		return JSON(JSON::value_t::array);
	}

	bool IsType() const
	{
		return declarationType == UnitDeclarationType::TYPE || declarationType == UnitDeclarationType::CLASS;
	}

	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<UnitDeclaration> target) const;
};

class ErrorDeclaration : public UnitDeclaration
{
public:
	Int32 value;
	bool hasValue;

public:
	ErrorDeclaration()
		: UnitDeclaration(UnitDeclarationType::ERROR)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ErrorDeclaration> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<VariableDeclaration> target) const;
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

public:
	MethodDeclaration(MethodType methodType)
		: VariableDeclaration(), methodType(methodType)
	{
		variableType = VariableDeclarationType::METHOD;
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<MethodDeclaration> target) const;
};

class ConstructorInitializer
{
public:
	String name;

	Ref<Expression> value;
};

class ConstructorDeclaration : public MethodDeclaration
{
public:
	Array<ConstructorInitializer> initializers;

public:
	ConstructorDeclaration()
		: MethodDeclaration(MethodType::CONSTRUCTOR)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ConstructorDeclaration> target) const;
};

STRICT_ENUM(OperatorType,
			NONE,
			// Non mutating binary operators
			PLUS,
			MINUS,
			MULTIPLY,
			DIVIDE,
			AND,
			AND_AND,
			OR,
			OR_OR,
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
			SHIFT_LEFT_EQUAL,
			SHIFT_RIGHT_EQUAL,
			ASSIGN,
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
			ACCESS,
			CALL)

class OperatorDeclaration : public MethodDeclaration
{
public:
	OperatorType operatorType;

public:
	OperatorDeclaration()
		: MethodDeclaration(MethodType::OPERATOR)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<OperatorDeclaration> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<MemberVariableDeclaration> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<TemplateDeclaration> target) const;
};

class TypeDeclaration : public UnitDeclaration
{
public:
	Array<Ref<VariableDeclaration>> members;
	Ref<TemplateDeclaration> typeTemplate;
	Array<Ref<ObjectType>> superTypes;

public:
	TypeDeclaration()
		: UnitDeclaration(UnitDeclarationType::TYPE)
	{
	}

	TypeDeclaration(UnitDeclarationType declarationType)
		: UnitDeclaration(declarationType)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<TypeDeclaration> target) const;
};

class ClassDeclaration : public TypeDeclaration
{
public:
	bool isSingleton;

public:
	ClassDeclaration()
		: TypeDeclaration(UnitDeclarationType::CLASS), isSingleton(false)
	{
	}

	ClassDeclaration(bool isSingleton)
		: TypeDeclaration(UnitDeclarationType::CLASS), isSingleton(isSingleton)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ClassDeclaration> target) const;
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

	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Unit> target) const;
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

class Module : public ASTItem
{
public:
	String name;
	Array<Ref<Unit>> units;
	Array<Ref<Module>> dependencies;

public:
	Module(const String &name)
		: ASTItem(ASTItemType::MODULE), name(name)
	{
	}

	Ref<Unit> GetUnit(const String &name) const
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

	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Module> target) const;
};

STRICT_ENUM(ExpressionType,
			LITERAL,
			VARIABLE,
			OPERATOR,
			CALL,
			TERNARY,
			BRACKET,
			NEW)

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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Expression> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<LiteralExpression> target) const;
};

class BracketExpression : public Expression
{
public:
	Ref<Expression> expression;

public:
	BracketExpression()
		: Expression(ExpressionType::BRACKET)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<BracketExpression> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<VariableExpression> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<OperatorExpression> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<CallExpression> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<TernaryExpression> target) const;
};

class NewExpression : public Expression
{
public:
	Ref<DataType> dataType;
	Array<Ref<Expression>> arguments;

public:
	NewExpression()
		: Expression(ExpressionType::NEW)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<NewExpression> target) const;
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
			VARIABLE_DECLARATION,
			DELETE)

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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Statement> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<BlockStatement> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ExpressionStatement> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<IfStatement> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ForStatement> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<WhileStatement> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ReturnStatement> target) const;
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

public:
	virtual Ref<ASTItem> Clone() const;

	void CloneImplementation(Ref<VariableDeclarationStatement> target) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;
};

class DeleteStatement : public Statement
{
public:
	Ref<Expression> expression;

public:
	DeleteStatement()
		: Statement(StatementType::DELETE)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<DeleteStatement> target) const;
};

#endif /* SOURCE_AST_AST */
