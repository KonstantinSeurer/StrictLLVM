#ifndef SOURCE_AST_AST
#define SOURCE_AST_AST

#include "../Base.h"
#include "../JSON.h"
#include "../Lexer.h"

#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

class Template;
class Unit;
class UnitDeclaration;
class Expression;
class Statement;
class Module;

STRICT_ENUM(ASTItemType, NONE, MODULE, UNIT, UNIT_DECLARATION, TEMPLATE_DECLARATION, TEMPLATE, VARIABLE_DECLARATION, DATA_TYPE, EXPRESSION, STATEMENT)

class ASTItemMeta
{
public:
};

class ASTItem
{
public:
	ASTItemMeta itemMeta;
	ASTItemType type;
	UInt32 characterIndex = CHARACTER_INDEX_NONE;

public:
	ASTItem(ASTItemType type) : type(type)
	{
	}

	virtual ~ASTItem()
	{
	}

public:
	String ToString(UInt32 indentation) const;

	virtual Ref<ASTItem> Clone() const = 0;

	bool operator==(const ASTItem& other) const;

	virtual String ToStrict() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ASTItem> target) const;
};

DECLARE_HASH(ASTItem)

STRICT_FLAGS(DeclarationFlags, PRIVATE = 0, INTERNAL = 1, PROTECTED = 2, PUBLIC = 7, MUT = 8, IMPURE = 16, VIRTUAL = 32, EXTERNAL = 64)

STRICT_ENUM(DataTypeType, TYPE, PRIMITIVE, OBJECT, REFERENCE, POINTER, ARRAY)

class DataTypeMeta
{
public:
	llvm::Type* ir = nullptr;
	llvm::DIType* di = nullptr;
};

class DataType : public ASTItem
{
public:
	DataTypeMeta dataTypeMeta;
	DataTypeType dataTypeType;
	DeclarationFlags flags;

public:
	DataType() : ASTItem(ASTItemType::DATA_TYPE), flags(DeclarationFlags::PRIVATE)
	{
	}

	virtual ~DataType()
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const DataType& other) const;

	bool IsPointer() const
	{
		return dataTypeType == DataTypeType::POINTER || dataTypeType == DataTypeType::REFERENCE || dataTypeType == DataTypeType::ARRAY;
	}

	virtual String ToStrict() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<DataType> target) const;
};

DECLARE_HASH(DataType)

class PrimitiveTypeMeta
{
public:
};

class PrimitiveType : public DataType
{
public:
	PrimitiveTypeMeta primitiveTypeMeta;
	TokenType primitiveType;

public:
	PrimitiveType() : DataType()
	{
		dataTypeType = DataTypeType::PRIMITIVE;
	}

	PrimitiveType(TokenType primitiveType) : DataType(), primitiveType(primitiveType)
	{
		dataTypeType = DataTypeType::PRIMITIVE;
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const PrimitiveType& other) const;

	virtual String ToStrict() const;

	bool IsSigned() const;
	bool IsFloat() const;

	UInt8 GetSize() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<PrimitiveType> target) const;
};

DECLARE_HASH(PrimitiveType)

class ObjectTypeMeta
{
public:
	Ref<Unit> unit = nullptr;
};

class ObjectType : public DataType
{
public:
	ObjectTypeMeta objectTypeMeta;
	String name;
	Ref<Template> typeTemplate;

public:
	ObjectType() : DataType()
	{
		dataTypeType = DataTypeType::OBJECT;
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const ObjectType& other) const;

	virtual String ToStrict() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ObjectType> target) const;
};

DECLARE_HASH(ObjectType)

bool CanCast(const DataType* source, const DataType* destination);

class PointerTypeMeta
{
public:
};

class PointerType : public DataType
{
public:
	PointerTypeMeta pointerTypeMeta;
	Ref<DataType> value;
	Ref<Expression> arrayLength;

public:
	PointerType() : DataType()
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const PointerType& other) const;

	virtual String ToStrict() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<PointerType> target) const;
};

Ref<DataType> GetReferencedType(Ref<DataType> dataType);
DataType* GetReferencedType(DataType* dataType);

DECLARE_HASH(PointerType)

class TemplateArgumentMeta
{
public:
};

class TemplateArgument
{
public:
	TemplateArgumentMeta templateArgumentMeta;
	Ref<Expression> expression;
	Ref<DataType> dataType;

public:
	bool operator==(const TemplateArgument& other) const;
};

DECLARE_HASH(TemplateArgument)

class TemplateMeta
{
public:
};

class Template : public ASTItem
{
public:
	TemplateMeta templateMeta;
	Array<TemplateArgument> arguments;

public:
	Template() : ASTItem(ASTItemType::TEMPLATE)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const Template& other) const;

	virtual String ToStrict() const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Template> target) const;
};

DECLARE_HASH(Template)

STRICT_ENUM(UnitDeclarationType, ERROR, TYPE, CLASS)

class UnitDeclarationMeta
{
public:
	Ref<ObjectType> thisType = nullptr;
	Unit* parent = nullptr;
};

class UnitDeclaration : public ASTItem
{
public:
	String name;
	UnitDeclarationMeta unitDeclarationMeta;
	DeclarationFlags flags;
	UnitDeclarationType declarationType;

public:
	UnitDeclaration(UnitDeclarationType declarationType) : ASTItem(ASTItemType::UNIT_DECLARATION), declarationType(declarationType)
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

	bool operator==(const UnitDeclaration& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<UnitDeclaration> target) const;
};

DECLARE_HASH(UnitDeclaration)

class ErrorDeclarationMeta
{
public:
};

class ErrorDeclaration : public UnitDeclaration
{
public:
	ErrorDeclarationMeta errorDeclarationMeta;
	Int32 value;
	bool hasValue;

public:
	ErrorDeclaration() : UnitDeclaration(UnitDeclarationType::ERROR), value(0)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const ErrorDeclaration& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ErrorDeclaration> target) const;
};

DECLARE_HASH(ErrorDeclaration)

STRICT_ENUM(VariableDeclarationType, VARIABLE, MEMBER_VARIABLE, METHOD)

class TypeDeclaration;

class VariableDeclarationMeta
{
public:
	llvm::Value* ir = nullptr;
	TypeDeclaration* parentType = nullptr;
};

class VariableDeclaration : public ASTItem
{
public:
	VariableDeclarationMeta variableDeclarationMeta;
	DeclarationFlags flags;
	VariableDeclarationType variableType;
	String name;
	Ref<DataType> dataType;

public:
	VariableDeclaration() : ASTItem(ASTItemType::VARIABLE_DECLARATION), variableType(VariableDeclarationType::VARIABLE), flags(DeclarationFlags::PRIVATE)
	{
	}

	virtual ~VariableDeclaration()
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const VariableDeclaration& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<VariableDeclaration> target) const;
};

DECLARE_HASH(VariableDeclaration)

STRICT_ENUM(MethodType, METHOD, CONSTRUCTOR, DESTRUCTOR, OPERATOR, GETTER, SETTER)

class MethodDeclarationMeta
{
public:
	String name;
	TypeDeclaration* parent;
};

class MethodDeclaration : public VariableDeclaration
{
public:
	MethodDeclarationMeta methodDeclarationMeta;
	MethodType methodType;
	Array<Ref<VariableDeclaration>> parameters;

	Ref<Statement> body;

public:
	MethodDeclaration(MethodType methodType) : VariableDeclaration(), methodType(methodType)
	{
		variableType = VariableDeclarationType::METHOD;
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const MethodDeclaration& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<MethodDeclaration> target) const;
};

DECLARE_HASH(MethodDeclaration)

class ConstructorInitializerMeta
{
public:
};

class ConstructorInitializer
{
public:
	ConstructorInitializerMeta constructorInitializerMeta;
	String name;
	Ref<Expression> value;

public:
	bool operator==(const ConstructorInitializer& other) const;
};

DECLARE_HASH(ConstructorInitializer)

class ConstructorDeclarationMeta
{
public:
};

class ConstructorDeclaration : public MethodDeclaration
{
public:
	ConstructorDeclarationMeta constructorDeclarationMeta;
	Array<ConstructorInitializer> initializers;

public:
	ConstructorDeclaration() : MethodDeclaration(MethodType::CONSTRUCTOR)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const ConstructorDeclaration& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ConstructorDeclaration> target) const;
};

DECLARE_HASH(ConstructorDeclaration)

STRICT_ENUM(OperatorType, NONE,
            // Non mutating binary operators
            PLUS, MINUS, MULTIPLY, DIVIDE, AND, AND_AND, OR, OR_OR, XOR, SHIFT_LEFT, SHIFT_RIGHT, GREATER, LESS, EQUAL, NOT_EQUAL, GREATER_EQUAL, LESS_EQUAL,
            // Mutating binary operators
            PLUS_EQUAL, MINUS_EQUAL, MULTIPLY_EQUAL, DIVIDE_EQUAL, AND_EQUAL, OR_EQUAL, XOR_EQUAL, SHIFT_LEFT_EQUAL, SHIFT_RIGHT_EQUAL, ASSIGN,
            // Misc binary operators
            ARRAY_ACCESS,
            // Non mutating unary operators
            NEGATIVE, NOT, INVERSE, IMPLICIT_CAST, EXPLICIT_CAST, DEREFERENCE,
            // Mutating unary operators
            INCREMENT, DECREMENT,
            // Internal operators
            ACCESS, CALL, POST_STAR, POST_AND, TEMPLATE)

class OperatorDeclarationMeta
{
public:
};

class OperatorDeclaration : public MethodDeclaration
{
public:
	OperatorDeclarationMeta operatorDeclarationMeta;
	OperatorType operatorType;

public:
	OperatorDeclaration() : MethodDeclaration(MethodType::OPERATOR)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const OperatorDeclaration& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<OperatorDeclaration> target) const;
};

DECLARE_HASH(OperatorDeclaration)

class MemberVariableDeclarationMeta
{
public:
	UInt32 index = 0;
};

class MemberVariableDeclaration : public VariableDeclaration
{
public:
	MemberVariableDeclarationMeta memberVariableDeclarationMeta;
	Array<Ref<MethodDeclaration>> accessors;
	Ref<Expression> value;

public:
	MemberVariableDeclaration() : VariableDeclaration()
	{
		variableType = VariableDeclarationType::MEMBER_VARIABLE;
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const MemberVariableDeclaration& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<MemberVariableDeclaration> target) const;
};

DECLARE_HASH(MemberVariableDeclaration)

class TemplateDeclarationMeta
{
public:
};

class TemplateDeclaration : public ASTItem
{
public:
	TemplateDeclarationMeta templateDeclarationMeta;
	Array<Ref<VariableDeclaration>> parameters;

public:
	TemplateDeclaration() : ASTItem(ASTItemType::TEMPLATE_DECLARATION)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const TemplateDeclaration& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<TemplateDeclaration> target) const;
};

DECLARE_HASH(TemplateDeclaration)

class TypeDeclarationMeta
{
public:
	HashMap<ObjectType, Array<ObjectType*>> usedTemplateTypes;
};

class TypeDeclaration : public UnitDeclaration
{
public:
	TypeDeclarationMeta typeDeclarationMeta;
	Array<Ref<VariableDeclaration>> members;
	Ref<TemplateDeclaration> typeTemplate;
	Array<Ref<ObjectType>> superTypes;

public:
	TypeDeclaration() : UnitDeclaration(UnitDeclarationType::TYPE)
	{
	}

	TypeDeclaration(UnitDeclarationType declarationType) : UnitDeclaration(declarationType)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const TypeDeclaration& other) const;

	Ref<ConstructorDeclaration> GetDefaultConstructor() const;
	Ref<ConstructorDeclaration> GetCopyConstructor() const;

	Ref<MethodDeclaration> GetDestructor() const;

	Ref<MethodDeclaration> FindMethod(MethodType methodType, Array<DataType*>* parameters = nullptr, const String& name = "") const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<TypeDeclaration> target) const;
};

DECLARE_HASH(TypeDeclaration)

class ClassDeclaration;

class ClassDeclarationMeta
{
public:
	Ref<VariableDeclaration> thisDeclaration = nullptr;

	Unique<llvm::Module> module = nullptr;
	llvm::Function* malloc = nullptr;
	llvm::Function* free = nullptr;
	HashMap<MethodDeclaration*, llvm::Function*> methods;
	HashMap<ClassDeclaration*, llvm::Value*> singletons;
	llvm::Value* typeId = nullptr;

	// The templated class if this is a specialization.
	ClassDeclaration* sourceClass = nullptr;
};

class ClassDeclaration : public TypeDeclaration
{
public:
	ClassDeclarationMeta classDeclarationMeta;
	bool isSingleton;

public:
	ClassDeclaration() : TypeDeclaration(UnitDeclarationType::CLASS), isSingleton(false)
	{
	}

	ClassDeclaration(bool isSingleton) : TypeDeclaration(UnitDeclarationType::CLASS), isSingleton(isSingleton)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const ClassDeclaration& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ClassDeclaration> target) const;
};

DECLARE_HASH(ClassDeclaration)

class UnitMeta
{
public:
	Array<Ref<Unit>> dependencies;
	Lexer lexer;
	Module* parent;
	bool hasExternals = false;
};

class Unit : public ASTItem
{
public:
	UnitMeta unitMeta;
	Array<String> dependencyNames;
	String name;
	Ref<UnitDeclaration> declaredType;

public:
	Unit() : ASTItem(ASTItemType::UNIT)
	{
	}

	Unit(const JSON& structureJSON);

public:
	JSON GetStructureJSON() const;

	virtual Ref<ASTItem> Clone() const;

	bool operator==(const Unit& other) const;

	Ref<Unit> GetDependency(const String& name) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Unit> target) const;
};

DECLARE_HASH(Unit)

STRICT_ENUM(ModuleType, STATIC, DYNAMIC, INLINE, EXECUTABLE)

STRICT_FLAGS(TargetFlags, NONE = 0, BIT32 = 1, BIT64 = 2, X86 = 4, LINUX = 8)

bool IsTargetActive(TargetFlags target, TargetFlags buildTarget);
TargetFlags JSONToTargetFlags(const JSON& json);

class ModuleMeta
{
public:
	Ref<llvm::Module> module = nullptr;
	llvm::Function* entryPoint = nullptr;
	llvm::Function* main = nullptr;
	llvm::Value* virtualTable = nullptr;
	llvm::Value* typeId = nullptr;

	HashMap<ObjectType, Ref<ClassDeclaration>> templateSpecializations;

	String path;
	String outputPath;
};

class Module : public ASTItem
{
public:
	ModuleMeta moduleMeta;
	ModuleType moduleType;
	String name;
	Array<Ref<Unit>> units;
	Array<Ref<Module>> dependencies;

public:
	Module(ModuleType moduleType, const String& name) : ASTItem(ASTItemType::MODULE), moduleType(moduleType), name(name)
	{
	}

	Ref<Unit> GetUnit(const String& name) const
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

	bool operator==(const Module& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Module> target) const;
};

DECLARE_HASH(Module)

STRICT_ENUM(ExpressionType, LITERAL, IDENTIFIER, OPERATOR, CALL, TERNARY, BRACKET, NEW)

class ExpressionMeta
{
public:
	Ref<DataType> dataType = nullptr;
	Statement* parentStatement = nullptr;
	Expression* parentExpression = nullptr;
	llvm::Value* ir = nullptr;
	bool pointer = false;

public:
	llvm::Value* Load(llvm::IRBuilder<>& builder) const;
};

class Expression : public ASTItem
{
public:
	ExpressionMeta expressionMeta;
	ExpressionType expressionType;

public:
	Expression(ExpressionType expressionType) : ASTItem(ASTItemType::EXPRESSION), expressionType(expressionType)
	{
	}

	virtual ~Expression()
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const Expression& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Expression> target) const;
};

DECLARE_HASH(Expression)

class LiteralExpressionMeta
{
public:
};

class LiteralExpression : public Expression
{
public:
	LiteralExpressionMeta literalExpressionMeta;
	Token data;

public:
	LiteralExpression() : Expression(ExpressionType::LITERAL)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const LiteralExpression& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<LiteralExpression> target) const;
};

DECLARE_HASH(LiteralExpression)

class BracketExpressionMeta
{
public:
};

class BracketExpression : public Expression
{
public:
	BracketExpressionMeta bracketExpressionMeta;
	Ref<Expression> expression;

public:
	BracketExpression() : Expression(ExpressionType::BRACKET)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const BracketExpression& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<BracketExpression> target) const;
};

DECLARE_HASH(BracketExpression)

class IdentifierExpressionMeta
{
public:
	Ref<ASTItem> destination = nullptr;
};

class IdentifierExpression : public Expression
{
public:
	IdentifierExpressionMeta identifierExpressionMeta;
	String name;

public:
	IdentifierExpression() : Expression(ExpressionType::IDENTIFIER)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const IdentifierExpression& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<IdentifierExpression> target) const;
};

DECLARE_HASH(IdentifierExpression)

class SecondOperandMeta
{
public:
};

struct SecondOperand
{
public:
	SecondOperandMeta secondOperandMeta;
	Ref<Expression> expression = nullptr;
	Ref<DataType> dataType = nullptr;
	Ref<Template> typeTemplate = nullptr;

public:
	bool operator==(const SecondOperand& other) const;
};

DECLARE_HASH(SecondOperand)

class OperatorExpressionMeta
{
public:
};

class OperatorExpression : public Expression
{
public:
	OperatorExpressionMeta operatorExpressionMeta;
	Ref<Expression> a;
	Optional<SecondOperand> b;

	OperatorType operatorType;

public:
	OperatorExpression() : Expression(ExpressionType::OPERATOR)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const OperatorExpression& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<OperatorExpression> target) const;
};

DECLARE_HASH(OperatorExpression)

class CallExpressionMeta
{
public:
	Ref<VariableDeclaration> destination;
	Ref<Expression> context;
};

class CallExpression : public Expression
{
public:
	CallExpressionMeta callExpressionMeta;
	Ref<Expression> method;
	Array<Ref<Expression>> arguments;

public:
	CallExpression() : Expression(ExpressionType::CALL)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const CallExpression& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<CallExpression> target) const;
};

DECLARE_HASH(CallExpression)

class TernaryExpressionMeta
{
public:
};

class TernaryExpression : public Expression
{
public:
	TernaryExpressionMeta ternaryExpressionMeta;
	Ref<Expression> condition;
	Ref<Expression> thenExpression;
	Ref<Expression> elseExpression;

public:
	TernaryExpression() : Expression(ExpressionType::TERNARY)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const TernaryExpression& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<TernaryExpression> target) const;
};

DECLARE_HASH(TernaryExpression)

STRICT_ENUM(AllocationType, STACK, HEAP)
class NewExpressionMeta
{
public:
};

class NewExpression : public Expression
{
public:
	NewExpressionMeta newExpressionMeta;
	Ref<DataType> dataType;
	Array<Ref<Expression>> arguments;
	AllocationType allocationType;

public:
	NewExpression() : Expression(ExpressionType::NEW)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const NewExpression& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<NewExpression> target) const;
};

DECLARE_HASH(NewExpression)

STRICT_ENUM(StatementType, NOP, BLOCK, EXPRESSION, IF, FOR, WHILE, RETURN, BREAK, CONTINUE, VARIABLE_DECLARATION, DELETE)

class StatementMeta
{
public:
	Statement* parent = nullptr;
};

class Statement : public ASTItem
{
public:
	StatementMeta statementMeta;
	StatementType statementType;

public:
	Statement(StatementType statementType) : ASTItem(ASTItemType::STATEMENT), statementType(statementType)
	{
	}

	virtual ~Statement()
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const Statement& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<Statement> target) const;
};

DECLARE_HASH(Statement)

class BlockStatementMeta
{
public:
};

class BlockStatement : public Statement
{
public:
	BlockStatementMeta blockStatementMeta;
	Array<Ref<Statement>> statements;

public:
	BlockStatement() : Statement(StatementType::BLOCK)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const BlockStatement& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<BlockStatement> target) const;
};

DECLARE_HASH(BlockStatement)

class ExpressionStatementMeta
{
public:
};

class ExpressionStatement : public Statement
{
public:
	ExpressionStatementMeta expressionStatementMeta;
	Ref<Expression> expression;

public:
	ExpressionStatement() : Statement(StatementType::EXPRESSION)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const ExpressionStatement& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ExpressionStatement> target) const;
};

DECLARE_HASH(ExpressionStatement)

class IfStatementMeta
{
public:
};

class IfStatement : public Statement
{
public:
	IfStatementMeta ifStatementMeta;
	Ref<Expression> condition;
	Ref<Statement> thenStatement;
	Ref<Statement> elseStatement;

public:
	IfStatement() : Statement(StatementType::IF)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const IfStatement& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<IfStatement> target) const;
};

DECLARE_HASH(IfStatement)

class ForStatementMeta
{
public:
};

class ForStatement : public Statement
{
public:
	ForStatementMeta forStatementMeta;
	Ref<Statement> startStatement;
	Ref<Expression> condition;
	Ref<Expression> incrementExpression;
	Ref<Statement> bodyStatement;

public:
	ForStatement() : Statement(StatementType::FOR)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const ForStatement& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ForStatement> target) const;
};

DECLARE_HASH(ForStatement)

class WhileStatementMeta
{
public:
};

class WhileStatement : public Statement
{
public:
	WhileStatementMeta whileStatementMeta;
	Ref<Expression> condition;
	Ref<Statement> bodyStatement;
	bool checkAfterBody;

public:
	WhileStatement() : Statement(StatementType::WHILE)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const WhileStatement& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<WhileStatement> target) const;
};

DECLARE_HASH(WhileStatement)

class ReturnStatementMeta
{
public:
};

class ReturnStatement : public Statement
{
public:
	ReturnStatementMeta returnStatementMeta;
	Ref<Expression> expression;

public:
	ReturnStatement() : Statement(StatementType::RETURN)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const ReturnStatement& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<ReturnStatement> target) const;
};

DECLARE_HASH(ReturnStatement)

class VariableDeclarationStatementMeta
{
public:
};

class VariableDeclarationStatement : public Statement
{
public:
	VariableDeclarationStatementMeta variableDeclarationStatementMeta;
	Ref<VariableDeclaration> declaration;
	Ref<Expression> value;

public:
	VariableDeclarationStatement() : Statement(StatementType::VARIABLE_DECLARATION)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const VariableDeclarationStatement& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<VariableDeclarationStatement> target) const;
};

DECLARE_HASH(VariableDeclarationStatement)

class DeleteStatementMeta
{
public:
};

class DeleteStatement : public Statement
{
public:
	DeleteStatementMeta deleteStatementMeta;
	Ref<Expression> expression;

public:
	DeleteStatement() : Statement(StatementType::DELETE)
	{
	}

public:
	virtual Ref<ASTItem> Clone() const;

	bool operator==(const DeleteStatement& other) const;

protected:
	virtual String ToStringImplementation(UInt32 indentation) const;

	void CloneImplementation(Ref<DeleteStatement> target) const;
};

DECLARE_HASH(DeleteStatement)

#endif /* SOURCE_AST_AST */
