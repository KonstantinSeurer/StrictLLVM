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
			TEMPLATE,
			VARIABLE_DECLARATION,
			METHOD_DECLARATION,
			DATA_TYPE,
			MEMBER_VARIABLE_DECLARATION)

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

STRICT_ENUM(DeclarationFlags, PRIVATE = 0, INTERNAL = 1, PROTECTED = 2, PUBLIC = 7, MUT = 8, IMPURE = 16)

STRICT_ENUM(DataTypeType, TYPE, VALUE, REFERENCE, POINTER, ARRAY)

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
};

class ValueType : public DataType
{
public:
	String name;
	Ref<Template> typeTemplate;

public:
	ValueType()
		: DataType()
	{
		dataTypeType = DataTypeType::VALUE;
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

class VariableDeclaration : public ASTItem
{
public:
	DeclarationFlags flags;
	String name;
	Ref<DataType> dataType;

public:
	VariableDeclaration(ASTItemType type)
		: ASTItem(type)
	{
	}
};

class Template : public ASTItem
{
public:
	Array<Ref<VariableDeclaration>> arguments;

public:
	Template()
		: ASTItem(ASTItemType::TEMPLATE)
	{
	}
};

class TypeDeclaration : public UnitDeclaration
{
public:
	DeclarationFlags flags;
	Array<Ref<VariableDeclaration>> members;
	Ref<Template> typeTemplate;
	Array<Ref<ValueType>> superTypes;

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

STRICT_ENUM(ModuleType, STATIC, DYNAMIC, INLINE)
STRICT_ENUM(TargetFlags, NONE = 0, BIT32 = 1, BIT64 = 2, X86 = 4, LINUX = 8)

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