#ifndef SOURCE_AST_ASTITEM
#define SOURCE_AST_ASTITEM

#include "../Base.h"
#include "../Lexer.h"
#include "../JSON.h"

STRICT_ENUM(ASTItemType,
			MODULE,
			UNIT,
			CLASS_DECLARATION,
			ERROR_DECLARATION,
			TYPE_DECLARATION,
			TEMPLATE,
			MEMBER_VARIABLE_DECLARATION,
			METHOD_DECLARATION,
			DATA_TYPE)

class ASTItem
{
protected:
	ASTItemType type;

public:
	ASTItem(ASTItemType type)
		: type(type)
	{
	}

public:
	ASTItemType GetType() const
	{
		return type;
	}
};

class DataType : public ASTItem
{
public:
public:
	DataType()
		: ASTItem(ASTItemType::DATA_TYPE)
	{
	}
};

STRICT_ENUM(DeclarationFlags, PRIVATE = 0, INTERNAL = 1, PROTECTED = 2, PUBLIC = 7, MUT = 8, IMPURE = 16)

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

class MemberDeclaration : public ASTItem
{
public:
	DeclarationFlags flags;
	String name;
	Ref<DataType> dataType;

public:
	MemberDeclaration(ASTItemType type)
		: ASTItem(type)
	{
	}
};

class Template : public ASTItem
{
public:
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
	Array<Ref<MemberDeclaration>> members;
	Ref<Template> typeTemplate;

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

#endif /* SOURCE_AST_ASTITEM */
