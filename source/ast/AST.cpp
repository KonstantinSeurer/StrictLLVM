
#include "AST.h"

// TODO: implement CloneImplementation using macros

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

String ASTItem::ToStrict() const
{
	abort();
	return "";
}

String ASTItem::ToStringImplementation(UInt32 indentation) const
{
	return "\n";
}

void ASTItem::CloneImplementation(Ref<ASTItem> target) const
{
	target->type = type;
	target->itemMeta = itemMeta;
	target->characterIndex = characterIndex;
}

DEFINE_HASH(ASTItem, HASH_VALUE(ASTItemType, type))

bool ASTItem::operator==(const ASTItem& other) const
{
	return type == other.type;
}

static String ToString(const Token& token)
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

#define ENUM_VAR(indentation, name)                                                                \
	Indentation(indentation) + #name + " = " + ::ToString(name) + "\n"
#define STRING_VAR(indentation, name) Indentation(indentation) + #name + " = " + name + "\n"
#define AST_VAR(indentation, name)                                                                 \
	Indentation(indentation) + #name + " = " + (name ? name->ToString(indentation) : "null\n")
#define PRIMITIVE_VAR(indentation, name)                                                           \
	Indentation(indentation) + #name + " = " + std::to_string(name)
#define TOKEN_VAR(indentation, name)                                                               \
	Indentation(indentation) + #name + " = " + ::ToString(name) + "\n"

#define CLONE_METHOD(Type, ...)                                                                    \
	Ref<ASTItem> Type::Clone() const                                                               \
	{                                                                                              \
		Ref<Type> result = Allocate<Type>(__VA_ARGS__);                                            \
		Type::CloneImplementation(result);                                                         \
		return result;                                                                             \
	}

static String ToStrict(DeclarationFlags flags)
{
	String result;
	if ((flags & DeclarationFlags::PUBLIC) == DeclarationFlags::PUBLIC)
	{
		result += "public ";
	}
	else
	{
		if ((flags & DeclarationFlags::INTERNAL) == DeclarationFlags::INTERNAL)
		{
			result += "internal ";
		}
		if ((flags & DeclarationFlags::PROTECTED) == DeclarationFlags::PROTECTED)
		{
			result += "protected ";
		}
	}
	if ((flags & DeclarationFlags::MUT) == DeclarationFlags::MUT)
	{
		result += "mut ";
	}
	if ((flags & DeclarationFlags::IMPURE) == DeclarationFlags::IMPURE)
	{
		result += "impure ";
	}
	if ((flags & DeclarationFlags::VIRTUAL) == DeclarationFlags::VIRTUAL)
	{
		result += "virtual ";
	}
	return result;
}

String DataType::ToStrict() const
{
	return ::ToStrict(flags);
}

String DataType::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, dataTypeType) + ENUM_VAR(indentation, flags);
}

CLONE_METHOD(DataType)

void DataType::CloneImplementation(Ref<DataType> target) const
{
	ASTItem::CloneImplementation(target);
	target->dataTypeMeta = dataTypeMeta;
	target->dataTypeType = dataTypeType;
	target->flags = flags;
}

DEFINE_HASH_WITH_SUPER(DataType, ASTItem,
                       HASH_VALUE(DataTypeType, dataTypeType) HASH_VALUE(DeclarationFlags, flags))

bool DataType::operator==(const DataType& other) const
{
	return dataTypeType == other.dataTypeType && flags == other.flags;
}

String PrimitiveType::ToStrict() const
{
	return DataType::ToStrict() + ::ToStrict(primitiveType);
}

String PrimitiveType::ToStringImplementation(UInt32 indentation) const
{
	return DataType::ToStringImplementation(indentation) + ENUM_VAR(indentation, primitiveType);
}

CLONE_METHOD(PrimitiveType)

void PrimitiveType::CloneImplementation(Ref<PrimitiveType> target) const
{
	DataType::CloneImplementation(target);
	target->primitiveTypeMeta = primitiveTypeMeta;
	target->primitiveType = primitiveType;
}

DEFINE_HASH_WITH_SUPER(PrimitiveType, DataType, HASH_VALUE(TokenType, primitiveType))

bool PrimitiveType::operator==(const PrimitiveType& other) const
{
	if (!DataType::operator==(other))
	{
		return false;
	}

	return primitiveType == other.primitiveType;
}

bool PrimitiveType::IsSigned() const
{
	switch (primitiveType)
	{
	case TokenType::INT8:
	case TokenType::INT16:
	case TokenType::INT32:
	case TokenType::INT64:
		return true;
	}

	return false;
}

UInt8 PrimitiveType::GetSize() const
{
	switch (primitiveType)
	{
	case TokenType::BOOL:
	case TokenType::INT8:
	case TokenType::UINT8:
		return 1;
	case TokenType::INT16:
	case TokenType::UINT16:
		return 2;
	case TokenType::INT32:
	case TokenType::UINT32:
	case TokenType::FLOAT32:
		return 4;
	case TokenType::INT64:
	case TokenType::UINT64:
	case TokenType::FLOAT64:
		return 8;
	default:
		STRICT_UNREACHABLE;
	}

	return 0;
}

bool PrimitiveType::IsFloat() const
{
	return primitiveType == TokenType::FLOAT32 || primitiveType == TokenType::FLOAT64;
}

String ObjectType::ToStrict() const
{
	return DataType::ToStrict() + name + (typeTemplate ? typeTemplate->ToStrict() : " ");
}

String ObjectType::ToStringImplementation(UInt32 indentation) const
{
	return DataType::ToStringImplementation(indentation) + STRING_VAR(indentation, name) +
	       AST_VAR(indentation, typeTemplate);
}

CLONE_METHOD(ObjectType)

void ObjectType::CloneImplementation(Ref<ObjectType> target) const
{
	DataType::CloneImplementation(target);
	target->objectTypeMeta = objectTypeMeta;
	target->name = name;
	if (typeTemplate)
	{
		target->typeTemplate = std::dynamic_pointer_cast<Template>(typeTemplate->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(ObjectType, DataType,
                       HASH_VALUE(String, name) HASH_REF(Template, typeTemplate))

bool ObjectType::operator==(const ObjectType& other) const
{
	if (!DataType::operator==(other))
	{
		return false;
	}

	EQUALS_REF(other, typeTemplate)

	return name == other.name;
}

String PointerType::ToStrict() const
{
	return DataType::ToStrict() + "(" + value->ToStrict() +
	       (arrayLength ? "[" + arrayLength->ToStrict() + "]" : "") + ")";
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
	target->pointerTypeMeta = pointerTypeMeta;
	target->value = std::dynamic_pointer_cast<DataType>(value->Clone());
	if (arrayLength)
	{
		target->arrayLength = std::dynamic_pointer_cast<Expression>(arrayLength->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(PointerType, DataType,
                       HASH_REF(DataType, value) HASH_REF(Expression, arrayLength))

bool PointerType::operator==(const PointerType& other) const
{
	if (!DataType::operator==(other))
	{
		return false;
	}

	EQUALS_REF(other, value)
	EQUALS_REF(other, arrayLength)

	return true;
}

Ref<DataType> GetReferencedType(Ref<DataType> dataType)
{
	if (dataType->dataTypeType != DataTypeType::REFERENCE)
	{
		return dataType;
	}
	Ref<PointerType> referenceType = std::dynamic_pointer_cast<PointerType>(dataType);
	return GetReferencedType(referenceType->value);
}

DataType* GetReferencedType(DataType* dataType)
{
	if (dataType->dataTypeType != DataTypeType::REFERENCE)
	{
		return dataType;
	}
	return GetReferencedType(((PointerType*)dataType)->value.get());
}

bool CanCast(const DataType* source, const DataType* destination)
{
	if (source == destination)
	{
		return true;
	}

	if (source->dataTypeType != destination->dataTypeType)
	{
		return false;
	}

	if (source->dataTypeType == DataTypeType::PRIMITIVE)
	{
		const PrimitiveType* sourcePrimitive = (const PrimitiveType*)source;
		const PrimitiveType* destinationPrimitive = (const PrimitiveType*)destination;

		if (sourcePrimitive->primitiveType == destinationPrimitive->primitiveType)
		{
			return true;
		}

		const bool sourceFloat = sourcePrimitive->IsFloat();
		const bool destinationFloat = destinationPrimitive->IsFloat();

		if (sourceFloat && !destinationFloat)
		{
			return false;
		}

		if (destinationFloat && !sourceFloat)
		{
			return true;
		}

		return sourcePrimitive->GetSize() < destinationPrimitive->GetSize();
	}

	if (source->dataTypeType == DataTypeType::OBJECT)
	{
		const ObjectType* sourceObject = (const ObjectType*)source;
		const ObjectType* destinationObject = (const ObjectType*)destination;

		return *sourceObject == *destinationObject;
	}

	if (source->dataTypeType == DataTypeType::REFERENCE ||
	    source->dataTypeType == DataTypeType::POINTER ||
	    source->dataTypeType == DataTypeType::ARRAY)
	{
		const PointerType* sourcePointer = (const PointerType*)source;
		const PointerType* destinationPointer = (const PointerType*)destination;

		if (sourcePointer->value->dataTypeType == DataTypeType::PRIMITIVE)
		{
			const PrimitiveType* sourcePrimitive = (const PrimitiveType*)sourcePointer->value.get();
			const PrimitiveType* destinationPrimitive =
				(const PrimitiveType*)destinationPointer->value.get();

			return sourcePrimitive->primitiveType == destinationPrimitive->primitiveType;
		}

		return CanCast(sourcePointer->value.get(), destinationPointer->value.get());
	}

	STRICT_UNREACHABLE;
}

bool TypeEquals(const DataType* a, const DataType* b)
{
	if (a == b)
	{
		return true;
	}

	if (!a || !b)
	{
		return false;
	}

	if (!(*a == *b))
	{
		return false;
	}

	switch (a->dataTypeType)
	{
	case DataTypeType::ARRAY:
	case DataTypeType::POINTER:
	case DataTypeType::REFERENCE:
		return *(const PointerType*)a == *(const PointerType*)b;
	case DataTypeType::OBJECT:
		return *(const ObjectType*)a == *(const ObjectType*)b;
	case DataTypeType::PRIMITIVE:
		return *(const PrimitiveType*)a == *(const PrimitiveType*)b;
	case DataTypeType::TYPE:
		return true;
	default:
		STRICT_UNREACHABLE;
		return false;
	}
}

llvm::Value* ExpressionMeta::Load(llvm::IRBuilder<>& builder) const
{
	return pointer ? builder.CreateLoad(dataType->dataTypeMeta.ir, ir) : ir;
}

String Expression::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, expressionType);
}

CLONE_METHOD(Expression, expressionType)

void Expression::CloneImplementation(Ref<Expression> target) const
{
	ASTItem::CloneImplementation(target);
	target->expressionMeta = expressionMeta;
}

DEFINE_HASH_WITH_SUPER(Expression, ASTItem, HASH_VALUE(ExpressionType, expressionType))

bool Expression::operator==(const Expression& other) const
{
	return expressionType == other.expressionType;
}

String LiteralExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + TOKEN_VAR(indentation, data);
}

CLONE_METHOD(LiteralExpression)

void LiteralExpression::CloneImplementation(Ref<LiteralExpression> target) const
{
	Expression::CloneImplementation(target);
	target->literalExpressionMeta = literalExpressionMeta;
	target->data = data;
}

DEFINE_HASH_WITH_SUPER(LiteralExpression, Expression, HASH_VALUE(TokenType, data.type))

bool LiteralExpression::operator==(const LiteralExpression& other) const
{
	return data == other.data;
}

String BracketExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

CLONE_METHOD(BracketExpression)

void BracketExpression::CloneImplementation(Ref<BracketExpression> target) const
{
	Expression::CloneImplementation(target);
	target->bracketExpressionMeta = bracketExpressionMeta;
	target->expression = std::dynamic_pointer_cast<Expression>(expression->Clone());
}

DEFINE_HASH_WITH_SUPER(BracketExpression, Expression, HASH_REF(Expression, expression))

String IdentifierExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + STRING_VAR(indentation, name);
}

CLONE_METHOD(IdentifierExpression)

void IdentifierExpression::CloneImplementation(Ref<IdentifierExpression> target) const
{
	Expression::CloneImplementation(target);
	target->identifierExpressionMeta = identifierExpressionMeta;
	target->name = name;
}

DEFINE_HASH_WITH_SUPER(IdentifierExpression, Expression, HASH_VALUE(String, name))

String OperatorExpression::ToStringImplementation(UInt32 indentation) const
{
	String result = Expression::ToStringImplementation(indentation) +
	                ENUM_VAR(indentation, operatorType) + AST_VAR(indentation, a) +
	                Indentation(indentation) + "b:\n";
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
	target->operatorExpressionMeta = operatorExpressionMeta;
	target->operatorType = operatorType;
	target->a = std::dynamic_pointer_cast<Expression>(a->Clone());
	if (b)
	{
		target->b = SecondOperand();
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

DEFINE_HASH(SecondOperand, HASH_REF(Expression, expression) HASH_REF(DataType, dataType))

DEFINE_HASH_WITH_SUPER(OperatorExpression, Expression,
                       HASH_VALUE(OperatorType, operatorType) HASH_REF(Expression, a)
                           HASH_REF(SecondOperand, b))

String CallExpression::ToStringImplementation(UInt32 indentation) const
{
	String result = Expression::ToStringImplementation(indentation) + AST_VAR(indentation, method) +
	                Indentation(indentation) + "arguments = [\n";
	for (const auto& argument : arguments)
	{
		result += Indentation(indentation + 1) + argument->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(CallExpression)

void CallExpression::CloneImplementation(Ref<CallExpression> target) const
{
	Expression::CloneImplementation(target);
	target->callExpressionMeta = callExpressionMeta;
	target->method = std::dynamic_pointer_cast<Expression>(method->Clone());
	target->arguments.resize(arguments.size());
	for (UInt64 argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
	{
		target->arguments[argumentIndex] =
			std::dynamic_pointer_cast<Expression>(arguments[argumentIndex]->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(CallExpression, Expression,
                       HASH_REF(Expression, method) for (
						   UInt64 argumentIndex = 0; argumentIndex < HASH_ACCESS(arguments).size();
						   argumentIndex++){HASH_REF(Expression, arguments[argumentIndex])})

String TernaryExpression::ToStringImplementation(UInt32 indentation) const
{
	return Expression::ToStringImplementation(indentation) + AST_VAR(indentation, condition) +
	       AST_VAR(indentation, thenExpression) + AST_VAR(indentation, elseExpression);
}

CLONE_METHOD(TernaryExpression)

void TernaryExpression::CloneImplementation(Ref<TernaryExpression> target) const
{
	Expression::CloneImplementation(target);
	target->ternaryExpressionMeta = ternaryExpressionMeta;
	target->condition = std::dynamic_pointer_cast<Expression>(condition->Clone());
	target->thenExpression = std::dynamic_pointer_cast<Expression>(thenExpression->Clone());
	target->elseExpression = std::dynamic_pointer_cast<Expression>(elseExpression->Clone());
}

DEFINE_HASH_WITH_SUPER(TernaryExpression, Expression,
                       HASH_REF(Expression, condition) HASH_REF(Expression, thenExpression)
                           HASH_REF(Expression, elseExpression))

String NewExpression::ToStringImplementation(UInt32 indentation) const
{
	String result = Expression::ToStringImplementation(indentation) +
	                AST_VAR(indentation, dataType) + Indentation(indentation) + "arguments = [\n";
	for (const auto& argument : arguments)
	{
		result += Indentation(indentation + 1) + argument->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n" + ENUM_VAR(indentation, allocationType);
}

CLONE_METHOD(NewExpression)

void NewExpression::CloneImplementation(Ref<NewExpression> target) const
{
	Expression::CloneImplementation(target);
	target->newExpressionMeta = newExpressionMeta;
	target->dataType = std::dynamic_pointer_cast<DataType>(dataType->Clone());
	target->arguments.resize(arguments.size());
	for (UInt64 argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
	{
		target->arguments[argumentIndex] =
			std::dynamic_pointer_cast<Expression>(arguments[argumentIndex]->Clone());
	}
	target->allocationType = allocationType;
}

DEFINE_HASH_WITH_SUPER(
	NewExpression, Expression,
	HASH_REF(DataType, dataType) for (UInt64 argumentIndex = 0;
                                      argumentIndex < HASH_ACCESS(arguments).size();
                                      argumentIndex++){
		HASH_REF(Expression, arguments[argumentIndex])} HASH_VALUE(AllocationType, allocationType))

String Statement::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, statementType);
}

CLONE_METHOD(Statement, statementType)

void Statement::CloneImplementation(Ref<Statement> target) const
{
	ASTItem::CloneImplementation(target);
	target->statementMeta = statementMeta;
}

DEFINE_HASH_WITH_SUPER(Statement, ASTItem, HASH_VALUE(StatementType, statementType))

String BlockStatement::ToStringImplementation(UInt32 indentation) const
{
	String result = Statement::ToStringImplementation(indentation) + Indentation(indentation) +
	                "statements = [\n";
	for (const auto& statement : statements)
	{
		result += Indentation(indentation + 1) + statement->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(BlockStatement)

void BlockStatement::CloneImplementation(Ref<BlockStatement> target) const
{
	Statement::CloneImplementation(target);
	target->blockStatementMeta = blockStatementMeta;
	target->statements.resize(statements.size());
	for (UInt64 statementIndex = 0; statementIndex < statements.size(); statementIndex++)
	{
		target->statements[statementIndex] =
			std::dynamic_pointer_cast<Statement>(statements[statementIndex]->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(BlockStatement, Statement,
                       for (UInt64 statementIndex = 0;
                            statementIndex < HASH_ACCESS(statements).size();
                            statementIndex++){HASH_REF(Statement, statements[statementIndex])})

String ExpressionStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

CLONE_METHOD(ExpressionStatement)

void ExpressionStatement::CloneImplementation(Ref<ExpressionStatement> target) const
{
	Statement::CloneImplementation(target);
	target->expressionStatementMeta = expressionStatementMeta;
	target->expression = std::dynamic_pointer_cast<Expression>(expression->Clone());
}

DEFINE_HASH_WITH_SUPER(ExpressionStatement, Statement, HASH_REF(Expression, expression))

String IfStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, condition) +
	       AST_VAR(indentation, thenStatement) + AST_VAR(indentation, elseStatement);
}

CLONE_METHOD(IfStatement)

void IfStatement::CloneImplementation(Ref<IfStatement> target) const
{
	Statement::CloneImplementation(target);
	target->ifStatementMeta = ifStatementMeta;
	target->condition = std::dynamic_pointer_cast<Expression>(condition->Clone());
	target->thenStatement = std::dynamic_pointer_cast<Statement>(thenStatement->Clone());
	if (elseStatement)
	{
		target->elseStatement = std::dynamic_pointer_cast<Statement>(elseStatement->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(IfStatement, Statement,
                       HASH_REF(Expression, condition) HASH_REF(Statement, thenStatement)
                           HASH_REF(Statement, elseStatement))

String ForStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, startStatement) +
	       AST_VAR(indentation, condition) + AST_VAR(indentation, incrementExpression) +
	       AST_VAR(indentation, bodyStatement);
}

CLONE_METHOD(ForStatement)

void ForStatement::CloneImplementation(Ref<ForStatement> target) const
{
	Statement::CloneImplementation(target);
	target->forStatementMeta = forStatementMeta;
	target->startStatement = std::dynamic_pointer_cast<Statement>(startStatement->Clone());
	target->condition = std::dynamic_pointer_cast<Expression>(condition->Clone());
	target->incrementExpression =
		std::dynamic_pointer_cast<Expression>(incrementExpression->Clone());
	target->bodyStatement = std::dynamic_pointer_cast<Statement>(bodyStatement->Clone());
}

DEFINE_HASH_WITH_SUPER(ForStatement, Statement,
                       HASH_REF(Statement, startStatement) HASH_REF(Expression, condition)
                           HASH_REF(Expression, incrementExpression)
                               HASH_REF(Statement, bodyStatement))

String WhileStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, condition) +
	       AST_VAR(indentation, bodyStatement) + PRIMITIVE_VAR(indentation, checkAfterBody);
}

CLONE_METHOD(WhileStatement)

void WhileStatement::CloneImplementation(Ref<WhileStatement> target) const
{
	Statement::CloneImplementation(target);
	target->whileStatementMeta = whileStatementMeta;
	target->condition = std::dynamic_pointer_cast<Expression>(condition->Clone());
	target->bodyStatement = std::dynamic_pointer_cast<Statement>(bodyStatement->Clone());
	target->checkAfterBody = checkAfterBody;
}

DEFINE_HASH_WITH_SUPER(WhileStatement, Statement,
                       HASH_REF(Expression, condition) HASH_REF(Statement, bodyStatement)
                           HASH_VALUE(bool, checkAfterBody))

String ReturnStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

CLONE_METHOD(ReturnStatement)

void ReturnStatement::CloneImplementation(Ref<ReturnStatement> target) const
{
	Statement::CloneImplementation(target);
	target->returnStatementMeta = returnStatementMeta;
	target->expression = std::dynamic_pointer_cast<Expression>(expression->Clone());
}

DEFINE_HASH_WITH_SUPER(ReturnStatement, Statement, HASH_REF(Expression, expression))

String VariableDeclarationStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, declaration) +
	       AST_VAR(indentation, value);
}

CLONE_METHOD(VariableDeclarationStatement)

void VariableDeclarationStatement::CloneImplementation(
	Ref<VariableDeclarationStatement> target) const
{
	Statement::CloneImplementation(target);
	target->variableDeclarationStatementMeta = variableDeclarationStatementMeta;
	target->declaration = std::dynamic_pointer_cast<VariableDeclaration>(declaration->Clone());
	target->value = std::dynamic_pointer_cast<Expression>(value->Clone());
}

DEFINE_HASH_WITH_SUPER(VariableDeclarationStatement, Statement,
                       HASH_REF(VariableDeclaration, declaration) HASH_REF(Expression, value))

String DeleteStatement::ToStringImplementation(UInt32 indentation) const
{
	return Statement::ToStringImplementation(indentation) + AST_VAR(indentation, expression);
}

CLONE_METHOD(DeleteStatement)

void DeleteStatement::CloneImplementation(Ref<DeleteStatement> target) const
{
	Statement::CloneImplementation(target);
	target->deleteStatementMeta = deleteStatementMeta;
	target->expression = std::dynamic_pointer_cast<Expression>(expression->Clone());
}

DEFINE_HASH_WITH_SUPER(DeleteStatement, Statement, HASH_REF(Expression, expression))

DEFINE_HASH(TemplateArgument, HASH_REF(Expression, expression) HASH_REF(DataType, dataType))

bool TemplateArgument::operator==(const TemplateArgument& other) const
{
	EQUALS_REF(other, expression)
	EQUALS_REF(other, dataType)

	return true;
}

String Template::ToStrict() const
{
	String result;
	for (UInt32 argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
	{
		if (argumentIndex > 0)
		{
			result += ",";
		}

		const auto& argument = arguments[argumentIndex];
		if (argument.dataType)
		{
			result += argument.dataType->ToStrict();
		}
		if (argument.expression)
		{
			result += argument.expression->ToStrict();
		}
	}
	return "<" + result + ">";
}

String Template::ToStringImplementation(UInt32 indentation) const
{
	String result = Indentation(indentation) + "arguments = [\n";
	for (const auto& argument : arguments)
	{
		result += Indentation(indentation + 1);
		if (argument.dataType)
		{
			result += argument.dataType->ToString(indentation + 1);
		}
		if (argument.expression)
		{
			result += argument.expression->ToString(indentation + 1);
		}
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(Template)

void Template::CloneImplementation(Ref<Template> target) const
{
	ASTItem::CloneImplementation(target);
	target->templateMeta = templateMeta;
	target->arguments.resize(arguments.size());
	for (UInt64 argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
	{
		if (arguments[argumentIndex].dataType)
		{
			target->arguments[argumentIndex].dataType =
				std::dynamic_pointer_cast<DataType>(arguments[argumentIndex].dataType->Clone());
		}
		if (arguments[argumentIndex].expression)
		{
			target->arguments[argumentIndex].expression =
				std::dynamic_pointer_cast<Expression>(arguments[argumentIndex].expression->Clone());
		}
	}
}

DEFINE_HASH_WITH_SUPER(Template, ASTItem,
                       for (UInt64 argumentIndex = 0; argumentIndex < HASH_ACCESS(arguments).size();
                            argumentIndex++){
						   HASH_VALUE(TemplateArgument, arguments[argumentIndex])})

bool Template::operator==(const Template& other) const
{
	if (arguments.size() != other.arguments.size())
	{
		return false;
	}

	for (UInt64 argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
	{
		if (!(arguments[argumentIndex] == other.arguments[argumentIndex]))
		{
			return false;
		}
	}

	return true;
}

String UnitDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, declarationType) + ENUM_VAR(indentation, flags);
}

CLONE_METHOD(UnitDeclaration, declarationType)

void UnitDeclaration::CloneImplementation(Ref<UnitDeclaration> target) const
{
	ASTItem::CloneImplementation(target);
	target->unitDeclarationMeta = unitDeclarationMeta;
	target->flags = flags;
	target->declarationType = declarationType;
	target->name = name;
}

DEFINE_HASH_WITH_SUPER(UnitDeclaration, ASTItem,
                       HASH_VALUE(DeclarationFlags, flags)
                           HASH_VALUE(UnitDeclarationType, declarationType))

String ErrorDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return UnitDeclaration::ToStringImplementation(indentation) +
	       (hasValue ? PRIMITIVE_VAR(indentation, value) : "");
}

CLONE_METHOD(ErrorDeclaration)

void ErrorDeclaration::CloneImplementation(Ref<ErrorDeclaration> target) const
{
	UnitDeclaration::CloneImplementation(target);
	target->errorDeclarationMeta = errorDeclarationMeta;
	target->value = value;
	target->hasValue = hasValue;
}

DEFINE_HASH_WITH_SUPER(ErrorDeclaration, UnitDeclaration,
                       HASH_VALUE(bool, hasValue) HASH_VALUE(Int32, value))

String VariableDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return ENUM_VAR(indentation, flags) + ENUM_VAR(indentation, variableType) +
	       STRING_VAR(indentation, name) + AST_VAR(indentation, dataType);
}

CLONE_METHOD(VariableDeclaration)

void VariableDeclaration::CloneImplementation(Ref<VariableDeclaration> target) const
{
	ASTItem::CloneImplementation(target);
	target->variableDeclarationMeta = variableDeclarationMeta;
	target->flags = flags;
	target->variableType = variableType;
	target->name = name;
	target->dataType = std::dynamic_pointer_cast<DataType>(dataType->Clone());
}

DEFINE_HASH_WITH_SUPER(VariableDeclaration, ASTItem,
                       HASH_VALUE(DeclarationFlags, flags)
                           HASH_VALUE(VariableDeclarationType, variableType)
                               HASH_VALUE(String, name) HASH_REF(DataType, dataType))

String MethodDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = VariableDeclaration::ToStringImplementation(indentation) +
	                ENUM_VAR(indentation, methodType) + Indentation(indentation) +
	                "parameters = [\n";
	for (const auto& parameter : parameters)
	{
		result += Indentation(indentation + 1) + parameter->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n" + AST_VAR(indentation, body);
}

CLONE_METHOD(MethodDeclaration, methodType)

void MethodDeclaration::CloneImplementation(Ref<MethodDeclaration> target) const
{
	VariableDeclaration::CloneImplementation(target);
	target->methodDeclarationMeta = methodDeclarationMeta;
	target->methodType = methodType;
	target->parameters.resize(parameters.size());
	for (UInt64 parameterIndex = 0; parameterIndex < parameters.size(); parameterIndex++)
	{
		target->parameters[parameterIndex] =
			std::dynamic_pointer_cast<VariableDeclaration>(parameters[parameterIndex]->Clone());
	}
	if (body)
	{
		target->body = std::dynamic_pointer_cast<Statement>(body->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(MethodDeclaration, VariableDeclaration,
                       for (UInt64 parameterIndex = 0;
                            parameterIndex < HASH_ACCESS(parameters).size(); parameterIndex++) //
                       {HASH_REF(VariableDeclaration, parameters[parameterIndex])}             //
                       HASH_VALUE(MethodType, methodType) HASH_REF(Statement, body))

DEFINE_HASH(ConstructorInitializer, HASH_VALUE(String, name) HASH_REF(Expression, value))

String ConstructorDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = MethodDeclaration::ToStringImplementation(indentation) +
	                Indentation(indentation) + "initializers = [\n";
	for (const auto& initializer : initializers)
	{
		result += Indentation(indentation + 1) + initializer.name + ": " +
		          (initializer.value ? initializer.value->ToString(indentation + 1) : "null\n");
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(ConstructorDeclaration)

void ConstructorDeclaration::CloneImplementation(Ref<ConstructorDeclaration> target) const
{
	MethodDeclaration::CloneImplementation(target);
	target->constructorDeclarationMeta = constructorDeclarationMeta;
	target->initializers.resize(initializers.size());
	for (UInt64 initializerIndex = 0; initializerIndex < initializers.size(); initializerIndex++)
	{
		target->initializers[initializerIndex].name = initializers[initializerIndex].name;
		target->initializers[initializerIndex].value =
			std::dynamic_pointer_cast<Expression>(initializers[initializerIndex].value->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(ConstructorDeclaration, MethodDeclaration,
                       for (UInt64 initializerIndex = 0;
                            initializerIndex < HASH_ACCESS(initializers).size();
                            initializerIndex++) //
                       {HASH_VALUE(ConstructorInitializer, initializers[initializerIndex])})

String OperatorDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return MethodDeclaration::ToStringImplementation(indentation) +
	       ENUM_VAR(indentation, operatorType);
}

CLONE_METHOD(OperatorDeclaration)

void OperatorDeclaration::CloneImplementation(Ref<OperatorDeclaration> target) const
{
	MethodDeclaration::CloneImplementation(target);
	target->operatorDeclarationMeta = operatorDeclarationMeta;
	target->operatorType = operatorType;
}

DEFINE_HASH_WITH_SUPER(OperatorDeclaration, MethodDeclaration,
                       HASH_VALUE(OperatorType, operatorType))

String MemberVariableDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = VariableDeclaration::ToStringImplementation(indentation) +
	                AST_VAR(indentation, value) + Indentation(indentation) + "accessors = [\n";
	for (const auto& accessor : accessors)
	{
		result += Indentation(indentation + 1) + accessor->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(MemberVariableDeclaration)

void MemberVariableDeclaration::CloneImplementation(Ref<MemberVariableDeclaration> target) const
{
	VariableDeclaration::CloneImplementation(target);
	target->memberVariableDeclarationMeta = memberVariableDeclarationMeta;
	target->accessors.resize(accessors.size());
	for (UInt64 accessorIndex = 0; accessorIndex < accessors.size(); accessorIndex++)
	{
		target->accessors[accessorIndex] =
			std::dynamic_pointer_cast<MethodDeclaration>(accessors[accessorIndex]->Clone());
	}
	if (value)
	{
		target->value = std::dynamic_pointer_cast<Expression>(value->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(MemberVariableDeclaration, VariableDeclaration,
                       for (UInt64 accessorIndex = 0; accessorIndex < HASH_ACCESS(accessors).size();
                            accessorIndex++)                                   //
                       {HASH_REF(MethodDeclaration, accessors[accessorIndex])} //
                       HASH_REF(Expression, value))

String TemplateDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = Indentation(indentation) + "parameters = [\n";
	for (const auto& parameter : parameters)
	{
		result += Indentation(indentation + 1) + parameter->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(TemplateDeclaration)

void TemplateDeclaration::CloneImplementation(Ref<TemplateDeclaration> target) const
{
	ASTItem::CloneImplementation(target);
	target->templateDeclarationMeta = templateDeclarationMeta;
	target->parameters.resize(parameters.size());
	for (UInt64 parameterIndex = 0; parameterIndex < parameters.size(); parameterIndex++)
	{
		target->parameters[parameterIndex] =
			std::dynamic_pointer_cast<VariableDeclaration>(parameters[parameterIndex]->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(TemplateDeclaration, ASTItem,
                       for (UInt64 parameterIndex = 0;
                            parameterIndex < HASH_ACCESS(parameters).size(); parameterIndex++) //
                       {HASH_REF(VariableDeclaration, parameters[parameterIndex])})

String TypeDeclaration::ToStringImplementation(UInt32 indentation) const
{
	String result = Indentation(indentation) + "members = [\n";
	for (const auto& member : members)
	{
		result += Indentation(indentation + 1) + member->ToString(indentation + 1);
	}
	result += Indentation(indentation) + "]\n" + AST_VAR(indentation, typeTemplate) +
	          Indentation(indentation) + "superTypes = [\n";
	for (const auto& superType : superTypes)
	{
		result += Indentation(indentation + 1) + superType->ToString(indentation + 1);
	}
	return result + Indentation(indentation) + "]\n";
}

CLONE_METHOD(TypeDeclaration)

void TypeDeclaration::CloneImplementation(Ref<TypeDeclaration> target) const
{
	UnitDeclaration::CloneImplementation(target);
	target->typeDeclarationMeta = typeDeclarationMeta;
	target->members.resize(members.size());
	for (UInt64 memberIndex = 0; memberIndex < members.size(); memberIndex++)
	{
		target->members[memberIndex] =
			std::dynamic_pointer_cast<VariableDeclaration>(members[memberIndex]->Clone());
	}
	if (typeTemplate)
	{
		target->typeTemplate =
			std::dynamic_pointer_cast<TemplateDeclaration>(typeTemplate->Clone());
	}
	target->superTypes.resize(superTypes.size());
	for (UInt64 superTypeIndex = 0; superTypeIndex < superTypes.size(); superTypeIndex++)
	{
		target->superTypes[superTypeIndex] =
			std::dynamic_pointer_cast<ObjectType>(superTypes[superTypeIndex]->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(TypeDeclaration, UnitDeclaration,
                       for (UInt64 memberIndex = 0; memberIndex < HASH_ACCESS(members).size();
                            memberIndex++)                                   //
                       {HASH_REF(VariableDeclaration, members[memberIndex])} //
                       HASH_REF(TemplateDeclaration, typeTemplate)           //
                       for (UInt64 superTypeIndex = 0;
                            superTypeIndex < HASH_ACCESS(superTypes).size(); superTypeIndex++) //
                       {HASH_REF(ObjectType, superTypes[superTypeIndex])})

Ref<ConstructorDeclaration> TypeDeclaration::GetDefaultConstructor() const
{
	return std::dynamic_pointer_cast<ConstructorDeclaration>(FindMethod(MethodType::CONSTRUCTOR));
}

Ref<ConstructorDeclaration> TypeDeclaration::GetCopyConstructor() const
{
	Array<DataType*> parameters = {unitDeclarationMeta.thisType.get()};
	return std::dynamic_pointer_cast<ConstructorDeclaration>(
		FindMethod(MethodType::CONSTRUCTOR, &parameters));
}

Ref<MethodDeclaration> TypeDeclaration::GetDestructor() const
{
	return FindMethod(MethodType::DESTRUCTOR);
}

Ref<MethodDeclaration> TypeDeclaration::FindMethod(MethodType methodType,
                                                   Array<DataType*>* parameters,
                                                   const String& name) const
{
	for (auto member : members)
	{
		if (member->variableType != VariableDeclarationType::METHOD)
		{
			continue;
		}

		Ref<MethodDeclaration> method = std::dynamic_pointer_cast<MethodDeclaration>(member);
		if (method->methodType != methodType || method->name != name)
		{
			continue;
		}

		if (parameters)
		{
			if (method->parameters.size() != parameters->size())
			{
				continue;
			}

			bool select = true;
			for (UInt32 parameterIndex = 0; parameterIndex < parameters->size(); parameterIndex++)
			{
				DataType* destination =
					GetReferencedType(method->parameters[parameterIndex]->dataType.get());
				DataType* source = GetReferencedType((*parameters)[parameterIndex]);
				if (!CanCast(source, destination))
				{
					select = false;
					break;
				}
			}

			if (select)
			{
				return method;
			}
		}
		else if (method->parameters.empty())
		{
			return method;
		}
	}

	return nullptr;
}

String ClassDeclaration::ToStringImplementation(UInt32 indentation) const
{
	return TypeDeclaration::ToStringImplementation(indentation) +
	       PRIMITIVE_VAR(indentation, isSingleton);
}

CLONE_METHOD(ClassDeclaration)

void ClassDeclaration::CloneImplementation(Ref<ClassDeclaration> target) const
{
	TypeDeclaration::CloneImplementation(target);
	target->isSingleton = isSingleton;
}

DEFINE_HASH_WITH_SUPER(ClassDeclaration, TypeDeclaration, HASH_VALUE(bool, isSingleton))

Unit::Unit(const JSON& structureJSON) : ASTItem(ASTItemType::UNIT)
{
	name = structureJSON["name"];

	for (const JSON& dependencyName : structureJSON["dependencies"])
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
	for (const auto& dependencyName : dependencyNames)
	{
		result += Indentation(indentation + 1) + dependencyName + "\n";
	}
	return result + Indentation(indentation) + "]\n" + STRING_VAR(indentation, name) +
	       AST_VAR(indentation, declaredType);
}

CLONE_METHOD(Unit)

void Unit::CloneImplementation(Ref<Unit> target) const
{
	ASTItem::CloneImplementation(target);
	target->unitMeta = unitMeta;
	target->dependencyNames = dependencyNames;
	target->name = name;
	target->declaredType = std::dynamic_pointer_cast<UnitDeclaration>(declaredType->Clone());
}

DEFINE_HASH_WITH_SUPER(Unit, ASTItem, HASH_VALUE(String, name))

Ref<Unit> Unit::GetDependency(const String& name) const
{
	for (auto dependency : unitMeta.dependencies)
	{
		if (dependency->name == name)
		{
			return dependency;
		}
	}

	return nullptr;
}

bool IsTargetActive(TargetFlags target, TargetFlags buildTarget)
{
	return (target & buildTarget) == target; // target must be a subset of buildTarget to be active
}

TargetFlags JSONToTargetFlags(const JSON& json)
{
	TargetFlags flags = TargetFlags::NONE;

	for (const auto& flag : json)
	{
		flags = flags | StringToTargetFlags(String(flag));
	}

	return flags;
}

String Module::ToStringImplementation(UInt32 indentation) const
{
	return "";
}

CLONE_METHOD(Module, moduleType, name)

void Module::CloneImplementation(Ref<Module> target) const
{
	ASTItem::CloneImplementation(target);
	target->moduleMeta = moduleMeta;
	target->units.resize(units.size());
	for (UInt64 unitIndex = 0; unitIndex < units.size(); unitIndex++)
	{
		target->units[unitIndex] = std::dynamic_pointer_cast<Unit>(units[unitIndex]->Clone());
	}
	target->dependencies.resize(dependencies.size());
	for (UInt64 dependencyIndex = 0; dependencyIndex < dependencies.size(); dependencyIndex++)
	{
		target->dependencies[dependencyIndex] =
			std::dynamic_pointer_cast<Module>(dependencies[dependencyIndex]->Clone());
	}
}

DEFINE_HASH_WITH_SUPER(Module, ASTItem, HASH_VALUE(String, name))
