
#include "InlineTemplatesPass.h"
#include "ResolveIdentifiersPass.h"

static void GenerateTemplateSpecializations(Ref<Module> module)
{
}

InlineTemplatesPass::InlineTemplatesPass() : Pass("InlineTemplatesPass")
{
	gatherInformationPass = Allocate<GatherInformationPass>(GatherInformationFlags::USED_TEMPLATES);
	gatherAllInformationPass = Allocate<GatherInformationPass>(GatherInformationFlags::ALL);
}

void InlineTemplatesPass::InlineExpression(Ref<Expression>* target, const String& name,
                                           const TemplateArgument& argument)
{
	Ref<Expression> expression = *target;
	switch (expression->expressionType)
	{
	case ExpressionType::BRACKET: {
		Ref<BracketExpression> bracketExpression =
			std::dynamic_pointer_cast<BracketExpression>(expression);
		InlineExpression(&bracketExpression->expression, name, argument);
		break;
	}
	case ExpressionType::CALL: {
		Ref<CallExpression> callExpression = std::dynamic_pointer_cast<CallExpression>(expression);
		InlineExpression(&callExpression->method, name, argument);
		for (auto& callArgument : callExpression->arguments)
		{
			InlineExpression(&callArgument, name, argument);
		}
		break;
	}
	case ExpressionType::IDENTIFIER: {
		Ref<IdentifierExpression> identifierExpression =
			std::dynamic_pointer_cast<IdentifierExpression>(expression);
		if (identifierExpression->name == name)
		{
			*target = argument.expression;
		}
		break;
	}
	case ExpressionType::NEW: {
		Ref<NewExpression> newExpression = std::dynamic_pointer_cast<NewExpression>(expression);
		InlineDataType(&newExpression->dataType, name, argument);
		for (auto& newArgument : newExpression->arguments)
		{
			InlineExpression(&newArgument, name, argument);
		}
		break;
	}
	case ExpressionType::OPERATOR: {
		Ref<OperatorExpression> operatorExpression =
			std::dynamic_pointer_cast<OperatorExpression>(expression);
		InlineExpression(&operatorExpression->a, name, argument);
		if (operatorExpression->b)
		{
			if (operatorExpression->b->expression)
			{
				InlineExpression(&operatorExpression->b->expression, name, argument);
			}
			if (operatorExpression->b->dataType)
			{
				InlineDataType(&operatorExpression->b->dataType, name, argument);
			}
		}
		break;
	}
	case ExpressionType::TERNARY: {
		Ref<TernaryExpression> ternaryExpression =
			std::dynamic_pointer_cast<TernaryExpression>(expression);
		InlineExpression(&ternaryExpression->condition, name, argument);
		InlineExpression(&ternaryExpression->thenExpression, name, argument);
		InlineExpression(&ternaryExpression->elseExpression, name, argument);
		break;
	}
	}
}

void InlineTemplatesPass::InlineStatement(Ref<Statement> target, const String& name,
                                          const TemplateArgument& argument)
{
	switch (target->statementType)
	{
	case StatementType::BLOCK: {
		Ref<BlockStatement> blockStatement = std::dynamic_pointer_cast<BlockStatement>(target);
		for (auto subStatement : blockStatement->statements)
		{
			InlineStatement(subStatement, name, argument);
		}
		break;
	}
	case StatementType::DELETE: {
		Ref<DeleteStatement> deleteStatement = std::dynamic_pointer_cast<DeleteStatement>(target);
		InlineExpression(&deleteStatement->expression, name, argument);
		break;
	}
	case StatementType::EXPRESSION: {
		Ref<ExpressionStatement> expressionStatement =
			std::dynamic_pointer_cast<ExpressionStatement>(target);
		InlineExpression(&expressionStatement->expression, name, argument);
		break;
	}
	case StatementType::FOR: {
		Ref<ForStatement> forStatement = std::dynamic_pointer_cast<ForStatement>(target);
		InlineStatement(forStatement->startStatement, name, argument);
		InlineExpression(&forStatement->condition, name, argument);
		InlineExpression(&forStatement->incrementExpression, name, argument);
		InlineStatement(forStatement->bodyStatement, name, argument);
		break;
	}
	case StatementType::IF: {
		Ref<IfStatement> ifStatement = std::dynamic_pointer_cast<IfStatement>(target);
		InlineExpression(&ifStatement->condition, name, argument);
		InlineStatement(ifStatement->thenStatement, name, argument);
		if (ifStatement->elseStatement)
		{
			InlineStatement(ifStatement->elseStatement, name, argument);
		}
		break;
	}
	case StatementType::RETURN: {
		Ref<ReturnStatement> returnStatement = std::dynamic_pointer_cast<ReturnStatement>(target);
		InlineExpression(&returnStatement->expression, name, argument);
		break;
	}
	case StatementType::VARIABLE_DECLARATION: {
		Ref<VariableDeclarationStatement> variableDeclarationStatement =
			std::dynamic_pointer_cast<VariableDeclarationStatement>(target);
		InlineVariableDeclaration(variableDeclarationStatement->declaration, name, argument);
		if (variableDeclarationStatement->value)
		{
			InlineExpression(&variableDeclarationStatement->value, name, argument);
		}
		break;
	}
	case StatementType::WHILE: {
		Ref<WhileStatement> whileStatement = std::dynamic_pointer_cast<WhileStatement>(target);
		InlineExpression(&whileStatement->condition, name, argument);
		InlineStatement(whileStatement->bodyStatement, name, argument);
		break;
	}
	}
}

void InlineTemplatesPass::InlineMethodDeclaration(Ref<MethodDeclaration> target, const String& name,
                                                  const TemplateArgument& argument)
{
	if (target->body)
	{
		InlineStatement(target->body, name, argument);
	}

	for (auto parameter : target->parameters)
	{
		InlineVariableDeclaration(parameter, name, argument);
	}
}

void InlineTemplatesPass::InlineMemberVariableDeclaration(Ref<MemberVariableDeclaration> target,
                                                          const String& name,
                                                          const TemplateArgument& argument)
{
	if (target->value)
	{
		InlineExpression(&target->value, name, argument);
	}

	for (auto accessor : target->accessors)
	{
		InlineMethodDeclaration(accessor, name, argument);
	}
}

void InlineTemplatesPass::InlineVariableDeclaration(Ref<VariableDeclaration> target,
                                                    const String& name,
                                                    const TemplateArgument& argument)
{
	InlineDataType(&target->dataType, name, argument);

	if (target->variableType == VariableDeclarationType::MEMBER_VARIABLE)
	{
		Ref<MemberVariableDeclaration> memberVariable =
			std::dynamic_pointer_cast<MemberVariableDeclaration>(target);
		InlineMemberVariableDeclaration(memberVariable, name, argument);
	}
	else if (target->variableType == VariableDeclarationType::METHOD)
	{
		Ref<MethodDeclaration> method = std::dynamic_pointer_cast<MethodDeclaration>(target);
		InlineMethodDeclaration(method, name, argument);
	}
}

void InlineTemplatesPass::InlineDataType(Ref<DataType>* target, const String& name,
                                         const TemplateArgument& argument)
{
	Ref<DataType> type = *target;
	if (type->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(type);

		if (objectType->name == name && argument.dataType)
		{
			*target = argument.dataType;
			objectType = std::dynamic_pointer_cast<ObjectType>(argument.dataType);
		}

		if (objectType && objectType->typeTemplate)
		{
			for (auto& templateArgument : objectType->typeTemplate->arguments)
			{
				if (templateArgument.dataType)
				{
					InlineDataType(&templateArgument.dataType, name, argument);
				}
				if (templateArgument.expression)
				{
					InlineExpression(&templateArgument.expression, name, argument);
				}
			}
		}
	}
	else if (type->IsPointer())
	{
		Ref<PointerType> pointerType = std::dynamic_pointer_cast<PointerType>(type);

		InlineDataType(&pointerType->value, name, argument);

		if (pointerType->arrayLength)
		{
			InlineExpression(&pointerType->arrayLength, name, argument);
		}
	}
}

void InlineTemplatesPass::InlineTemplateArgument(Ref<ClassDeclaration> target, const String& name,
                                                 const TemplateArgument& argument)
{
	for (auto& superType : target->superTypes)
	{
		InlineDataType((Ref<DataType>*)&superType, name, argument);
	}

	for (auto member : target->members)
	{
		InlineVariableDeclaration(member, name, argument);
	}
}

Ref<Unit> InlineTemplatesPass::GenerateSpecialization(const ObjectType& type)
{
	if (type.objectTypeMeta.unit->declaredType->declarationType != UnitDeclarationType::CLASS)
	{
		return nullptr;
	}

	for (const TemplateArgument& argument : type.typeTemplate->arguments)
	{
		if (!argument.dataType || argument.dataType->dataTypeType != DataTypeType::OBJECT)
		{
			continue;
		}

		Ref<ObjectType> argumentObject = std::dynamic_pointer_cast<ObjectType>(argument.dataType);

		if (argumentObject->objectTypeMeta.unit->declaredType->declarationType ==
		    UnitDeclarationType::TYPE)
		{
			return nullptr;
		}
	}

	Ref<Unit> sourceUnit = std::dynamic_pointer_cast<Unit>(type.objectTypeMeta.unit);
	Ref<ClassDeclaration> sourceClass =
		std::dynamic_pointer_cast<ClassDeclaration>(sourceUnit->declaredType);

	Ref<Unit> resultUnit = std::dynamic_pointer_cast<Unit>(sourceUnit->Clone());
	Ref<ClassDeclaration> resultClass =
		std::dynamic_pointer_cast<ClassDeclaration>(resultUnit->declaredType);

	sourceClass->typeDeclarationMeta.specializations.push_back(resultUnit);

	resultClass->name = ReplaceChar(type.ToStrict(), ' ', '-');
	resultUnit->name = resultClass->name;
	resultClass->typeTemplate = nullptr;
	resultClass->classDeclarationMeta.sourceClass = sourceClass.get();

	for (UInt32 argumentIndex = 0; argumentIndex < sourceClass->typeTemplate->parameters.size();
	     argumentIndex++)
	{
		InlineTemplateArgument(resultClass,
		                       sourceClass->typeTemplate->parameters[argumentIndex]->name,
		                       type.typeTemplate->arguments[argumentIndex]);
	}

	return resultUnit;
}

bool InlineTemplatesPass::GenerateSpecializations(PrintFunction print, BuildContext& context,
                                                  HashMap<ObjectType, Array<ObjectType*>>& types)
{
	bool progress = false;

	Array<ObjectType> eraseList;

	for (const auto& type : types)
	{
		Module* module = type.first.objectTypeMeta.unit->unitMeta.parent;

		if (module->moduleMeta.templateSpecializations.find(type.first) !=
		    module->moduleMeta.templateSpecializations.end())
		{
			continue;
		}

		Ref<Unit> specialization = GenerateSpecialization(type.first);

		if (specialization)
		{
			gatherAllInformationPass->GatherInformation(context, module, specialization);
			ResolveUnitIdentifiers(print, context, specialization);

			progress = true;
			Ref<ClassDeclaration> specializedClass =
				std::dynamic_pointer_cast<ClassDeclaration>(specialization->declaredType);
			module->moduleMeta.templateSpecializations[type.first] = specializedClass;

			for (auto typeRef : type.second)
			{
				typeRef->name = specializedClass->name;
				typeRef->typeTemplate = nullptr;
				typeRef->objectTypeMeta.unit = specialization;
			}

			eraseList.push_back(type.first);
		}
	}

	for (const auto& dataType : eraseList)
	{
		types.erase(dataType);
	}

	return progress;
}

static void MergeTemplateMaps(const HashMap<ObjectType, Array<ObjectType*>>& source,
                              HashMap<ObjectType, Array<ObjectType*>>& destination)
{
	for (const auto& type : source)
	{
		if (destination.find(type.first) == destination.end())
		{
			destination.insert(type);
		}
		else
		{
			auto references = destination.at(type.first);
			for (auto reference : type.second)
			{
				references.push_back(reference);
			}
		}
	}
}

// TODO: FIX! All usedTemplateTypes need to be merged.
PassResultFlags InlineTemplatesPass::Run(PrintFunction print, BuildContext& context)
{
	PassResultFlags result = PassResultFlags::SUCCESS;
	bool progress;
	do
	{
		result = result | gatherInformationPass->Run(print, context);

		HashMap<ObjectType, Array<ObjectType*>> usedTemplateTypes;

		for (auto module : context.GetModules())
		{
			for (auto unit : module->units)
			{
				if (!unit->declaredType->IsType())
				{
					continue;
				}

				auto& types = std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType)
				                  ->typeDeclarationMeta.usedTemplateTypes;
				MergeTemplateMaps(types, usedTemplateTypes);
			}

			for (auto specialization : module->moduleMeta.templateSpecializations)
			{
				auto& types = specialization.second->typeDeclarationMeta.usedTemplateTypes;
				MergeTemplateMaps(types, usedTemplateTypes);
			}
		}

		progress = GenerateSpecializations(print, context, usedTemplateTypes);
	} while (progress);

	return result;
}
