
#include "ResolveIdentifiers.h"

Ref<UnitDeclaration> ResolveContext::ResolveType(const String& name)
{
	if (name == unit->name)
	{
		return unit->declaredType;
	}

	Ref<TypeDeclaration> declaration = std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType);

	if (declaration->typeTemplate)
	{
		for (auto parameter : declaration->typeTemplate->parameters)
		{
			if (parameter->name != name || parameter->dataType->dataTypeType != DataTypeType::OBJECT)
			{
				continue;
			}

			Ref<UnitDeclaration> result = std::dynamic_pointer_cast<ObjectType>(parameter->dataType)->objectTypeMeta.unit;

			if (result->declarationType != UnitDeclarationType::TYPE)
			{
				continue;
			}

			return result;
		}
	}

	for (const auto& dependency : unit->dependencyNames)
	{
		if (dependency.find_last_of(name) == dependency.length() - 1 && dependency[dependency.length() - name.length() - 1] == '.')
		{
			return context->ResolveUnit(dependency)->declaredType;
		}
	}

	err.PrintError("Unable to resolve type '" + name + "'!");
	return nullptr;
}

PassResultFlags ResolveContext::ResolveTemplate(Ref<Template> typeTemplate)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto argument : typeTemplate->arguments)
	{
		if (argument.dataType)
		{
			result = result | ResolveDataType(argument.dataType);
		}
	}

	return result;
}

PassResultFlags ResolveContext::ResolveDataType(Ref<DataType> type)
{
	if (type->dataTypeType == DataTypeType::OBJECT)
	{
		Ref<ObjectType> objectType = std::dynamic_pointer_cast<ObjectType>(type);
		objectType->objectTypeMeta.unit = ResolveType(objectType->name);
		PassResultFlags result = (objectType->objectTypeMeta.unit == nullptr) ? PassResultFlags::CRITICAL_ERROR : PassResultFlags::SUCCESS;

		if (objectType->typeTemplate)
		{
			result = result | ResolveTemplate(objectType->typeTemplate);
		}

		return result;
	}

	if (type->dataTypeType == DataTypeType::ARRAY || type->dataTypeType == DataTypeType::REFERENCE || type->dataTypeType == DataTypeType::POINTER)
	{
		Ref<PointerType> pointerType = std::dynamic_pointer_cast<PointerType>(type);
		return ResolveDataType(pointerType->value);
	}

	return PassResultFlags::SUCCESS;
}

PassResultFlags ResolveContext::ResolveTemplateDeclaration(Ref<TemplateDeclaration> declaration)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto parameter : declaration->parameters)
	{
		result = result | ResolveDataType(parameter->dataType);
	}

	return result;
}

PassResultFlags ResolveContext::ResolveSuperTypes()
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto superType : type->superTypes)
	{
		result = result | ResolveDataType(superType);
	}

	return result;
}

PassResultFlags ResolveContext::ResolveMembers()
{
	return PassResultFlags::SUCCESS;
}

PassResultFlags ResolveContext::ResolveIdentifiers()
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	if (type->typeTemplate)
	{
		result = result | ResolveTemplateDeclaration(type->typeTemplate);
	}

	result = result | ResolveSuperTypes();
	result = result | ResolveMembers();

	return result;
}

PassResultFlags ResolveIdentifiers(PrintFunction print, BuildContext& context)
{
	PassResultFlags result = PassResultFlags::SUCCESS;

	for (auto module : context.GetModules())
	{
		for (auto unit : module->units)
		{
			if (unit->declaredType->IsType())
			{
				ResolveContext resolve(&context, unit->name, print, nullptr);
				resolve.unit = unit;
				resolve.type = std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType);
				resolve.ResolveIdentifiers();
			}
		}
	}

	return result;
}
