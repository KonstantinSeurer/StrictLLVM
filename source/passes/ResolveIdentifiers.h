#ifndef SOURCE_PASSES_RESOLVEIDENTIFIERS
#define SOURCE_PASSES_RESOLVEIDENTIFIERS

#include "../BuildContext.h"

class ResolveContext
{
public:
	BuildContext* context;
	ErrorStream err;
	Ref<Unit> unit;
	Ref<TypeDeclaration> type;

public:
	ResolveContext(BuildContext* context, const String& fileName, PrintFunction print, Ref<const Lexer> lexer) : context(context), err(fileName, print, lexer)
	{
	}

private:
	Ref<UnitDeclaration> ResolveType(const String& name);

	PassResultFlags ResolveDataType(Ref<DataType> type);

	PassResultFlags ResolveTemplate(Ref<Template> typeTemplate);

	PassResultFlags ResolveTemplateDeclaration(Ref<TemplateDeclaration> declaration);

	PassResultFlags ResolveSuperTypes();

	PassResultFlags ResolveMembers();

public:
	PassResultFlags ResolveIdentifiers();
};

PassResultFlags ResolveIdentifiers(PrintFunction print, BuildContext& context);

#endif /* SOURCE_PASSES_RESOLVEIDENTIFIERS */
