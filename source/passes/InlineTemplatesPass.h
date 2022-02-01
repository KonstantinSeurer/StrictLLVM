#ifndef SOURCE_PASSES_INLINETEMPLATES
#define SOURCE_PASSES_INLINETEMPLATES

#include "GatherInformationPass.h"

class InlineTemplatesPass : public Pass
{
private:
	Ref<GatherInformationPass> gatherInformationPass;

public:
	InlineTemplatesPass();

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);

private:
	void InlineExpression(Ref<Expression>* target, const String& name, const TemplateArgument& argument);

	void InlineStatement(Ref<Statement> target, const String& name, const TemplateArgument& argument);

	void InlineMethodDeclaration(Ref<MethodDeclaration> target, const String& name, const TemplateArgument& argument);

	void InlineMemberVariableDeclaration(Ref<MemberVariableDeclaration> target, const String& name, const TemplateArgument& argument);

	void InlineVariableDeclaration(Ref<VariableDeclaration> target, const String& name, const TemplateArgument& argument);

	void InlineDataType(Ref<DataType>* target, const String& name, const TemplateArgument& argument);

	void InlineTemplateArgument(Ref<ClassDeclaration> target, const String& name, const TemplateArgument& argument);

	bool GenerateSpecializations(PrintFunction print, BuildContext& context, Ref<Module> module, const HashSet<ObjectType>& types);

	Ref<Unit> GenerateSpecialization(const ObjectType& type);
};

#endif /* SOURCE_PASSES_INLINETEMPLATES */
