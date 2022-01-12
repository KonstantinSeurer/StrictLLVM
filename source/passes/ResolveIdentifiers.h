#ifndef SOURCE_PASSES_RESOLVEIDENTIFIERS
#define SOURCE_PASSES_RESOLVEIDENTIFIERS

#include "../BuildContext.h"

STRICT_ENUM(ResolvePass, NONE, DATA_TYPES, EXPRESSION, NEW)

STRICT_FLAGS(TraversalLevel, NONE = 0, DECLARATION = 1, STATEMENT = 3, EXPRESSION = 7)

class ResolveContext
{
public:
	BuildContext* context;
	ErrorStream err;
	Ref<Unit> unit;
	Ref<TypeDeclaration> type;

private:
	ResolvePass pass;
	TraversalLevel requiredTraversalLevel;

public:
	ResolveContext(BuildContext* context, const String& fileName, PrintFunction print, const Lexer* lexer)
		: context(context), err(fileName, print, lexer), pass(ResolvePass::NONE)
	{
	}

private:
	bool IsTraversalRequired(TraversalLevel level) const;

	Ref<UnitDeclaration> ResolveType(const String& name, bool optional);

	PassResultFlags ResolveDataType(Ref<MethodDeclaration> method, Ref<DataType> type);

	PassResultFlags ResolveTemplate(Ref<MethodDeclaration> method, Ref<Template> typeTemplate);

	PassResultFlags ResolveTemplateDeclaration(Ref<TemplateDeclaration> declaration);

	PassResultFlags ResolveSuperTypes();

	PassResultFlags ResolveOperatorExpression(Ref<MethodDeclaration> method, Ref<OperatorExpression> expression);

	PassResultFlags ResolveIdentifierExpression(Ref<ObjectType> context, Ref<MethodDeclaration> method, Ref<IdentifierExpression> expression, bool required);

	PassResultFlags ResolveBracketExpression(Ref<MethodDeclaration> method, Ref<BracketExpression> expression);

	Ref<DataType> ConvertExpressionToDataType(Ref<Expression> expression);

	PassResultFlags ResolveCallExpression(Ref<MethodDeclaration> method, Ref<Expression>* expression);

	PassResultFlags ResolveNewExpression(Ref<MethodDeclaration> method, Ref<NewExpression> expression);

	PassResultFlags ResolveTernaryExpression(Ref<MethodDeclaration> method, Ref<TernaryExpression> expression);

	PassResultFlags ResolveExpression(Ref<MethodDeclaration> method, Ref<Expression>* expression);

	PassResultFlags ResolveStatement(Ref<MethodDeclaration> method, Ref<Statement> statement);

	PassResultFlags ResolveMethodDeclaration(Ref<MethodDeclaration> method);

	PassResultFlags ResolveMemberVariableDeclaration(Ref<MemberVariableDeclaration> variable);

	PassResultFlags ResolveVariableDeclaration(Ref<VariableDeclaration> variable);

	PassResultFlags ResolveMembers();

public:
	void SetPass(ResolvePass pass);

	PassResultFlags ResolveIdentifiers();
};

PassResultFlags ResolveIdentifiers(PrintFunction print, BuildContext& context);

#endif /* SOURCE_PASSES_RESOLVEIDENTIFIERS */
