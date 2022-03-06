
#ifndef SOURCE_PASSES_LOWEROPERATORS
#define SOURCE_PASSES_LOWEROPERATORS

#include "../BuildContext.h"

class LowerOperatorsPass : public Pass
{
public:
	LowerOperatorsPass();

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);

private:
	void LowerOperatorExpression(Ref<Expression>* pExpression);

	void LowerExpression(Ref<Expression>* pExpression);

	void LowerStatement(Ref<Statement> statement);

	void LowerMethodDeclaration(Ref<MethodDeclaration> MethodDeclaration);

	void LowerMemberVariableDeclaration(Ref<MemberVariableDeclaration> variable);

	void LowerVariableDeclaration(Ref<VariableDeclaration> variable);

	void LowerClass(Ref<ClassDeclaration> classDeclaration);
};

#endif
