#ifndef SOURCE_PASSES_LOWERIMPLICITCASTS
#define SOURCE_PASSES_LOWERIMPLICITCASTS

#include "../BuildContext.h"

class LowerImplicitCastsPass : public Pass
{
public:
	LowerImplicitCastsPass() : Pass("LowerImplicitCastsPass")
	{
	}

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);

private:
	void LowerExpression(MethodDeclaration* method, Ref<Expression>* pExpression,
	                     Ref<DataType> expectedType);

	void LowerStatement(MethodDeclaration* method, Statement* statement);

	void LowerMethodDeclaration(MethodDeclaration* MethodDeclaration);

	void LowerMemberVariableDeclaration(MemberVariableDeclaration* variable);

	void LowerVariableDeclaration(VariableDeclaration* variable);

	void LowerClass(ClassDeclaration* classDeclaration);
};

#endif /* SOURCE_PASSES_LOWERIMPLICITCASTS */
