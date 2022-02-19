#ifndef SOURCE_PASSES_GATHERPRERESOLVEMETA
#define SOURCE_PASSES_GATHERPRERESOLVEMETA

#include "../BuildContext.h"

STRICT_FLAGS(GatherInformationFlags, NONE = 0, THIS = 1, USED_TEMPLATES = 2, PARENT = 4, MEMBER_INDEX = 8, METHOD_NAME = 16, ALL = 31)

class GatherInformationPass : public Pass
{
private:
	GatherInformationFlags flags;

public:
	GatherInformationPass(GatherInformationFlags flags) : Pass("GatherInformationPass"), flags(flags)
	{
	}

public:
	virtual PassResultFlags Run(PrintFunction print, BuildContext& context);

	void GatherInformation(BuildContext& context, Ref<Unit> unit);

private:
	void TryToInsertTemplatedObjectType(HashMap<ObjectType, Array<ObjectType*>>& target, DataType& dataType);

	void GatherInformation(Ref<TypeDeclaration> type, Ref<Expression> expression, Statement* parentStatement);
	void GatherInformation(Ref<TypeDeclaration> type, Ref<Statement> statement, Statement* parent);
	void GatherInformation(Ref<TypeDeclaration> type, Ref<MethodDeclaration> method);
	void GatherInformation(Ref<TypeDeclaration> type, Ref<MemberVariableDeclaration> variable, UInt32 index);
	void GatherInformation(Ref<TypeDeclaration> type);
};

#endif /* SOURCE_PASSES_GATHERPRERESOLVEMETA */
