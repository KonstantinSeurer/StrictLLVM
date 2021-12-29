
#include "LowerImpliedDeclarationFlags.h"

static HashSet<OperatorType> mutatingOperatorSet = {
	// Mutating binary operators
	OperatorType::PLUS_EQUAL, OperatorType::MINUS_EQUAL, OperatorType::MULTIPLY_EQUAL, OperatorType::DIVIDE_EQUAL, OperatorType::AND_EQUAL,
	OperatorType::OR_EQUAL, OperatorType::XOR_EQUAL, OperatorType::SHIFT_LEFT_EQUAL, OperatorType::SHIFT_RIGHT_EQUAL,
	// Mutating unary operators
	OperatorType::INCREMENT, OperatorType::DECREMENT};

void LowerImpliedDeclarationFlags(Ref<MethodDeclaration> method, bool isVirtualType)
{
	switch (method->methodType)
	{
	case MethodType::SETTER:
	case MethodType::CONSTRUCTOR:
		method->flags = method->flags | DeclarationFlags::MUT;
		break;
	case MethodType::DESTRUCTOR:
		method->flags =
			method->flags | DeclarationFlags::MUT | DeclarationFlags::PUBLIC | (isVirtualType ? DeclarationFlags::VIRTUAL : DeclarationFlags::PRIVATE);
		break;
	case MethodType::OPERATOR: {
		Ref<OperatorDeclaration> operatorDeclaration = std::dynamic_pointer_cast<OperatorDeclaration>(method);
		if (mutatingOperatorSet.find(operatorDeclaration->operatorType) != mutatingOperatorSet.end())
		{
			method->flags = method->flags | DeclarationFlags::MUT;
		}
		break;
	}
	default:
		break;
	}
}

void LowerImpliedDeclarationFlags(Ref<MemberVariableDeclaration> variable)
{
	for (auto accessor : variable->accessors)
	{
		LowerImpliedDeclarationFlags(accessor, false);
	}
}

void LowerImpliedDeclarationFlags(Ref<TypeDeclaration> type)
{
	for (auto member : type->members)
	{
		if (member->variableType == VariableDeclarationType::MEMBER_VARIABLE)
		{
			LowerImpliedDeclarationFlags(std::dynamic_pointer_cast<MemberVariableDeclaration>(member));
		}
		else if (member->variableType == VariableDeclarationType::METHOD)
		{
			const bool isVirtualType = (type->flags & DeclarationFlags::VIRTUAL) == DeclarationFlags::VIRTUAL;
			LowerImpliedDeclarationFlags(std::dynamic_pointer_cast<MethodDeclaration>(member), isVirtualType);
		}
	}
}

PassResultFlags LowerImpliedDeclarationFlags(PrintFunction print, BuildContext& context)
{
	for (auto module : context.GetModules())
	{
		for (auto unit : module->units)
		{
			if (unit->declaredType->IsType())
			{
				LowerImpliedDeclarationFlags(std::dynamic_pointer_cast<TypeDeclaration>(unit->declaredType));
			}
		}
	}

	return PassResultFlags::SUCCESS;
}
