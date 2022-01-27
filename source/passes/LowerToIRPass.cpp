
#include "LowerToIRPass.h"

LowerToIRPass::LowerToIRPass()
{
	context = Allocate<llvm::LLVMContext>();
	builder = Allocate<llvm::IRBuilder<>>(*context);
}

void LowerToIRPass::LowerDataType(Ref<llvm::Module> module, Ref<DataType> type)
{
	if (type->dataTypeMeta.ir)
		return;

	if (type->dataTypeType == DataTypeType::PRIMITIVE)
	{
		Ref<PrimitiveType> primitive = std::dynamic_pointer_cast<PrimitiveType>(type);
		switch (primitive->primitiveType)
		{
		case TokenType::VOID:
			type->dataTypeMeta.ir = llvm::Type::getVoidTy(*context);
			break;
		case TokenType::BOOL:
			type->dataTypeMeta.ir = llvm::Type::getInt1Ty(*context);
			break;
		case TokenType::INT8:
		case TokenType::UINT8:
			type->dataTypeMeta.ir = llvm::Type::getInt8Ty(*context);
			break;
		case TokenType::INT16:
		case TokenType::UINT16:
			type->dataTypeMeta.ir = llvm::Type::getInt16Ty(*context);
			break;
		case TokenType::INT32:
		case TokenType::UINT32:
			type->dataTypeMeta.ir = llvm::Type::getInt32Ty(*context);
			break;
		case TokenType::INT64:
		case TokenType::UINT64:
			type->dataTypeMeta.ir = llvm::Type::getInt64Ty(*context);
			break;
		case TokenType::FLOAT32:
			type->dataTypeMeta.ir = llvm::Type::getFloatTy(*context);
			break;
		case TokenType::FLOAT64:
			type->dataTypeMeta.ir = llvm::Type::getDoubleTy(*context);
			break;
		default:
			STRICT_UNREACHABLE;
		}

		STRICT_UNREACHABLE;
	}
}

void LowerToIRPass::LowerMethod(Ref<llvm::Module> module, Ref<MethodDeclaration> method)
{
	LowerDataType(module, method->dataType);

	for (auto parameter : method->parameters)
	{
		LowerDataType(module, parameter->dataType);
	}

	auto signature = llvm::FunctionType::get(method->dataType->dataTypeMeta.ir, false);
	auto function = llvm::Function::Create(signature, llvm::GlobalValue::LinkageTypes::ExternalLinkage, method->name, *module);
}

void LowerToIRPass::LowerClass(Ref<ClassDeclaration> classDeclaration)
{
	if (classDeclaration->typeTemplate)
	{
		return;
	}

	auto module = Allocate<llvm::Module>(classDeclaration->name, *context);

	for (auto member : classDeclaration->members)
	{
		if (member->variableType == VariableDeclarationType::METHOD)
		{
			LowerMethod(module, std::dynamic_pointer_cast<MethodDeclaration>(member));
		}
	}

	module->print(llvm::outs(), nullptr);

	classDeclaration->classDeclarationMeta.module = module;
}

PassResultFlags LowerToIRPass::Run(PrintFunction print, BuildContext& context)
{
	for (auto module : context.GetModules())
	{
		for (auto& specialization : module->moduleMeta.templateSpecializations)
		{
			LowerClass(specialization.second);
		}

		for (auto unit : module->units)
		{
			if (unit->declaredType->declarationType != UnitDeclarationType::CLASS)
			{
				continue;
			}

			LowerClass(std::dynamic_pointer_cast<ClassDeclaration>(unit->declaredType));
		}
	}

	return PassResultFlags::SUCCESS;
}
