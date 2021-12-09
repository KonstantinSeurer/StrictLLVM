#ifndef SOURCE_AST_ASTPARSER
#define SOURCE_AST_ASTPARSER

#include "../Lexer.h"
#include "../ErrorStream.h"
#include "AST.h"

Ref<Unit> ParseUnit(ErrorStream &err, Lexer lexer, const String &name);

#endif /* SOURCE_AST_ASTPARSER */
