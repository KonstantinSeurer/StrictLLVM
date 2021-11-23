#ifndef SOURCE_AST_ASTPARSER
#define SOURCE_AST_ASTPARSER

#include "../Lexer.h"
#include "Unit.h"

Ref<Unit> ParseUnit(TokenStream lexer);

#endif /* SOURCE_AST_ASTPARSER */
