#ifndef AST_TRANSLATE_H
#define AST_TRANSLATE_H

#include "ast/ast.h"
#include "ir/ir.h"

ir::Program *translate(ASTNode const *ast);

#endif
