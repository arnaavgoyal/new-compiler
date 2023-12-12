#ifndef AST_TRANSLATE_H
#define AST_TRANSLATE_H

#include "ast/ast.h"
#include "ir/ir.h"
#include <map>

class ASTTranslator {
private:
    ir::Function *curr_func;
    ir::Block *curr_block;
    std::map<Symbol *, ir::SAllocInstr *> ir_symtable;
    std::map<ir::SAllocInstr *, ir::Type *> lvartys;
    bool ci_lval = false;
    ir::SAllocInstr *last_lvar = nullptr;

    ir::Type *t_type(Type const *ty);
    ir::Program *t_program(ASTNode const *ast);
    ir::GlobalVar *t_gvar(ASTNode const *vdecl, ir::Program *p);
    ir::Function *t_func(ASTNode const *fdecl, ir::Program *p);
    ir::Def *t_stmt(ASTNode const *node, ir::Block *b);
    ir::SAllocInstr *t_lvar(ASTNode const *vdecl, ir::Block *b);
    ir::Def *t_binop(ASTNode const *binop, ir::Block *b);
    ir::CallInstr *t_call(ASTNode const *cexpr, ir::Block *b);
    ir::SAllocInstr *t_ref(ASTNode const *ref, ir::Block *b);
    ir::Def *t_unop(ASTNode const *unop, ir::Block *b);
    ir::ReadInstr *t_rval(ir::Def *lval, ir::Type *ty, ir::Block *b);
    ir::Def *t_if(ASTNode const *ifstmt, ir::Block *b);
    ir::Def *t_loop(ASTNode const *lnode, ir::Block *b);

public:
    ir::Program *translate(ASTNode const *ast) { return t_program(ast); }
};

#endif
