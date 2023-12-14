#ifndef AST_TRANSLATE_H
#define AST_TRANSLATE_H

#include "ast/ast.h"
#include "ir/ir.h"
#include <map>

class ASTTranslator {
private:
    ir::Function *curr_func = nullptr;
    ir::Block *curr_block = nullptr;
    std::map<Symbol *, ir::SAllocInstr *> ir_symtable;
    std::map<ir::SAllocInstr *, ir::Type *> lvartys;
    bool ci_lval = false;
    ir::SAllocInstr *last_lvar = nullptr;

    ir::Type *t_type(Type const *ty);
    ir::Program *t_program(ASTNode const *ast);
    ir::GlobalVar *t_gvar(ASTNode const *vdecl, ir::Program *p);
    ir::Function *t_func(ASTNode const *fdecl, ir::Program *p);
    ir::Def *t_stmt(ASTNode const *node);
    ir::SAllocInstr *t_lvar(ASTNode const *vdecl);
    ir::WriteInstr *t_assign(ir::Def *lval, ASTNode const *expr);
    ir::Def *t_binop(ASTNode const *binop);
    ir::CallInstr *t_call(ASTNode const *cexpr);
    ir::SAllocInstr *t_ref(ASTNode const *ref);
    ir::Def *t_unop(ASTNode const *unop);
    ir::ReadInstr *t_rval(ir::Def *lval, ir::Type *ty);
    ir::Def *t_if(ASTNode const *ifstmt);
    ir::Def *t_loop(ASTNode const *lnode);
    ir::Def *t_cond(ASTNode const *cond);

public:
    ir::Program *translate(ASTNode const *ast) { return t_program(ast); }
};

#endif
