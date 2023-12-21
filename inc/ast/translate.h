#ifndef AST_TRANSLATE_H
#define AST_TRANSLATE_H

#include "ast/ast.h"
#include "ir/ir.h"
#include <map>

namespace fe {

class ASTTranslator {
private:
    ir::Function *curr_func = nullptr;
    ir::Block *curr_block = nullptr;
    std::map<Symbol *, ir::SAllocInstr *> ir_symtable;
    std::map<ir::SAllocInstr *, ir::Type *> lvartys;
    bool ci_lval = false;
    ir::SAllocInstr *last_lvar = nullptr;

    ir::Type *t_type(Type *ty);
    ir::Program *t_program(ASTNode *ast);
    ir::GlobalVar *t_gvar(ASTNode *vdecl, ir::Program *p);
    ir::Function *t_func(ASTNode *fdecl, ir::Program *p);
    ir::Def *t_stmt(ASTNode *node);
    ir::SAllocInstr *t_lvar(ASTNode *vdecl);
    ir::WriteInstr *t_assign(ir::Def *lval, ASTNode *expr);
    ir::Def *t_binop(ASTNode *binop);
    ir::CallInstr *t_call(ASTNode *cexpr);
    ir::SAllocInstr *t_ref(ASTNode *ref);
    ir::Def *t_unop(ASTNode *unop);
    ir::ReadInstr *t_rval(ir::Def *lval, ir::Type *ty);
    ir::Def *t_if(ASTNode *ifstmt);
    ir::Def *t_loop(ASTNode *lnode);
    ir::Def *t_cond(ASTNode *cond);

public:
    ir::Program *translate(ASTNode *ast) { return t_program(ast); }
};

}

#endif
