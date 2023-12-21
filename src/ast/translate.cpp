#include "ast/translate.h"
#include "utils/iterator.h"
#include "analyzer/scope.h"
#include "analyzer/symbol.h"
#include "utils/symtable.h"
#include <iostream>
#include <map>

#define UNREACHABLE assert(false && "should be unreachable");
#define NYI(WHAT) assert(false && "NYI" #WHAT);

namespace fe {

ir::Type *ASTTranslator::t_type(Type *ty) {
    ir::Type *nt = nullptr;
    auto tykind = ty->get_canonical()->get_kind();
    if (tykind == typekind::pointer_t) nt = ir::PrimitiveType::get_ptr_type();

#define PRIM_TYPE_IF_CASE(TYKW) \
if (tykind == typekind::TYKW##_t) nt =  ir::PrimitiveType::get_##TYKW##_type();
#define PRIM_TYPE_ELSE_IF_CASE(TYKW) \
else PRIM_TYPE_IF_CASE(TYKW)

    PRIM_TYPE_ELSE_IF_CASE(u8)
    PRIM_TYPE_ELSE_IF_CASE(i8)
    PRIM_TYPE_ELSE_IF_CASE(u16)
    PRIM_TYPE_ELSE_IF_CASE(i16)
    PRIM_TYPE_ELSE_IF_CASE(u32)
    PRIM_TYPE_ELSE_IF_CASE(i32)
    PRIM_TYPE_ELSE_IF_CASE(u64)
    PRIM_TYPE_ELSE_IF_CASE(i64)
    PRIM_TYPE_ELSE_IF_CASE(f32)
    PRIM_TYPE_ELSE_IF_CASE(f64)
    PRIM_TYPE_ELSE_IF_CASE(void)

#undef PRIM_TYPE_ELSE_IF_CASE
#undef PRIM_TYPE_IF_CASE

    else if (tykind == typekind::alias_t) assert(false && "canonical type with kind type::alias_type?");
    else if (tykind == typekind::array_t) assert(false && "array type NYI");
    else UNREACHABLE

    //std::cout << *ty->str << " = " << nt->stringify() << std::endl;

    return nt;
}

ir::Program *ASTTranslator::t_program(ASTNode *ast) {

    assert(ast->kind == ast::translation_unit && "translation requires an AST rooted at a translation unit");
    
    // make a new program
    ir::Program *p = new ir::Program("input");

    // generate ir
    for (ASTNode *node : ast->children) {

        // only var and func decls can be here
        switch (node->kind) {
            case ast::func_decl:
                t_func(node, p);
                break;
            case ast::var_decl:
                t_gvar(node, p);
                break;
            case ast::typedef_stmt:
                // aliasing means nothing in ir,
                // so ignore this
                break;
            default:
                //node->print();
                UNREACHABLE
        }
    }

    return p;
}

ir::GlobalVar *ASTTranslator::t_gvar(ASTNode *vdecl, ir::Program *p) {
    assert(vdecl->str && "should have a str representation");
    return new ir::GlobalVar(t_type(vdecl->type), ir::linkage::external, p, false, *vdecl->str);
}

ir::Function *ASTTranslator::t_func(ASTNode *fdecl, ir::Program *p) {

    assert(fdecl->type->get_kind() == typekind::function_t);

    FunctionType *ftype = static_cast<FunctionType *>(fdecl->type);
    // make a new function
    ir::Function *f = new ir::Function(
        t_type(ftype->get_return_ty()->get_canonical()),
        ir::linkage::external,
        p,
        *fdecl->str
    );
    curr_func = f;

    // make the params
    std::vector<ir::Param *> params;
    for (unsigned i = 0; i < ftype->get_params().size(); i++) {
        params.push_back(
            new ir::Param(
                t_type(ftype->get_params()[i]->get_canonical()),
                f,
                i,
                *fdecl->children[i]->str
            )
        );
    }
    
    // make the entry block
    ir::Block *entry = new ir::Block(f, "entry");
    curr_block = entry;

    // null last lvar
    last_lvar = nullptr;

    // add param lvars (to make them mutable variables)
    for (unsigned i = 0; i < params.size(); i++) {
        auto sa = t_lvar(fdecl->children[i]);
        sa->set_name(sa->get_name() + ".addr");
        auto wi = new ir::WriteInstr(params[i], sa, curr_block);
    }
    
    // generate ir for the function body
    t_stmt(fdecl->children[params.size()]);
    
    return f;
}



ir::Def *ASTTranslator::t_stmt(ASTNode *node) {

    ir::Def *res;
    ci_lval = false;

    switch (node->kind) {
        case ast::binary_op:
            return t_binop(node);
        case ast::call_expr:
            return t_call(node);
        case ast::cast_expr:
            NYI(cast exprs)
        case ast::char_lit:
            return ir::IntegralConstant::get(
                ir::PrimitiveType::get_i8_type(),
                (*node->str)[0]
            );
        case ast::decl_stmt:
            UNREACHABLE
        case ast::error:
            UNREACHABLE
        case ast::func_decl:
            UNREACHABLE
        case ast::int_lit:
            return ir::IntegralConstant::get(
                ir::PrimitiveType::get_i32_type(),
                std::stoi(*node->str)
            );
        case ast::loop_stmt:
            return t_loop(node);
        case ast::if_stmt:
            return t_if(node);
        case ast::param_decl:
            UNREACHABLE
        case ast::paren_expr:
            for (ASTNode *c : node->children) {
                res = t_stmt(c);
            }
            return res;
        case ast::recovery:
            UNREACHABLE
        case ast::ref_expr: {
            ci_lval = true;
            return t_ref(node);
        }
        case ast::ret_stmt: {
            auto i = t_stmt(node->children[0]);
            if (ci_lval) {
                i = t_rval(i, t_type(node->children[0]->type));
            }
            return new ir::ReturnInstr(i, curr_block);
        }
        case ast::stmt_block:
            for (ASTNode *c : node->children) {
                res = t_stmt(c);
            }
            return res;
        case ast::str_lit:
            NYI(string literals)
        case ast::subscript_expr:
            NYI(subscript exprs)
        case ast::translation_unit:
            UNREACHABLE
        case ast::type:
            UNREACHABLE
        case ast::typedef_stmt:
            return nullptr;
        case ast::unary_op:
            return t_unop(node);
        case ast::var_decl:
            ci_lval = true;
            return t_lvar(node);
        default:
            UNREACHABLE
    }
}

ir::SAllocInstr *ASTTranslator::t_lvar(ASTNode *vdecl) {

    // get the type
    ir::Type *ty = t_type(vdecl->type);

    // make the instruction
    ir::SAllocInstr *sa = new ir::SAllocInstr(ty, nullptr, nullptr, *vdecl->str);

    // insert into block
    if (last_lvar) {
        sa->insert_before(last_lvar);
    }
    else {
        sa->insert_before(*curr_block->end());
    }
    last_lvar = sa;

    // add to symtable
    ir_symtable.emplace(vdecl->sym, sa);

    // add to lvar -> type map
    lvartys.emplace(sa, ty);

    // check for definition
    if (vdecl->children.size() > 0) {
        t_assign(sa, vdecl->children[0]);
    }

    return sa;
}

ir::WriteInstr *ASTTranslator::t_assign(ir::Def *lval, ASTNode *expr) {

    assert(ci_lval && "assignment to non-lval value");

    ir::Def *ir_expr = t_stmt(expr);
    if (ci_lval) { ir_expr = t_rval(ir_expr, t_type(expr->type)); }
    
    return new ir::WriteInstr(
        ir_expr,
        lval,
        curr_block
    );
}

ir::Def *ASTTranslator::t_binop(ASTNode *binop) {

    switch (binop->op) {
        case op::add:
            NYI(add)
        case op::assign: {
            return t_assign(t_stmt(binop->children[0]), binop->children[1]);
        }
        case op::div:
            NYI(div)
        case op::eq: {
            ir::Def *lhs = t_stmt(binop->children[0]);
            bool lhs_lval = ci_lval;
            ir::Def *rhs = t_stmt(binop->children[1]);
            bool rhs_lval = ci_lval;
            if (lhs_lval) { lhs = t_rval(lhs, t_type(binop->children[0]->type)); }
            if (rhs_lval) { rhs = t_rval(rhs, t_type(binop->children[1]->type)); }
            return new ir::ICmpInstr(ir::cmpkind::eq, lhs, rhs, curr_block, nullptr, "tmp");
        }
        case op::group:
            NYI(group)
        case op::gt:
            NYI(gt)
        case op::gte:
            NYI(gte)
        case op::land:
            NYI(land)
        case op::lor:
            NYI(lor)
        case op::lt:
            NYI(lt)
        case op::lte:
            NYI(lte)
        case op::mod:
            NYI(mod)
        case op::mult:
            NYI(mult)
        case op::neq:
            NYI(neq)
        case op::sub:
            NYI(sub)
        case op::postdecr:
        case op::postincr:
        case op::predecr:
        case op::preincr:
        case op::lnot:
        case op::addr:
            // unary
            UNREACHABLE
        default:
            std::cout << binop->op << std::endl;
            UNREACHABLE
    }
}

ir::CallInstr *ASTTranslator::t_call(ASTNode *cexpr) {

    // generate ir for the arguments
    std::vector<ir::Def *> args;
    for (ASTNode *a : iterator_range(cexpr->children.begin() + 1, cexpr->children.end())) {
        ir::Def *d;
        d = t_stmt(a);
        if (ci_lval) {
            d = t_rval(d, t_type(a->type));
        }
        args.push_back(d);
        //d->dump();
    }

    // get the callee function
    ir::Function *callee = curr_block->get_parent()->get_parent()->get_function(*cexpr->children[0]->str);
    assert(callee && "callee does not exist?");

    // for now, funcs only return rvalues
    ci_lval = false;

    return new ir::CallInstr(callee, args, curr_block, nullptr, "tmp");
}

/** 
 * the def returned is the pointer to the location in memory where it resides.
 * aka, its salloc instr.
*/
ir::SAllocInstr * ASTTranslator::t_ref(ASTNode *ref) {

    // get the scope of the symbol being referenced
    Scope *ast_scope = ref->sym->scope;

    auto it = ast_scope->sym_table.find(*ref->str);
    assert(it != ast_scope->sym_table.end() && "analyzer messed up");
    
    auto ist_it = ir_symtable.find(it.operator*().second);
    assert(ist_it != ir_symtable.end() && "declared symbol is not in ir_symtable");

    return ist_it.operator*().second;
}

ir::Def *ASTTranslator::t_unop(ASTNode *unop) {

    //std::cout << "\nUNOP------------\n";

    // generate ir for the inner expr
    Type *ast_inner_ty = (Type *)unop->children[0]->type;
    ir::Def *d = t_stmt(unop->children[0]);
    ir::Def *e;

    switch (unop->op) {
        case op::addr:
            // idk how to impl this rn
            NYI(addr)
        case op::indirect: {
            //unop->print_ast(unop, "  ");
            if (ci_lval) {
                d = t_rval(d, ir::PrimitiveType::get_ptr_type());
            }
            //d->dump(2);
            ci_lval = true;
            return d;
        }
        case op::lnot:
            if (ci_lval) {
                d = t_rval(d, t_type(ast_inner_ty));
            }
            return new ir::ICmpInstr(
                ir::cmpkind::eq,
                d,
                ir::IntegralConstant::get(d->get_type(), 0),
                curr_block,
                nullptr,
                "tmp"
            );
        case op::neg:
            NYI(neg)
        case op::postdecr:
        case op::postincr:
        case op::predecr:
        case op::preincr:
            // idk
            NYI(unary in/decrs)
        default:
            UNREACHABLE
    }
}

ir::ReadInstr *ASTTranslator::t_rval(ir::Def *lval, ir::Type *ty) {
    ci_lval = false;
    return new ir::ReadInstr(ty, lval, curr_block, nullptr, "tmp");
}

ir::Def *ASTTranslator::t_if(ASTNode *ifstmt) {

    // get cond
    ir::Def *cond = t_cond(ifstmt->children[0]);

    // cache the current block
    ir::Block *startblock = curr_block;

    // make if block
    ir::Block *ifblock = new ir::Block(curr_block->get_parent(), "ifthen");

    // add all inner stmts
    curr_block = ifblock;
    for (ASTNode *child : ifstmt->children[1]->children) {
        t_stmt(child);
    }

    // in the case of nested control flow, the original if block
    // we made might not be the one we need to actually branch out
    // from. Here, we update the ifblock pointer to account for this.
    // If nested control flow, this points to the natural ending block
    // in the control flow path started by the if branch. Else, this
    // just points to ifblock.
    ir::Block *if_cfpath_endblock = curr_block;

    ir::Block *elseblock = nullptr;
    ir::Block *else_cfpath_endblock = nullptr;

    // do for else, if necessary
    if (ifstmt->children.size() > 2) {

        // make else block
        elseblock = new ir::Block(curr_block->get_parent(), "ifelse");
        
        // set curr block to else block so ir gen happens in the else block
        curr_block = elseblock;

        // ir gen the inner stmts
        t_stmt(ifstmt->children[2]);

        // same thing as the ifblock to account for nested control flow
        // except we have to cache the original else block so that we
        // can make the correct branch from the end of the start block
        // to the start of the else control flow later on
        else_cfpath_endblock = curr_block;
    }

    // make done block
    ir::Block *doneblock = new ir::Block(curr_block->get_parent(), "ifdone");

    // check if the block at the end of the if control flow path has
    // a terminator already (if it defined its own control flow path)
    if (if_cfpath_endblock->size() == 0 || !if_cfpath_endblock->get_last_instr()->is_terminator()) {

        // it does not have a terminator, so we have to branch back
        // to the normal control flow path
        ir::BranchInstr *br_if_to_done = new ir::BranchInstr(doneblock, if_cfpath_endblock);
    }

    ir::Block *nextblock = doneblock;

    // if theres an else
    if (elseblock) {

        // check if the block at the end of the else control flow path has
        // a terminator already (if it defined its own control flow path)
        if (else_cfpath_endblock->size() == 0 || !else_cfpath_endblock->get_last_instr()->is_terminator()) {

            // it does not have a terminator, so we have to branch back
            // to the normal control flow path
            ir::BranchInstr *br_else_to_done = new ir::BranchInstr(doneblock, else_cfpath_endblock);
        }

        nextblock = elseblock;
    }

    // branch from the start block into the if block or the next block (else or done)
    // based on the condition
    ir::BranchInstr *br_if_to_next = new ir::BranchInstr(cond, ifblock, nextblock, startblock);

    // set the curr block to the new end block (which represents the natural
    // end of the current control flow path)
    curr_block = doneblock;

    // TODO: make this return whatever the last value in the if stmt is
    return doneblock;
}

ir::Def *ASTTranslator::t_loop(ASTNode *lnode) {
    ir::Function *f = curr_block->get_parent();
    ir::Block *loopcond = new ir::Block(f, "loopcond");
    ir::BranchInstr *curr_to_cond = new ir::BranchInstr(loopcond, curr_block);
    curr_block = loopcond;
    ir::Def *cond_inner = t_cond(lnode->children[0]);
    ir::Block *loopbody = new ir::Block(f, "loopbody");
    curr_block = loopbody;
    for (ASTNode *stmt : lnode->children[1]->children) {
        t_stmt(stmt);
    }
    ir::Block *loopend = new ir::Block(f, "loopend");
    auto condbr = new ir::BranchInstr(cond_inner, loopbody, loopend, loopcond);
    auto jmp = new ir::BranchInstr(loopcond, curr_block);
    curr_block = loopend;

    // TODO: make this return whatever the last value in the loop is
    return loopend;
}

ir::Def *ASTTranslator::t_cond(ASTNode *expr) {
    ir::Def *irexpr = t_stmt(expr);
    if (ci_lval) {
        irexpr = t_rval(irexpr, t_type(expr->type));
    }
    if (irexpr->get_type() == ir::PrimitiveType::get_i1_type()) {
        return irexpr;
    }
    assert(irexpr->get_type()->is_integral_type() && "only integral types can be coerced to boolean");
    //irexpr->dump();
    return new ir::ICmpInstr(
        ir::cmpkind::neq,
        irexpr,
        ir::IntegralConstant::get(irexpr->get_type(), 0),
        curr_block,
        nullptr,
        "tmp"
    );
}

}

#undef NYI
#undef UNREACHABLE
