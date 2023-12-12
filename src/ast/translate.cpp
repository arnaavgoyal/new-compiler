#include "ast/translate.h"
#include "utils/iterator.h"
#include "analyzer/scope.h"
#include "analyzer/symbol.h"
#include "utils/symtable.h"
#include <iostream>
#include <map>

#define UNREACHABLE assert(false && "should be unreachable");
#define NYI(WHAT) assert(false && "NYI" #WHAT);

ir::Type *ASTTranslator::t_type(Type const *ty) {
    ir::Type *nt = nullptr;
    switch (ty->canonical->kind) {
        case type::pointer_type:
            nt = ir::PrimitiveType::get_ptr_type();
            break;
        case type::primitive_type:

#define PRIM_TYPE_IF_CASE(TYKW) \
if (ty->canonical == Type::get_##TYKW##_type()) { \
    nt =  ir::PrimitiveType::get_##TYKW##_type(); \
}
#define PRIM_TYPE_ELSE_IF_CASE(TYKW) \
else PRIM_TYPE_IF_CASE(TYKW)

            PRIM_TYPE_IF_CASE(u8)
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

            else {
                assert(false && "type has kind type::primitive_type but doesnt match any primitives?");
            }
            break;
        case type::alias_type:
            assert(false && "canonical type with kind type::alias_type?");
            break;
        case type::array_type:
            assert(false && "array type NYI");
            break;
        case type::function_type:
        case type::error_type:
        default:
            UNREACHABLE
    }

    //std::cout << *ty->str << " = " << nt->stringify() << std::endl;

    return nt;
}

ir::Program *ASTTranslator::t_program(ASTNode const *ast) {

    assert(ast->kind == ast::translation_unit && "translation requires an AST rooted at a translation unit");
    
    // make a new program
    ir::Program *p = new ir::Program("input");

    // generate ir
    for (ASTNode const *node : ast->children) {

        // only var and func decls can be here
        switch (node->kind) {
            case ast::func_decl:
                t_func(node, p);
                break;
            case ast::var_decl:
                t_gvar(node, p);
                break;
            default:
                UNREACHABLE
        }
    }

    return p;
}

ir::GlobalVar *ASTTranslator::t_gvar(ASTNode const *vdecl, ir::Program *p) {
    assert(vdecl->str && "should have a str representation");
    return new ir::GlobalVar(t_type(vdecl->type), ir::linkage::external, p, false, *vdecl->str);
}

ir::Function *ASTTranslator::t_func(ASTNode const *fdecl, ir::Program *p) {

    // make a new function
    ir::Function *f = new ir::Function(
        t_type(fdecl->type->returns->canonical),
        ir::linkage::external,
        p,
        *fdecl->str
    );

    // make the params
    std::vector<ir::Param *> params;
    for (unsigned i = 0; i < fdecl->type->params.size(); i++) {
        params.push_back(
            new ir::Param(
                t_type(fdecl->type->params[i]->canonical),
                f,
                i,
                *fdecl->children[i]->str
            )
        );
    }
    
    // make the entry block
    ir::Block *entry = new ir::Block(f, "entry");

    // null last lvar
    last_lvar = nullptr;

    // add param lvars (to make them mutable variables)
    for (unsigned i = 0; i < params.size(); i++) {
        t_lvar(fdecl->children[i], entry);
    }
    
    // generate ir for the function body
    t_stmt(fdecl->children[params.size()], entry);
    
    return f;
}



ir::Def *ASTTranslator::t_stmt(ASTNode const *node, ir::Block *b) {

    ir::Def *res;
    ci_lval = false;

    switch (node->kind) {
        case ast::binary_op:
            return t_binop(node, b);
        case ast::call_expr:
            return t_call(node, b);
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
            return t_loop(node, b);
        case ast::if_stmt:
            return t_if(node, b);
        case ast::param_decl:
            UNREACHABLE
        case ast::paren_expr:
            for (ASTNode const *c : node->children) {
                res = t_stmt(c, b);
            }
            return res;
        case ast::recovery:
            UNREACHABLE
        case ast::ref_expr: {
            ci_lval = true;
            return t_ref(node, b);
        }
        case ast::ret_stmt:
            return new ir::ReturnInstr(t_stmt(node->children[0], b), b);
        case ast::stmt_block:
            for (ASTNode const *c : node->children) {
                res = t_stmt(c, b);
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
            return t_unop(node, b);
        case ast::var_decl:
            ci_lval = true;
            return t_lvar(node, b);
        default:
            UNREACHABLE
    }
}

ir::SAllocInstr *ASTTranslator::t_lvar(ASTNode const *vdecl, ir::Block *b) {

    // get the type
    ir::Type *ty = t_type(vdecl->type);

    // make the instruction
    ir::SAllocInstr *sa = new ir::SAllocInstr(ty, nullptr, nullptr, *vdecl->str);

    // insert into block
    if (last_lvar) {
        sa->insert_before(last_lvar);
    }
    else {
        sa->insert_before(*b->end());
    }
    last_lvar = sa;

    // add to symtable
    ir_symtable.emplace(vdecl->sym, sa);

    // add to lvar -> type map
    lvartys.emplace(sa, ty);

    // check for definition
    if (vdecl->children.size() > 0) {
        ir::WriteInstr *wi = new ir::WriteInstr(t_stmt(vdecl->children[0], b), sa, b);
    }

    return sa;
}

ir::Def *ASTTranslator::t_binop(ASTNode const *binop, ir::Block *b) {

    ir::Def *lhs = t_stmt(binop->children[0], b);
    bool lhs_lval = ci_lval;
    ir::Def *rhs = t_stmt(binop->children[1], b);
    bool rhs_lval = ci_lval;

    std::cout << "lhs (" << lhs_lval << ")\n";
    lhs->dump(2);
    std::cout << "rhs (" << rhs_lval << ")\n";
    rhs->dump(2);

    switch (binop->op) {
        case op::add:
            NYI(add)
        case op::assign: {
            if (rhs_lval) { rhs = t_rval(rhs, t_type(binop->children[1]->type), b); }
            return new ir::WriteInstr(
                rhs,
                lhs,
                b
            );
        }
        case op::div:
            NYI(div)
        case op::eq:
            if (lhs_lval) { lhs = t_rval(lhs, t_type(binop->children[0]->type), b); }
            if (rhs_lval) { rhs = t_rval(rhs, t_type(binop->children[1]->type), b); }
            return new ir::ICmpInstr(ir::cmpkind::eq, lhs, rhs, b, nullptr, "tmp");
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

ir::CallInstr *ASTTranslator::t_call(ASTNode const *cexpr, ir::Block *b) {

    // generate ir for the arguments
    std::vector<ir::Def *> args;
    for (ASTNode const *a : iterator_range(cexpr->children.begin() + 1, cexpr->children.end())) {
        ir::Def *d;
        if (a->kind == ast::ref_expr) {
            auto res = t_ref(a, b);
            d = t_rval(res, res->get_alloc_ty(), b);
        }
        else {
            d = t_stmt(a, b);
        }
        args.push_back(d);
        d->dump();
    }

    // get the callee function
    ir::Function *callee = b->get_parent()->get_parent()->get_function(*cexpr->children[0]->str);
    assert(callee && "callee does not exist?");

    return new ir::CallInstr(callee, args, b, nullptr, "tmp");
}

/** 
 * the def returned is the pointer to the location in memory where it resides.
 * aka, its salloc instr.
*/
ir::SAllocInstr * ASTTranslator::t_ref(ASTNode const *ref, ir::Block *b) {

    // get the scope of the symbol being referenced
    Scope *ast_scope = ref->sym->scope;

    auto it = ast_scope->sym_table.find(*ref->str);
    assert(it != ast_scope->sym_table.end() && "analyzer messed up");
    
    auto ist_it = ir_symtable.find(it.operator*().second);
    assert(ist_it != ir_symtable.end() && "declared symbol is not in ir_symtable");

    return ist_it.operator*().second;
}

ir::Def *ASTTranslator::t_unop(ASTNode const *unop, ir::Block *b) {

    // generate ir for the inner expr
    Type *ast_inner_ty = (Type *)unop->children[0]->type;
    ir::Def *d = t_stmt(unop->children[0], b);
    ir::Def *e;

    switch (unop->op) {
        case op::addr:
            // idk how to impl this rn
            NYI(addr)
        case op::deref:
            ci_lval = true;
            return new ir::ReadInstr(ir::PrimitiveType::get_ptr_type(), d, b, nullptr, "tmp");
        case op::lnot:
            if (ci_lval) {
                d = t_rval(d, t_type(ast_inner_ty), b);
            }
            return new ir::ICmpInstr(
                ir::cmpkind::eq,
                d,
                ir::IntegralConstant::get(d->get_type(), 0),
                b,
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

ir::ReadInstr *ASTTranslator::t_rval(ir::Def *lval, ir::Type *ty, ir::Block *b) {
    ci_lval = false;
    return new ir::ReadInstr(ty, lval, b, nullptr, "tmp");
}

ir::Def *ASTTranslator::t_if(ASTNode const *ifstmt, ir::Block *b) {

    // get cond
    ir::Def *cond = t_cond(ifstmt->children[0], b);

    // make if block
    ir::Block *ifblock = new ir::Block(b->get_parent(), "if");

    // add all inner stmts
    for (ASTNode const *child : ifstmt->children[1]->children) {
        t_stmt(child, ifblock);
    }

    ir::Block *elseblock = nullptr;

    // do for else, if necessary
    if (ifstmt->children.size() > 2) {

        // make else block
        elseblock = new ir::Block(b->get_parent(), "else");

        // add inner stmts
        t_stmt(ifstmt->children[2], elseblock);
    }

    // make done block
    ir::Block *doneblock = new ir::Block(b->get_parent(), "done");

    // add branch from if block to done block
    ir::BranchInstr *br_if_to_done = new ir::BranchInstr(doneblock, ifblock);

    ir::Block *nextblock = doneblock;

    // if theres an else
    if (elseblock) {

        // branch from else block to done block
        ir::BranchInstr *br_else_to_done = new ir::BranchInstr(doneblock, elseblock);

        nextblock = elseblock;
    }

    // branch into if block or the next block (else or done)
    ir::BranchInstr *br_if_to_next = new ir::BranchInstr(cond, ifblock, nextblock, b);

    return doneblock;
}

ir::Def *ASTTranslator::t_loop(ASTNode const *lnode, ir::Block *b) {
    ir::Function *f = b->get_parent();
    ir::Block *loopcond = new ir::Block(f, "loopcond");
    ir::Def *cond_inner = t_cond(lnode->children[0], loopcond);
    ir::Block *loopbody = new ir::Block(f, "loopbody");
    ir::Block *loopend = new ir::Block(f, "loopend");
    auto cond = new ir::BranchInstr(cond_inner, loopbody, loopend, loopcond);
    b->get_parent()->dump();
    for (ASTNode const *stmt : lnode->children[1]->children) {
        t_stmt(stmt, loopbody);
    }
    b->get_parent()->dump();
    auto jmp = new ir::BranchInstr(loopcond, loopbody);
    return loopend;
}

ir::Def *ASTTranslator::t_cond(ASTNode const *expr, ir::Block *b) {
    ir::Def *irexpr = t_stmt(expr, b);
    if (ci_lval) {
        irexpr = t_rval(irexpr, t_type(expr->type), b);
    }
    if (irexpr->get_type() == ir::PrimitiveType::get_i1_type()) {
        return irexpr;
    }
    assert(irexpr->get_type()->is_integral_type() && "only integral types can be coerced to boolean");
    irexpr->dump();
    return new ir::ICmpInstr(
        ir::cmpkind::neq,
        irexpr,
        ir::IntegralConstant::get(irexpr->get_type(), 0),
        b,
        nullptr,
        "tmp"
    );
}

#undef NYI
#undef UNREACHABLE
