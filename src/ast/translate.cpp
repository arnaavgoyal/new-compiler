#include "ast/translate.h"
#include "utils/iterator.h"
#include "analyzer/scope.h"
#include "analyzer/symbol.h"
#include "utils/symtable.h"
#include <iostream>
#include <map>

#define UNREACHABLE assert(false && "should be unreachable");
#define NYI(WHAT) assert(false && "NYI" #WHAT);

static std::map<Symbol *, ir::SAllocInstr *> ir_symtable;

static ir::Def *translate_in_func_body(ASTNode const *node, ir::Function *f);
static std::pair<Symbol *, ir::SAllocInstr *> translate_ref_expr(ASTNode const *ref, ir::Function *f);

static ir::Type *translate_type(Type const *ty) {
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
            [[fallthrough]];
        case type::error_type:
            [[fallthrough]];
        default:
            UNREACHABLE
    }

    //std::cout << *ty->str << " = " << nt->stringify() << std::endl;

    return nt;
}

static ir::Def *translate_binop_expr(ASTNode const *binop, ir::Function *f) {
    switch (binop->op) {
        case op::add:
            UNREACHABLE
        case op::addr:
            UNREACHABLE
        case op::assign:
            assert(binop->children[0]->kind == ast::ref_expr && "can only with direct references in lvalue assignments");
            return new ir::WriteInstr(
                translate_in_func_body(binop->children[1], f),
                translate_ref_expr(binop->children[0], f).second,
                f->get_last_block()
            );
        case op::decr:
            UNREACHABLE
        case op::div:
            UNREACHABLE
        case op::eq:
            UNREACHABLE
        case op::group:
            UNREACHABLE
        case op::gt:
            UNREACHABLE
        case op::gte:
            UNREACHABLE
        case op::incr:
            UNREACHABLE
        case op::land:
            UNREACHABLE
        case op::lnot:
            UNREACHABLE
        case op::lor:
            UNREACHABLE
        case op::lt:
            UNREACHABLE
        case op::lte:
            UNREACHABLE
        case op::mod:
            UNREACHABLE
        case op::mult:
            UNREACHABLE
        case op::neq:
            UNREACHABLE
        case op::sub:
            UNREACHABLE
        default:
            std::cout << binop->op << std::endl;
            UNREACHABLE
    }
}

static ir::SAllocInstr *translate_lvar_decl(ASTNode const *vdecl, ir::Function *f) {

    // add to function
    ir::Block *b = f->get_last_block();
    ir::SAllocInstr *sa = new ir::SAllocInstr(translate_type(vdecl->type), b, *vdecl->str);
    std::cout << "var ty: " << sa->get_type()->stringify() << std::endl;

    // add to symtable
    ir_symtable.emplace(vdecl->sym, sa);

    // check for definition
    if (vdecl->children.size() > 0) {
        ir::WriteInstr *wi = new ir::WriteInstr(translate_in_func_body(vdecl->children[0], f), sa, f->get_last_block());
    }

    return sa;
}

static ir::ReadInstr *get_rval_from_lval(std::pair<Symbol *, ir::SAllocInstr *> p, ir::Function *f) {
    return new ir::ReadInstr(
        translate_type(p.first->type_ptr),
        p.second,
        f->get_last_block(),
        "tmp"
    );
}

static ir::CallInstr *translate_call_expr(ASTNode const *cexpr, ir::Function *f) {
    std::vector<ir::Def *> args;
    for (ASTNode const *a : iterator_range(cexpr->children.begin() + 1, cexpr->children.end())) {
        ir::Def *d;
        if (a->kind == ast::ref_expr) {
            d = get_rval_from_lval(translate_ref_expr(a, f), f);
        }
        else {
            d = translate_in_func_body(a, f);
        }
        args.push_back(d);
        d->dump();
    }
    ir::Function *callee = f->get_parent()->get_function(*cexpr->children[0]->str);
    assert(callee && "callee does not exist?");
    return new ir::CallInstr(
        callee,
        args,
        f->get_last_block(),
        "tmp"
    );
}

/** 
 * the def returned is the pointer to the location in memory where it resides.
 * aka, its salloc instr.
*/
static std::pair<Symbol *, ir::SAllocInstr *> translate_ref_expr(ASTNode const *ref, ir::Function *f) {

    Scope *ast_scope = ref->sym->scope;

    //std::cout << std::endl << *ref->str << std::endl << std::endl;
    //ast_scope->dump_me();
    //std::cout << std::endl;

    auto it = ast_scope->sym_table.find(*ref->str);
    assert(it != ast_scope->sym_table.end() && "analyzer messed up");
    
    auto ist_it = ir_symtable.find(it.operator*().second);
    assert(ist_it != ir_symtable.end() && "declared symbol is not in ir_symtable");

    return ist_it.operator*();
}

static ir::Def *translate_in_func_body(ASTNode const *node, ir::Function *f) {
    std::cout << "doing this node: \n";
    node->print();
    ir::Def *res;
    switch (node->kind) {
        case ast::binary_op:
            return translate_binop_expr(node, f);
        case ast::call_expr:
            return translate_call_expr(node, f);
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
            NYI(loops)
        case ast::param_decl:
            UNREACHABLE
        case ast::paren_expr:
            for (ASTNode const *c : node->children) {
                res = translate_in_func_body(c, f);
            }
            return res;
        case ast::recovery:
            UNREACHABLE
        case ast::ref_expr:
            // if there is just a random ref expr with no operator then use it as rval
            return get_rval_from_lval(translate_ref_expr(node, f), f);
        case ast::ret_stmt:
            return new ir::ReturnInstr(
                translate_in_func_body(node->children[0], f),
                f->get_last_block()
            );
        case ast::stmt_block:
            for (ASTNode const *c : node->children) {
                res = translate_in_func_body(c, f);
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
            NYI(unary ops)
        case ast::var_decl:
            std::cout << "var decl" << std::endl;
            return translate_lvar_decl(node, f);
        default:
            UNREACHABLE
    }
}

static ir::Function *translate_func_decl(ASTNode const *fdecl, ir::Program *p) {
    ir::Function *f = new ir::Function(
        translate_type(fdecl->type->returns->canonical),
        ir::linkage::external,
        p,
        *fdecl->str
    );
    std::vector<ir::Param *> params;
    for (unsigned i = 0; i < fdecl->type->params.size(); i++) {
        params.push_back(
            new ir::Param(
                translate_type(fdecl->type->params[i]->canonical),
                f,
                i,
                *fdecl->children[i]->str
            )
        );
    }
    //std::cout << "params done" << std::endl;
    ir::Block *entry = new ir::Block(f, "entry");
    for (unsigned i = 0; i < params.size(); i++) {
        std::cout << "adding salloc for param " << params[i]->get_name() << "_mut" << std::endl;
        auto pvar = new ir::SAllocInstr(params[i]->get_type(), entry, params[i]->get_name() + "_mut");
        auto en = ir_symtable.emplace(
            fdecl->children[i]->sym,
            pvar
        );
        std::cout << "param alloc " << en.first.operator*().first->name << std::endl;
    }
    std::cout << "func done" << std::endl;
    //std::cout << "entry done" << std::endl;
    auto param_end_it = fdecl->children.begin() + fdecl->type->params.size();
    ir::Def *d;
    translate_in_func_body(fdecl->children.rbegin().operator*(), f);
    std::cout << entry->size() << " instrs in entry block\n";
    std::cout << "body done" << std::endl;
    return f;
}

static ir::GlobalVar *translate_gvar_decl(ASTNode const *vdecl, ir::Program *p) {
    assert(vdecl->str && "should have a str representation");
    return new ir::GlobalVar(translate_type(vdecl->type), ir::linkage::external, p, false, *vdecl->str);
}

ir::Program *translate(ASTNode const *ast) {
    assert(ast->kind == ast::translation_unit && "translation requires an AST rooted at a translation unit");
    ir::Program *p = new ir::Program("input");
    for (ASTNode const *node : ast->children) {
        // only var and func decls can be here
        switch (node->kind) {
            case ast::func_decl:
                translate_func_decl(node, p);
                break;
            case ast::var_decl:
                translate_gvar_decl(node, p);
                break;
            default:
                UNREACHABLE
        }
    }
    return p;
}

#undef NYI
#undef UNREACHABLE
