#include "analyzer/analyzer.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include "memory/allocator.h"
#include "diagnostic/diagnostic.h"
#include "analyzer/scope.h"
#include <cassert>
#include <cstdlib>
#include <bitset>

namespace fe {

Scope *SemanticAnalyzer::global_scope() {
    return gscope;
}

void SemanticAnalyzer::enter_new_scope(Scope **curr) {
    Scope *sc = scope_allocator.alloc();
    sc->parent = *curr;
    *curr = sc;
}
void SemanticAnalyzer::exit_current_scope(Scope **curr) {
    *curr = (*curr)->parent;
}

Symbol *SemanticAnalyzer::find_symbol_in_current_scope(
    std::string *ident,
    Scope *scope
) {
    std::map<std::string, Symbol *>::iterator
        iter = scope->sym_table.find(*ident),
        end = scope->sym_table.end();

    if (iter == end) {
        return nullptr;
    }

    return iter->second;
}

Symbol *SemanticAnalyzer::find_symbol_in_any_active_scope(
    std::string *ident,
    Scope *scope
) {

    Scope *curr = scope;
    while (curr) {
        std::map<std::string, Symbol *>::iterator
            iter = curr->sym_table.find(*ident),
            end = curr->sym_table.end();

        if (iter != end) {
            return iter->second;
        }

        curr = curr->parent;
    }

    // not found
    return nullptr;
}

Symbol *SemanticAnalyzer::insert_symbol(
    Scope *scope,
    std::string const &name,
    Type *type_ptr,
    ASTNode *decl
) {
    Symbol *sym = sym_allocator.alloc();
    sym->name = name;
    sym->type_ptr = type_ptr;
    sym->scope = scope;
    sym->decl = decl;
    scope->sym_table.insert(std::make_pair(name, sym));
    std::cout << "inserted " << name
        << " of type " << type_ptr->stringify()
        << " into symtable\n";
    return sym;
}

Type *SemanticAnalyzer::find_type_in_current_scope(
    std::string *ident,
    Scope *scope
) {
    std::map<std::string, Type *>::iterator
        iter = scope->type_table.find(*ident),
        end = scope->type_table.end();

    if (iter == end) {
        return nullptr;
    }

    return iter->second;
}

Type *SemanticAnalyzer::find_type_in_any_active_scope(
    std::string *ident,
    Scope *scope
) {
    Scope *curr = scope;
    while (curr) {
        std::map<std::string, Type *>::iterator
            iter = curr->type_table.find(*ident),
            end = curr->type_table.end();

        if (iter != end) {
            return iter->second;
        }

        curr = curr->parent;
    }

    // not found
    return nullptr;  
}

void SemanticAnalyzer::insert_type(
    Scope *scope,
    std::string const &key,
    Type *value
) {
    scope->type_table.insert(std::make_pair(key, value));
}

bool SemanticAnalyzer::typecheck_assignment(ASTNode *lhs, ASTNode *rhs) {

    // typechecks are done on canonical types
    auto cfrom = rhs->type->get_canonical();
    auto cto = lhs->type->get_canonical();

    bool compat = true;

    // they are the same types
    if (cfrom == cto) {
        // we don't have to do anything
    }

    // both are integral types
    else if (cfrom->is_integral() && cto->is_integral()) {

        // compatible types, but ...

        if (cfrom->is_signed() && cto->is_unsigned()) {

            // unsigned 'to' = signed 'from'
            // if 'from' is negative, its value will be clobbered when assigned to 'to'
            
            // emit warning
            DiagnosticHandler::make(diag::id::warn_assignment_signed_to_unsigned, rhs->loc)
                .add(cfrom->stringify())
                .add(cto->stringify())
                .finish();
        }

        // downcast
        if (cfrom->get_size() > cto->get_size()) {

            // downcasts are valid but must be done with explicit casts
            DiagnosticHandler::make(diag::id::assignment_requires_explicit_downcast, rhs->loc)
                .add(cto->stringify())
                .add(cfrom->stringify())
                .finish();
            
            compat = false;
        }

        // upcast
        else if (cfrom->get_size() < cto->get_size()) {

            // upcasts are valid and can be done implicitly

            // for integral literals ...
            if (rhs->kind == ast::int_lit) {
                // upcast silently to avoid tons of worthless warnings
            }

            // for anything else ...
            else {
                // emit a warning
                DiagnosticHandler::make(diag::id::warn_assignment_implicit_upcast, rhs->loc)
                    .add(cfrom->stringify())
                    .add(cto->stringify())
                    .finish();
            }
        }

        // types are same size
        else {

            if (cfrom->is_unsigned() && cto->is_signed()) {

                // signed 'to' = unsigned 'from', where each type is the same size
                // if 'from' is positive and large, its value may be truncated when assigned to 'to'

                // emit warning
                DiagnosticHandler::make(diag::id::warn_assignment_unsigned_to_signed, rhs->loc)
                    .add(cfrom->stringify())
                    .add(cto->stringify())
                    .finish();
            }
        }
    }

    // both are floating point types
    else if (cfrom->is_fp() && cto->is_fp()) {
        DiagnosticHandler::make(diag::id::nyi, rhs->loc)
            .add("fp-to-fp casts")
            .finish();

        compat = false;
    }

    // both are pointer types
    else if (cfrom->get_kind() == typekind::pointer_t && cto->get_kind() == typekind::pointer_t) {
        
        // these differ at some point in their types.
        // pointer-to-pointer casts are valid, but warn
        DiagnosticHandler::make(diag::id::warn_assignment_pointer_to_pointer_cast, rhs->loc)
            .add(cfrom->stringify())
            .add(cto->stringify())
            .finish();
    }

    else {
        // error-type
        // return compatible bc there is no point in propagating
        //   an error that is already reported
    }

    return compat;
}

SemanticAnalyzer::SemanticAnalyzer(
    Allocator<std::string> &str_allocator,
    std::vector<token::token_type> const &primitives
) :
    str_allocator(str_allocator),
    primitive_keywords(primitives) {

    // create error node
    error_node = node_allocator.alloc();
    error_node->kind = ast::error;
    error_node->str = str_allocator.alloc();
    *(std::string *)error_node->str = std::string("<error-node>");
    error_node->type = Type::get_error_type();

    AliasType::set_error_node(error_node);

    // IRRELEVANT NOW
    // // add all primitive types
    // Type *type_ptr;
    // for (
    //     std::vector<token::token_type>::const_iterator
    //         i = primitives.begin(),
    //         end = primitives.end();
    //     i != end;
    //     i++
    // ) {
    //     type_ptr = type_allocator.alloc();
    //     type_ptr->kind = type::primitive_type;
    //     str = str_allocator.alloc();
    //     str->assign(token::get_keyword_string(*i));
    //     type_ptr->str = str;
    //     type_ptr->get_canonical() = type_ptr;
    //     type_ptr->contains_error = false;
    //     primitive_types.push_back(type_ptr);
    // }

    // create global scope
    gscope = scope_allocator.alloc();
}

SemanticAnalyzer::~SemanticAnalyzer() {
    // auto it = gscope->type_table.begin();
    // for ( ; it != gscope->type_table.end(); it++) {
    //     std::cout << (*it).first << std::endl;
    // }
}

Type *get_most_recent_non_alias(Type *ty) {
    while (ty->get_kind() == typekind::alias_t) {
        ty = static_cast<AliasType *>(ty)->get_aliasee();
    }
    return ty;
}

FunctionType *SemanticAnalyzer::analyze_function_type(
    Scope **scope,
    std::vector<Type *> param_types,
    Type *return_type,
    SourceLocation start_loc,
    SourceLocation end_loc
) {

    // track if the func def is canonical
    bool iscanon = true;

    // make string representations
    std::string rep = "(";
    std::string can_rep = "(";
    std::vector<Type *>::iterator
        i = param_types.begin(),
        end = param_types.end();
    bool start = true;
    for ( ; i != end; i++) {
        if (!start) {
            rep += ",";
            can_rep += ",";
        }
        else {
            start = false;
        }
        rep += (*i)->stringify();
        can_rep += (*i)->get_canonical()->stringify();
        if ((*i)->get_canonical() != (*i)) {
            iscanon = false;
        }
    }
    rep += ")" + return_type->stringify();
    can_rep += ")" + return_type->get_canonical()->stringify();
    if (return_type != return_type->get_canonical()) {
        iscanon = false;
    }

    // find type
    FunctionType *type = (FunctionType *)find_type_in_any_active_scope(&rep, *scope);

    // already exists
    if (type) {
        assert(type->get_kind() == typekind::function_t);
        return type;
    }

    // does not exist yet

    // attempt to find canon type
    FunctionType *canon = (FunctionType *)find_type_in_current_scope(&can_rep, gscope);

    // canon already exists
    if (canon) {
        assert(canon->get_kind() == typekind::function_t);

        // then the type itself is not canon

        // create str rep for type
        std::string *str = str_allocator.alloc();
        *str = rep;

        // create type
        type = new FunctionType(canon, return_type, param_types);

        // insert into curr scope
        insert_type(*scope, *str, type);

        return type;
    }

    // canon does not already exist
    else {

        // create str rep for canon
        std::string *can_str = str_allocator.alloc();
        *can_str = can_rep;

        // create canon
        std::vector<Type *> canon_param_types;
        for (auto pty : param_types) {
            canon_param_types.push_back(pty->get_canonical());
        }
        canon = new FunctionType(nullptr, return_type->get_canonical(), canon_param_types);

        // insert into global scope
        insert_type(gscope, *can_str, canon);

        // the type is not canon
        if (!iscanon) {

            // create str rep for type
            std::string *str = str_allocator.alloc();
            *str = rep;

            // create type
            type = new FunctionType(canon, return_type, param_types);

            // insert into curr scope
            insert_type(*scope, *str, type);

            return type;
        }

        else {
            return canon;
        }
    }
}

AliasType *SemanticAnalyzer::analyze_typename(
    Scope **scope,
    std::string *ident,
    SourceLocation loc
) {
    // get type
    Type *type = find_type_in_any_active_scope(ident, *scope);

    // type does not exist
    if (type == nullptr) {
        // ErrorHandler::handle(error::ident_is_not_a_typename, loc, ident->c_str());
        DiagnosticHandler::make(diag::id::use_of_undeclared_type, loc)
            .add(*ident)
            .finish();
        return AliasType::get_error_type();
    }

    // type exists
    assert(type->is_definable());
    return static_cast<AliasType *>(type);
}

PointerType *SemanticAnalyzer::analyze_pointer_type(
    Scope **scope,
    Type *pointee,
    SourceLocation pointer_modifier_loc
) {

    // make string representation
    std::string rep = "*" + pointee->stringify();

    // find type
    PointerType *type = (PointerType *)find_type_in_any_active_scope(&rep, *scope);

    // already exists
    if (type) {
        assert(type->get_kind() == typekind::pointer_t);
        return type;
    }

    // does not exist yet

    // make sure is not func type
    if (pointee->get_kind() == typekind::function_t) {
        // ErrorHandler::handle(
        //     error::pointer_to_function_type,
        //     pointer_modifier_loc,
        //     "cannot make pointers to function types"
        // );
        DiagnosticHandler::make(diag::id::pointer_to_function_type_invalid, pointer_modifier_loc)
            .finish();
        return PointerType::get_error_type();
    }

    // create str rep
    std::string *str = str_allocator.alloc();
    *str = rep;

    // requires a separate canon
    if (pointee->get_canonical() != pointee) {

        // make string rep for canon
        std::string *can_str = str_allocator.alloc();
        *can_str = "*" + pointee->get_canonical()->stringify();

        // attempt to find canon (in global scope)
        PointerType *canon = (PointerType *)find_type_in_any_active_scope(can_str, gscope);

        // not found
        if (canon == nullptr) {
            
            // make new canon type
            canon = new PointerType(nullptr, pointee->get_canonical());

            // insert into global scope
            insert_type(gscope, *can_str, canon);
        }

        // make type
        type = new PointerType(canon, pointee);

        // insert into local scope (since it is a non-canonical type)
        insert_type(*scope, *str, type);
    }

    // is canon itself
    else {

        // then the new type will also be canon
        type = new PointerType(nullptr, pointee);

        // insert into global scope (since it is a modified builtin)
        insert_type(gscope, *str, type);
    }

    // return result
    return type;
}

ArrayType *SemanticAnalyzer::analyze_array_type(
    Scope **scope,
    Type *array_of,
    SourceLocation array_modifier_loc
) {
    // TODO: implement
    // ErrorHandler::handle(error::nyi, array_modifier_loc, "array types");
    // ErrorHandler::prog_exit();
    DiagnosticHandler::make(diag::id::nyi, array_modifier_loc)
        .add("array types")
        .finish();
    return ArrayType::get_error_type();
}

ASTNode *SemanticAnalyzer::analyze_cast_expr(
    ASTNode *casted_expr,
    Type *cast_type,
    SourceLocation loc
) {
    // TODO: maybe check typecast compatibility

    ASTNode *cast_node = node_allocator.alloc();
    cast_node->set(
        ast::cast_expr,
        cast_type,
        nullptr,
        loc,
        token::kw_as,
        false
    );
    cast_node->children.push_back(casted_expr);
    return cast_node;
}

PrimitiveType *SemanticAnalyzer::analyze_primitive_type(
    token::token_type prim,
    SourceLocation loc
) {
    assert(token::is_primitive_type(prim) && "analyze_primitive_type() called with non-primitive type?");

    // THIS IS ALSO IRRELEVANT NOW
    // // this must be valid because the set of primitives is given
    // unsigned int idx = std::distance(
    //     primitive_keywords.begin(),
    //     std::find(
    //         primitive_keywords.begin(),
    //         primitive_keywords.end(),
    //         prim
    //     )
    // );
    // return (Type *)primitive_types[idx];

    return PrimitiveType::get_prim(prim);
}

ASTNode *SemanticAnalyzer::analyze_binary_op_expr(
    op::kind op,
    ASTNode *lhs,
    ASTNode *rhs,
    token::token_type tok,
    SourceLocation op_loc
) {
    // ErrorHandler::handle(error::nyi, op_loc, "binary operations");
    // ErrorHandler::prog_exit();

    Type *op_type;
    SourceLocation total_op_loc = lhs->loc;
    total_op_loc.copy_end(rhs->loc);

    // propagate potential error state
    bool err = lhs->has_error || rhs->has_error;

    if (op::is_val_op(op)) {

        // type compare
        if (lhs->type->get_canonical() != rhs->type->get_canonical()) {
            err = true;
            DiagnosticHandler::make(diag::id::binary_op_typecheck, total_op_loc)
                .add(token::get_operator_string(tok))
                .add(lhs->type->stringify())
                .add(rhs->type->stringify())
                .finish();
        }
        
        op_type = lhs->type;
    }

    else if (op::is_rel_op(op)) {

        // type compare
        if (lhs->type->get_canonical() != rhs->type->get_canonical()) {
            err = true;
            DiagnosticHandler::make(diag::id::binary_op_typecheck, total_op_loc)
                .add(token::get_operator_string(tok))
                .add(lhs->type->stringify())
                .add(rhs->type->stringify())
                .finish();
        }

        op_type = PrimitiveType::get_i8_type();
    }

    else if (op::is_log_op(op)) {

        // ErrorHandler::handle(error::nyi, op_loc, "logical bin ops");
        DiagnosticHandler::make(diag::id::nyi, total_op_loc)
                .add("logical binary operations")
                .finish();

        op_type = Type::get_error_type();
        err = true;
    }

    else if (op == op::group) {
        
        // type is last expr's type
        op_type = rhs->type;
    }

    else if (op::is_lval_op(op)) {
        if (!typecheck_assignment(lhs, rhs)) {
            err = true;
        }
        op_type = lhs->type;
    }

    else {
        std::cout << "unknown op: " << op << std::endl;
        assert(false && "unknown");
    }


    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::binary_op,
        op_type,
        nullptr,
        total_op_loc,
        tok,
        err
    );
    node->op = op;
    node->children.push_back(lhs);
    node->children.push_back(rhs);

    return node;
}

ASTNode *SemanticAnalyzer::analyze_postfix_op_expr(
    op::kind op,
    token::token_type tk,
    ASTNode *expr,
    SourceLocation loc
) {
    // ErrorHandler::handle(error::nyi, op_loc, "postfix operations");
    // ErrorHandler::prog_exit();

    bool err = expr->has_error;
    auto errloc = loc;
    errloc.copy_start(expr->loc);
    auto ty = expr->type;

    switch (op) {
        case op::postincr:
        case op::postdecr:
            // the expr must be of lvalue type
            if (!expr->is_lvalue) {
                // ErrorHandler::handle(error::lvalue_required, loc, "postfix in/decrement cannot be applied to non-lvalue values");
                DiagnosticHandler::make(diag::id::side_effecting_unary_typecheck_lvalue, errloc)
                    .add(expr->type->stringify())
                    .finish();
                err = true;
                ty = Type::get_error_type();
            }
            // the lvalue must be of integral type
            if (!expr->type->is_integral()) {
                // ErrorHandler::handle(error::integral_type_required, loc, "postfix in/decrement cannot be applied to non-integral types");
                DiagnosticHandler::make(diag::id::side_effecting_unary_typecheck_integral, errloc)
                    .add(expr->type->stringify())
                    .finish();
                err = true;
                ty = Type::get_error_type();
            }
            break;
        default:
            assert(false && "should be unreachable");
    }

    loc.copy_start(expr->loc);

    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::unary_op,
        ty,
        nullptr,
        loc,
        tk,
        err
    );
    node->op = op;
    node->children.push_back(expr);

    return node;
}

ASTNode *SemanticAnalyzer::analyze_call_expr(
    ASTNode *expr,
    std::vector<ASTNode *> args,
    SourceLocation call_start_loc,
    SourceLocation call_end_loc
) {
    // ErrorHandler::handle(error::nyi, call_start_loc, "call expressions");
    // ErrorHandler::prog_exit();

    FunctionType *callable = (FunctionType *)get_most_recent_non_alias(expr->type);
    bool error = expr->has_error;
    ASTNode *node = node_allocator.alloc();
    node->children.push_back(expr);
    SourceLocation call_loc;
    call_loc.copy_src(call_start_loc);
    call_loc.copy_end(call_end_loc);

    // verify func type
    if (callable->get_kind() != typekind::function_t) {
        // ErrorHandler::handle(
        //     error::type_is_not_callable,
        //     expr->loc,
        //     "does not have type that is callable"
        // );
        DiagnosticHandler::make(diag::id::noncallable_expression, expr->loc)
            .add(expr->type->stringify())
            .finish();
        node->set(
            ast::call_expr,
            Type::get_error_type(),
            nullptr,
            call_loc,
            token::op_leftparen,
            true
        );

        return node;
    }

    unsigned num_params = callable->params.size();

    // verify arg types
    ASTNode *arg;
    std::vector<Type *> const paramtypes = callable->params;
    unsigned i;
    for (i = 0; i < num_params; i++) {
        if (i == args.size()) {
            // too few args
            std::cout << expr->type->stringify() << std::endl;
            DiagnosticHandler::make(diag::id::call_expr_too_few_arguments, call_end_loc)
                .add(std::to_string(num_params))
                .add(std::to_string(args.size()))
                .add(expr->type->stringify())
                .finish();
            break;
        }
        arg = args[i];
        if (arg->type == Type::get_error_type()) {
            error = true;
        }
        else if (arg->type->get_canonical() != paramtypes[i]->get_canonical()) {
            error = true;
            // ErrorHandler::handle(
            //     error::argument_type_mismatch,
            //     arg->loc,
            //     "argument has wrong type"
            // );
            DiagnosticHandler::make(diag::id::call_expr_typecheck_argument, arg->loc)
                .add(paramtypes[i]->stringify())
                .add(arg->type->stringify())
                .finish();
        }
        node->children.push_back(args[i]);
    }

    if (i < args.size()) {
        // too many args
        SourceLocation errloc = args[i]->loc;
        errloc.copy_end(args.back()->loc);
        DiagnosticHandler::make(diag::id::call_expr_too_many_arguments, errloc)
            .add(std::to_string(num_params))
            .add(std::to_string(args.size()))
            .add(expr->type->stringify())
            .finish();
    }

    node->set(
        ast::call_expr,
        callable->get_return_ty(),
        nullptr,
        call_start_loc,
        token::op_leftparen,
        error
    );

    return node;
}

ASTNode *SemanticAnalyzer::analyze_subscript_expr(
    ASTNode *expr,
    ASTNode *subscript,
    SourceLocation subscript_start_loc,
    SourceLocation subscript_end_loc
) {
    // TODO: implement
    // ErrorHandler::handle(error::nyi, subscript_start_loc, "array subscripts");
    // ErrorHandler::prog_exit();
    auto errloc = subscript_start_loc;
    errloc.copy_end(subscript_end_loc);
    DiagnosticHandler::make(diag::id::nyi, errloc)
        .add("subscript expressions")
        .finish();
    return error_node;
}

ASTNode *SemanticAnalyzer::analyze_paren_expr(
    ASTNode *inside,
    SourceLocation loc
) {
    ASTNode *pexpr = node_allocator.alloc();
    pexpr->set(
        ast::paren_expr,
        inside->type,
        nullptr,
        loc,
        token::op_leftparen,
        inside->has_error
    );
    pexpr->is_lvalue = inside->is_lvalue;
    pexpr->children.push_back(inside);

    return pexpr;
}

ASTNode *SemanticAnalyzer::analyze_ref_expr(
    Scope **scope,
    std::string *ident,
    SourceLocation loc
) {
    // ErrorHandler::handle(error::nyi, loc, "reference expressions");
    // ErrorHandler::prog_exit();

    // TODO: allow for scope stack climbing to find shadowed symbols
    //       if current scope's symbol is invalid

    // find symbol
    Symbol *res = find_symbol_in_any_active_scope(ident, *scope);

    Type *sym_type;
    bool err = false;

    // no symbol
    if (res == nullptr) {
        sym_type = Type::get_error_type();
        err = true;
        // ErrorHandler::handle(
        //     error::referenced_ident_is_undefined,
        //     loc,
        //     "identifier is undefined"
        // );
        DiagnosticHandler::make(diag::id::use_of_undeclared_symbol, loc)
            .add(*ident)
            .finish();
    }
    else {
        sym_type = res->type_ptr;
    }

    ASTNode *expr = node_allocator.alloc();
    expr->set(
        ast::ref_expr,
        sym_type,
        ident,
        loc,
        token::identifier,
        err
    );
    expr->sym = (Symbol *)res;
    expr->is_lvalue = true;

    return expr;
}

ASTNode *SemanticAnalyzer::analyze_character_literal(
    std::string *char_lit,
    SourceLocation loc
) {
    // TODO: implement
    // ErrorHandler::handle(error::nyi, loc, "character literals");
    // ErrorHandler::prog_exit();
    DiagnosticHandler::make(diag::id::nyi, loc)
        .add("character literals")
        .finish();
    return error_node;
}

ASTNode *SemanticAnalyzer::analyze_numeric_literal(
    std::string *num_lit,
    SourceLocation loc
) {
    // ErrorHandler::handle(error::nyi, loc, "numeric literals");
    // ErrorHandler::prog_exit();

    // assume no overflow and default size i32 for now
    // TODO: size check and dynamic type assignment

    // the str rep of this int lit should never be negative
    assert(isdigit(num_lit->operator[](0)));

    uint64_t val = std::strtoull(num_lit->c_str(), nullptr, 0);

    // value does not fit in 64 bits
    if (val == ULLONG_MAX && errno == ERANGE) {
        DiagnosticHandler::make(diag::id::integral_literal_too_big, loc)
            .add(*num_lit)
            .finish();
    }

    std::cout << "int lit <" << *num_lit << ">\n  " << std::bitset<64>(val) << std::endl;

    // by default set as 8 bits (smallest integral type)
    Type *ty = PrimitiveType::get_u8_type();

    // test the most significant 32 bits
    if (val >> 32) {
        // requires 64 bits
        ty = PrimitiveType::get_u64_type();
    }

    // test the most significant 16 bits of the least significant 32 bits
    else if (static_cast<uint32_t>(val) >> 16) {
        // requires 32 bits
        ty = PrimitiveType::get_u32_type();
    }

    // test the most significant 8 bits of the least significant 16 bits
    else if (static_cast<uint16_t>(val) >> 8) {
        // requires 16 bits
        ty = PrimitiveType::get_u16_type();
    }

    // else, the default 8 bit type can contain the value

    // For now, override literal type narrowing and assume 32 bits in all cases
    //Type *ty = PrimitiveType::get_u32_type();

    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::int_lit,
        ty,
        num_lit,
        loc,
        token::numeric_literal,
        false
    );

    return node;
}

ASTNode *SemanticAnalyzer::analyze_string_literal(
    std::string *str_lit,
    SourceLocation loc
) {
    // TODO: implement
    // ErrorHandler::handle(error::nyi, loc, "string literals");
    // ErrorHandler::prog_exit();
    DiagnosticHandler::make(diag::id::nyi, loc)
        .add("string literals")
        .finish();
    return error_node;
}

ASTNode *SemanticAnalyzer::analyze_prefix_op_expr(
    op::kind op,
    token::token_type tk,
    ASTNode *expr,
    SourceLocation loc
) {

    bool err = expr->has_error;
    Type *ty = expr->type;
    bool lval = false;
    auto errloc = loc;
    errloc.copy_end(expr->loc);

    switch (op) {
        case op::preincr:
        case op::predecr:
            // the expr must be lvalue
            if (!expr->is_lvalue) {
                // ErrorHandler::handle(error::lvalue_required, loc, "prefix in/decrement cannot be applied to non-lvalue values");
                DiagnosticHandler::make(diag::id::side_effecting_unary_typecheck_lvalue, errloc)
                    .add(ty->stringify())
                    .finish();
                err = true;
            }
            // the lvalue must be of integral type
            if (!expr->type->is_integral()) {
                // ErrorHandler::handle(error::integral_type_required, loc, "prefix in/decrement cannot be applied to non-integral types");
                DiagnosticHandler::make(diag::id::side_effecting_unary_typecheck_integral, errloc)
                    .add(ty->stringify())
                    .finish();
                err = true;
            }
            // resulting value is an lval
            lval = true;
            break;
        case op::addr:
            // the expr must be lvalue
            if (!expr->is_lvalue) {
                // ErrorHandler::handle(error::lvalue_required, loc, "a non-lvalue value does not have an address");
                DiagnosticHandler::make(diag::id::address_of_typecheck, errloc)
                    .add(ty->stringify())
                    .finish();
                err = true;
            }
            // resulting value is pointer type
            // TODO: how do I get uniqued pointer type to expr type from here?
            ty = Type::get_error_type();
            break;
        case op::indirect:
            // the expr must be of pointer type
            if (ty->get_canonical()->get_kind() != typekind::pointer_t) {
                // ErrorHandler::handle(error::pointer_type_required, loc, "value of non-pointer type cannot be dereferenced");
                DiagnosticHandler::make(diag::id::indirection_typecheck, errloc)
                    .add(ty->stringify())
                    .finish();
                err = true;
                ty = Type::get_error_type();
            }
            else {
                ty = static_cast<PointerType *>(ty->get_canonical())->get_pointee();
            }
            lval = true;
            break;
        case op::lnot:
            // expr must be of integral type
            if (!ty->is_integral()) {
                // ErrorHandler::handle(error::integral_type_required, loc, "logical not cannot be applied to non-integral types");
                DiagnosticHandler::make(diag::id::logical_not_typecheck, errloc)
                    .add(ty->stringify())
                    .finish();
                err = true;
            }
            // resulting value is bool
            ty = PrimitiveType::get_i8_type();
            break;
        case op::neg:
            // expr must be of integral type
            if (!ty->is_integral()) {
                DiagnosticHandler::make(diag::id::unary_negate_typecheck, errloc)
                    .add(ty->stringify())
                    .finish();
                err = true;
                ty = Type::get_error_type();

                // FIXME: check for signedness and type capacity after negation
            }
            break;
        default:
            assert(false && "should be unreachable");
    }

    loc.copy_end(expr->loc);

    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::unary_op,
        ty,
        nullptr,
        loc,
        tk,
        err
    );
    node->op = op;
    node->is_lvalue = lval;
    node->children.push_back(expr);

    return node;
}

ASTNode *SemanticAnalyzer::analyze_func_decl(
    Scope **scope,
    std::vector<std::tuple<Type *, std::string *, SourceLocation>> params,
    Type *rtype,
    std::string *ident,
    SourceLocation ident_loc,
    SourceLocation param_list_start_loc,
    SourceLocation param_list_end_loc
) {
    // ErrorHandler::handle(error::nyi, ident_loc, "function declarations");
    // ErrorHandler::prog_exit();
    std::vector<Type *> ptypes, cptypes;
    bool diff = false;
    for (auto [ty, str, loc] : params) {
        std::cout << ty->stringify() << " " << *str << "\n";
        ptypes.push_back(ty);
        cptypes.push_back(ty->get_canonical());
        if (ty != ty->get_canonical()) {
            diff = true;
        }
    }
    
    FunctionType *canon = new FunctionType(nullptr, rtype->get_canonical(), cptypes);
    std::cout << "haha0\n";
    std::string *tmp = str_allocator.alloc();
    *tmp = canon->stringify();
    std::cout << canon->stringify() << "\n";
    insert_type(*scope, *tmp, canon);
    FunctionType *type = canon;

    if (diff) {
        std::cout << "haha0.5\n";
        type = new FunctionType(canon, rtype, ptypes);
        *tmp = type->stringify();
        insert_type(*scope, *tmp, type);
    }
    std::cout << "haha0.5\n";

    // redeclaration in current scope
    if (Symbol *osym = find_symbol_in_current_scope(ident, *scope)) {
        // ErrorHandler::handle(
        //     error::redeclaration_of_symbol_in_same_scope,
        //     ident_loc,
        //     "redeclared symbol"
        // );
        DiagnosticHandler::make(diag::id::symbol_redeclaration, ident_loc)
            .add(*ident)
            .finish();
        DiagnosticHandler::make(diag::id::note_original_declaration, osym->decl->loc)
            .finish();
    }
    std::cout << "haha1\n";
    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::func_decl,
        type,
        ident,
        ident_loc,
        token::unknown,
        type->has_error()
    );
    std::cout << "haha2\n";

    // add to symbol table
    Symbol *sym = insert_symbol(*scope, *ident, type, node);
    std::cout << "haha3\n";

    node->sym = sym;
    std::cout << "haha4\n";

    // enter new scope
    enter_new_scope(scope);
    std::cout << "haha5\n";

    // add params
    ASTNode *param;
    unsigned i;
    for (i = 0; i < type->get_params().size(); i++) {

        if (i == params.size()) {
            // too few params
            DiagnosticHandler::make(diag::id::func_decl_too_few_parameters, param_list_end_loc)
                .add(*ident)
                .add(type->stringify())
                .add(std::to_string(type->params.size()))
                .add(std::to_string(params.size()))
                .finish();
            break;
        }

        // make node
        param = node_allocator.alloc();
        param->set(
            ast::param_decl,
            type->params[i],
            std::get<1>(params[i]),
            std::get<2>(params[i]),
            token::identifier,
            type->params[i]->has_error()
        );

        // add to symbol table
        sym = insert_symbol(
            *scope,
            *std::get<1>(params[i]),
            type->params[i],
            param
        );

        param->sym = sym;

        // add to decl node
        node->children.push_back(param);
    }

    if (i < params.size()) {
        // too many params
        auto errloc = std::get<2>(params[i]);
        errloc.copy_end(std::get<2>(params.back()));
        DiagnosticHandler::make(diag::id::func_decl_too_many_parameters, errloc)
            .add(*ident)
            .add(type->stringify())
            .add(std::to_string(type->params.size()))
            .add(std::to_string(params.size()))
            .finish();
    }

    return node;
}

void SemanticAnalyzer::start_func_define(
    ASTNode *decl,
    SourceLocation define_start_loc
) {
    // scope should already have been entered
    // do nothing i guess
}

void SemanticAnalyzer::end_func_define(
    Scope **scope,
    SourceLocation define_end_loc
) {
    // exit scope
    exit_current_scope(scope);
}

ASTNode *SemanticAnalyzer::analyze_var_decl(
    Scope **scope,
    Type *type,
    std::string *ident,
    SourceLocation ident_loc
) {
    ASTNode *decl = node_allocator.alloc();
    decl->set(
        ast::var_decl,
        type,
        ident,
        ident_loc,
        token::identifier,
        type->has_error()
    );

    std::cout << "\n";
    (*scope)->dump();

    // ensure no redeclaration
    if (auto osym = find_symbol_in_current_scope(ident, *scope)) {
        // ErrorHandler::handle(
        //     error::redeclaration_of_symbol_in_same_scope,
        //     ident_loc,
        //     ident->c_str()
        // );
        DiagnosticHandler::make(diag::id::symbol_redeclaration, ident_loc)
            .add(*ident)
            .finish();
        DiagnosticHandler::make(diag::id::note_original_declaration, osym->decl->loc)
            .finish();
        decl->has_error = true;
    }

    Symbol *sym = insert_symbol(*scope, *ident, type, decl);
    decl->sym = sym;

    std::cout << "\n";
    (*scope)->dump();
    std::cout << "\n";

    ASTNode *res = decl;
    return res;
}

ASTNode *SemanticAnalyzer::analyze_var_decl(
    Scope **scope,
    Type *type,
    std::string *ident,
    SourceLocation ident_loc,
    ASTNode *rhs,
    SourceLocation eqloc
) {
    ASTNode *decl_node = analyze_var_decl(scope, type, ident, ident_loc);
    decl_node->children.push_back(rhs);
    bool compat = typecheck_assignment(decl_node, rhs);
    if (!compat) decl_node->has_error = true;
    return decl_node;
}

ASTNode *SemanticAnalyzer::analyze_type_alias(
    Scope **scope,
    Type *type,
    std::string *ident,
    SourceLocation ident_loc
) {
    bool err = false;
    Type *alias;
    ASTNode *stmt = node_allocator.alloc();

    // ensure no redeclaration
    if (auto oty = find_type_in_current_scope(ident, *scope)) {
        assert(oty->get_kind() == typekind::alias_t);
        // ErrorHandler::handle(
        //     error::redeclaration_of_symbol_in_same_scope,
        //     ident_loc,
        //     ident->c_str()
        // );
        DiagnosticHandler::make(diag::id::type_redeclaration, ident_loc)
            .add(*ident)
            .finish();
        DiagnosticHandler::make(diag::id::note_original_declaration, static_cast<AliasType *>(oty)->get_decl()->loc)
            .finish();
        err = true;
        alias = Type::get_error_type();
    }
    else {

        // add alias to type table
        alias = new AliasType(type, *ident, stmt);

        insert_type(*scope, *ident, alias);
    }

    stmt->set(
        ast::typedef_stmt,
        type,
        ident,
        ident_loc,
        token::identifier,
        err
    );

    return stmt;
}

void SemanticAnalyzer::start_scoped_block(
    Scope **scope,
    SourceLocation block_start_loc
) {
    enter_new_scope(scope);
}

void SemanticAnalyzer::end_scoped_block(
    Scope **scope,
    SourceLocation block_end_loc
) {
    exit_current_scope(scope);
}

ASTNode *SemanticAnalyzer::analyze_return_stmt(
    SourceLocation retloc,
    ASTNode *expr
) {
    ASTNode *node = node_allocator.alloc();
    retloc.copy_end(expr->loc);
    node->set(
        ast::ret_stmt,
        expr->type,
        nullptr,
        retloc,
        token::kw_return,
        expr->has_error
    );
    node->children.push_back(expr);

    return node;
}

ASTNode *SemanticAnalyzer::analyze_loop_stmt(
    ASTNode *cond,
    SourceLocation loc
) {
    // TODO: ensure cond is integral type
    ASTNode *loop = node_allocator.alloc();
    loop->set(
        ast::loop_stmt,
        cond->type,
        nullptr,
        loc,
        token::kw_while,
        false
    );
    loop->children.push_back(cond);
    return loop;
}

ASTNode *SemanticAnalyzer::analyze_if_stmt(
    ASTNode *cond,
    SourceLocation loc
) {
    // TODO: ensure cond is boolean (or boolean coercible)
    ASTNode *ifstmt = node_allocator.alloc();
    ifstmt->set(
        ast::if_stmt,
        cond->type,
        nullptr,
        loc,
        token::kw_if,
        false
    );
    ifstmt->children.push_back(cond);
    return ifstmt;
}

void SemanticAnalyzer::add_expr_as_stmt(
    ASTNode *expr
) {
    // TODO: implement
}

ASTNode *SemanticAnalyzer::analyze_tmpl_decl(
    Scope **scope,
    std::vector<std::pair<std::string *, SourceLocation>> params,
    SourceLocation decl_loc
) {
    ASTNode *tmpl = node_allocator.alloc();
    tmpl->set(
        ast::tmpl_decl,
        nullptr, // this is set later
        nullptr,
        decl_loc,
        token::op_less,
        false
    );

    // enter new scope
    enter_new_scope(scope);

    std::vector<Type *> ptys;
    for (auto &[ident, loc] : params) {
        ASTNode *tmplparam = node_allocator.alloc();
        Type *ty = new TemplatePlaceholderType(*ident, tmplparam);
        tmplparam->set(
            ast::tmpl_param_decl,
            ty,
            ident,
            loc,
            token::identifier,
            false
        );
        tmpl->children.push_back(tmplparam);
        ptys.push_back(ty);
        insert_type(*scope, *ident, ty);
    }

    Type *tmpl_ty = new UninstantiatedTemplateType(
        Type::get_error_type(), // updated with the base type in end_tmpl_define
        ptys,
        tmpl
    );
    tmpl->type = tmpl_ty;

    return tmpl;
}

void SemanticAnalyzer::end_tmpl_define(
    Scope **scope,
    ASTNode *tmpl,
    ASTNode *inner
) {
    exit_current_scope(scope);
    if (inner) {
        tmpl->children.push_back(inner);
        static_cast<UninstantiatedTemplateType *>(tmpl->type)->base = inner->type;
    }
    else {
        tmpl->children.push_back(error_node);
        tmpl->type = Type::get_error_type();
    }
}

}
