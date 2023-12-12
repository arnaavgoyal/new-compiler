#include "analyzer/analyzer.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include "memory/allocator.h"
#include "error/error.h"
#include "analyzer/scope.h"
#include <cassert>

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

Symbol const *SemanticAnalyzer::find_symbol_in_current_scope(
    std::string const *ident,
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

Symbol const *SemanticAnalyzer::find_symbol_in_any_active_scope(
    std::string const *ident,
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
    Type const *type_ptr
) {
    Symbol *sym = sym_allocator.alloc();
    sym->name = name;
    sym->type_ptr = type_ptr;
    sym->scope = scope;
    scope->sym_table.insert(std::make_pair(name, sym));
    std::cout << "inserted " << name
        << " of type " << *type_ptr->str
        << " into symtable\n";
    return sym;
}

Type const *SemanticAnalyzer::find_type_in_current_scope(
    std::string const *ident,
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

Type const *SemanticAnalyzer::find_type_in_any_active_scope(
    std::string const *ident,
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

SemanticAnalyzer::SemanticAnalyzer(
    Allocator<std::string> &str_allocator,
    std::vector<token::token_type> const &primitives
) :
    str_allocator(str_allocator),
    primitive_keywords(primitives) {

    std::string *str;

    // create error type
    error_type = type_allocator.alloc();
    error_type->kind = type::error_type;
    error_type->canonical = error_type;
    str = str_allocator.alloc();
    str->assign("<error-type>");
    error_type->str = str;
    error_type->contains_error = true;

    // create error node
    error_node = node_allocator.alloc();
    error_node->kind = ast::error;
    error_node->str = str_allocator.alloc();
    *(std::string *)error_node->str = std::string("<error-node>");
    error_node->type = error_type;

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
    //     type_ptr->canonical = type_ptr;
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

Type const *get_most_recent_non_alias(Type const *ty) {
    while (ty->kind == type::alias_type) {
        ty = ty->alias_of;
    }
    return ty;
}

Type *SemanticAnalyzer::analyze_function_type(
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
        rep += *(*i)->str;
        can_rep += *(*i)->canonical->str;
        if ((*i)->canonical != (*i)) {
            iscanon = false;
        }
    }
    rep += ")" + *return_type->str;
    can_rep += ")" + *return_type->canonical->str;
    if (return_type != return_type->canonical) {
        iscanon = false;
    }

    // find type
    Type *type = (Type *)find_type_in_any_active_scope(&rep, *scope);

    // already exists
    if (type) {
        return type;
    }

    // does not exist yet

    // attempt to find canon type
    Type *canon = (Type *)find_type_in_current_scope(&can_rep, gscope);

    // canon already exists
    if (canon) {

        // then the type itself is not canon

        // create str rep for type
        std::string *str = str_allocator.alloc();
        *str = rep;

        // create type
        type = type_allocator.alloc();
        type->set(
            str,
            type::function_type,
            return_type
        );

        // add params
        for (
            std::vector<Type *>::iterator
                i = param_types.begin(),
                end = param_types.end();
            i != end;
            i++
        ) {
            type->add_param(*i);
        }
        
        // set canon
        type->canonical = canon;

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
        canon = type_allocator.alloc();
        canon->set(
            can_str,
            type::function_type,
            return_type->canonical
        );

        // add params
        for (
            std::vector<Type *>::iterator
                i = param_types.begin(),
                end = param_types.end();
            i != end;
            i++
        ) {
            canon->params.push_back((*i)->canonical);
        }

        // set canon
        canon->canonical = canon;

        // insert into global scope
        insert_type(gscope, *can_str, canon);

        // the type is not canon
        if (!iscanon) {

            // create str rep for type
            std::string *str = str_allocator.alloc();
            *str = rep;

            // create type
            type = type_allocator.alloc();
            type->set(
                str,
                type::function_type,
                return_type
            );

            // add params
            for (
                std::vector<Type *>::iterator
                    i = param_types.begin(),
                    end = param_types.end();
                i != end;
                i++
            ) {
                type->add_param(*i);
            }

            // set canon
            type->canonical = canon;

            // insert into curr scope
            insert_type(*scope, *str, type);

            return type;
        }

        else {
            return canon;
        }
    }
}

Type *SemanticAnalyzer::analyze_typename(
    Scope **scope,
    std::string const *ident,
    SourceLocation loc
) {
    // get type
    Type const *type = find_type_in_any_active_scope(ident, *scope);

    // type does not exist
    if (type == nullptr) {
        ErrorHandler::handle(error::ident_is_not_a_typename, loc, ident->c_str());
        return error_type;
    }

    // type exists
    return (Type *)type;
}

Type *SemanticAnalyzer::analyze_pointer_type(
    Scope **scope,
    Type *pointee,
    SourceLocation pointer_modifier_loc
) {

    // make string representation
    std::string rep = "*" + *pointee->str;

    // find type
    Type *type = (Type *)find_type_in_any_active_scope(&rep, *scope);

    // already exists
    if (type) {
        return type;
    }

    // does not exist yet

    // make sure is not func type
    if (pointee->kind == type::function_type) {
        ErrorHandler::handle(
            error::pointer_to_function_type,
            pointer_modifier_loc,
            "cannot make pointers to function types"
        );
        return error_type;
    }

    // create str rep
    std::string *str = str_allocator.alloc();
    *str = rep;

    // requires a separate canon
    if (pointee->canonical != pointee) {

        // make string rep for canon
        std::string *can_str = str_allocator.alloc();
        *can_str = "*" + *pointee->canonical->str;

        // attempt to find canon (in global scope)
        Type *canon = (Type *)find_type_in_any_active_scope(can_str, gscope);

        // not found
        if (canon == nullptr) {
            
            // make new canon type
            canon = type_allocator.alloc();
            canon->set(
                can_str,
                type::pointer_type,
                pointee->canonical
            );
            canon->canonical = canon;

            // insert into global scope
            insert_type(gscope, *can_str, canon);
        }

        // make type
        type = type_allocator.alloc();
        type->set(
            str,
            type::pointer_type,
            pointee
        );
        type->canonical = canon;

        // insert into local scope (since it is a non-canonical type)
        insert_type(*scope, *str, type);
    }

    // is canon itself
    else {

        // then the new type will also be canon
        type = type_allocator.alloc();
        type->set(
            str,
            type::pointer_type,
            pointee
        );
        type->canonical = type;

        // insert into global scope (since it is a modified builtin)
        insert_type(gscope, *str, type);
    }

    // return result
    return type;
}

Type *SemanticAnalyzer::analyze_array_type(
    Scope **scope,
    Type *array_of,
    SourceLocation array_modifier_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, array_modifier_loc, "array types");
    ErrorHandler::prog_exit();
}

ASTNode *SemanticAnalyzer::analyze_cast_expr(
    ASTNode *casted_expr,
    Type const *cast_type,
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

Type *SemanticAnalyzer::analyze_primitive_type(
    token::token_type prim,
    SourceLocation loc
) {
    if (!token::is_primitive_type(prim)) {
        std::cout << "analyze_primitive_type() called with non-primitive type\n";
        ErrorHandler::prog_exit();
    }

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

    return Type::get_prim(prim);
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

    Type const *op_type;

    // propagate potential error state
    bool err = lhs->has_error || rhs->has_error;

    if (op::is_val_op(op)) {

        // type compare
        if (lhs->type->canonical != rhs->type->canonical) {
            op_type = error_type;
            err = true;
            ErrorHandler::handle(
                error::incompatible_operand_type,
                op_loc,
                "incompat val op types"
            );
        }
        else {
            op_type = lhs->type;
        }
    }

    else if (op::is_rel_op(op)) {

        // type compare
        if (lhs->type->canonical != rhs->type->canonical) {
            op_type = error_type;
            err = true;
            ErrorHandler::handle(
                error::incompatible_operand_type,
                op_loc,
                "relational op incompat operand type"
            );
        }
        else {
            op_type = Type::get_i8_type();
        }
    }

    else if (op::is_log_op(op)) {

        ErrorHandler::handle(error::nyi, op_loc, "logical bin ops");

        op_type = error_type;
        err = true;
    }

    else if (op == op::group) {
        
        // type is last expr's type
        op_type = rhs->type;
    }

    else if (op::is_lval_op(op)) {
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
        op_loc,
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

    switch (op) {
        case op::postincr:
        case op::postdecr:
            // the expr must be of lvalue type
            if (!expr->is_lvalue) {
                ErrorHandler::handle(error::lvalue_required, loc, "postfix in/decrement cannot be applied to non-lvalue values");
                err = true;
            }
            // the lvalue must be of integral type
            if (!expr->type->is_integral) {
                ErrorHandler::handle(error::integral_type_required, loc, "postfix in/decrement cannot be applied to non-integral types");
                err = true;
            }
            break;
        default:
            assert(false && "should be unreachable");
    }

    loc.copy_start(expr->loc);

    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::unary_op,
        expr->type,
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

    Type const *callable = get_most_recent_non_alias(expr->type);
    bool error = expr->has_error;
    ASTNode *node = node_allocator.alloc();
    node->children.push_back(expr);
    SourceLocation call_loc;
    call_loc.copy_src(call_start_loc);
    call_loc.copy_end(call_end_loc);

    // verify func type
    if (callable->kind != type::function_type) {
        ErrorHandler::handle(
            error::type_is_not_callable,
            expr->loc,
            "does not have type that is callable"
        );
        node->set(
            ast::call_expr,
            error_type,
            nullptr,
            call_loc,
            token::op_leftparen,
            true
        );

        return node;
    }

    int num_args = callable->params.size();

    // verify args
    if (args.size() != num_args) {
        error = true;

        ErrorHandler::handle(
            error::mismatch_between_func_type_and_param_list,
            call_end_loc,
            "wrong number of args"
        );

        node->set(
            ast::call_expr,
            error_type,
            nullptr,
            call_start_loc,
            token::op_leftparen,
            true
        );

        for (int i = 0; i < args.size(); i++) {
            node->children.push_back(args[i]);
        }
        return node;
    }

    // verify arg types
    ASTNode const *arg;
    std::vector<Type const *> const paramtypes = callable->params;
    for (int i = 0; i < num_args; i++) {
        arg = args[i];
        if (arg->type == error_type) {
            error = true;
        }
        else if (arg->type->canonical != paramtypes[i]->canonical) {
            error = true;
            ErrorHandler::handle(
                error::argument_type_mismatch,
                arg->loc,
                "argument has wrong type"
            );
        }
        node->children.push_back(args[i]);
    }

    node->set(
        ast::call_expr,
        callable->returns,
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
    ErrorHandler::handle(error::nyi, subscript_start_loc, "array subscripts");
    ErrorHandler::prog_exit();
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
    std::string const *ident,
    SourceLocation loc
) {
    // ErrorHandler::handle(error::nyi, loc, "reference expressions");
    // ErrorHandler::prog_exit();

    // TODO: allow for scope stack climbing to find shadowed symbols
    //       if current scope's symbol is invalid

    // find symbol
    Symbol const *res = find_symbol_in_any_active_scope(ident, *scope);

    Type const *sym_type;
    bool err = false;

    // no symbol
    if (res == nullptr) {
        sym_type = error_type;
        err = true;
        ErrorHandler::handle(
            error::referenced_ident_is_undefined,
            loc,
            "identifier is undefined"
        );
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
    std::string const *char_lit,
    SourceLocation loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, loc, "character literals");
    ErrorHandler::prog_exit();
}

ASTNode *SemanticAnalyzer::analyze_numeric_literal(
    std::string const *num_lit,
    SourceLocation loc
) {
    // ErrorHandler::handle(error::nyi, loc, "numeric literals");
    // ErrorHandler::prog_exit();

    // assume no overflow and default size i32 for now
    // TODO: size check and dynamic type assignment

    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::int_lit,
        analyze_primitive_type(token::kw_i32, loc), // SPAGHETTI CODE AT ITS FINEST
        num_lit,
        loc,
        token::numeric_literal,
        false
    );

    return node;
}

ASTNode *SemanticAnalyzer::analyze_string_literal(
    std::string const *str_lit,
    SourceLocation loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, loc, "string literals");
    ErrorHandler::prog_exit();
}

ASTNode *SemanticAnalyzer::analyze_prefix_op_expr(
    op::kind op,
    token::token_type tk,
    ASTNode *expr,
    SourceLocation loc
) {

    bool err = expr->has_error;
    Type const *ty = expr->type;
    bool lval = false;

    switch (op) {
        case op::preincr:
        case op::predecr:
            // the expr must be lvalue
            if (!expr->is_lvalue) {
                ErrorHandler::handle(error::lvalue_required, loc, "prefix in/decrement cannot be applied to non-lvalue values");
                err = true;
            }
            // the lvalue must be of integral type
            if (!expr->type->is_integral) {
                ErrorHandler::handle(error::integral_type_required, loc, "prefix in/decrement cannot be applied to non-integral types");
                err = true;
            }
            // resulting value is an lval
            lval = true;
            break;
        case op::addr:
            // the expr must be lvalue
            if (!expr->is_lvalue) {
                ErrorHandler::handle(error::lvalue_required, loc, "a non-lvalue value does not have an address");
                err = true;
            }
            // resulting value is pointer type
            // TODO: how do I get uniqued pointer type to expr type from here?
            ErrorHandler::handle(error::nyi, loc, "address-of operator");
            ErrorHandler::prog_exit();
            break;
        case op::deref:
            // the expr must be of pointer type
            if (expr->type->kind != type::pointer_type) {
                ErrorHandler::handle(error::pointer_type_required, loc, "value of non-pointer type cannot be dereferenced");
                err = true;
            }
            ty = expr->type->points_to;
            lval = true;
            break;
        case op::lnot:
            // expr must be of integral type
            if (!expr->type->is_integral) {
                ErrorHandler::handle(error::integral_type_required, loc, "logical not cannot be applied to non-integral types");
                err = true;
            }
            // resulting value is bool
            ty = Type::get_i8_type();
            break;
        case op::neg:
            // expr must be of integral type
            if (!expr->type->is_integral) {
                ErrorHandler::handle(error::integral_type_required, loc, "negation cannot be applied to non-integral types");
                err = true;
            }
            ty = expr->type;
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
    node->is_lvalue = lval;
    node->children.push_back(expr);

    return node;
}

ASTNode *SemanticAnalyzer::analyze_func_decl(
    Scope **scope,
    Type *type,
    std::string const *ident,
    std::vector<std::pair<std::string const *, SourceLocation>> params,
    SourceLocation ident_loc,
    SourceLocation param_list_loc
) {
    // ErrorHandler::handle(error::nyi, ident_loc, "function declarations");
    // ErrorHandler::prog_exit();

    // param num mismatch
    if (type->params.size() != params.size()) {
        ErrorHandler::handle(
            error::mismatch_between_func_type_and_param_list,
            param_list_loc,
            "wrong number of parameters"
        );
    }

    // redeclaration in current scope
    if (find_symbol_in_current_scope(ident, *scope)) {
        ErrorHandler::handle(
            error::redeclaration_of_symbol_in_same_scope,
            ident_loc,
            "redeclared symbol"
        );
    }

    // add to symbol table
    Symbol *sym = insert_symbol(*scope, *ident, type);

    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::func_decl,
        type,
        ident,
        ident_loc,
        token::unknown,
        type->contains_error
    );
    node->sym = sym;

    // enter new scope
    enter_new_scope(scope);

    // add params
    ASTNode *param;
    for (int i = 0; i < params.size(); i++) {

        // add to symbol table
        sym = insert_symbol(
            *scope,
            *params[i].first,
            type->params[i]
        );

        // make node
        param = node_allocator.alloc();
        param->set(
            ast::param_decl,
            type->params[i],
            params[i].first,
            params[i].second,
            token::identifier,
            type->params[i]->contains_error
        );
        param->sym = sym;

        // add to decl node
        node->children.push_back(param);
    }

    return (node);
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
    std::string const *ident,
    SourceLocation ident_loc
) {
    ASTNode *decl = node_allocator.alloc();
    decl->set(
        ast::var_decl,
        type,
        ident,
        ident_loc,
        token::identifier,
        type->contains_error
    );

    // ensure no redeclaration
    if (find_symbol_in_current_scope(ident, *scope)) {
        ErrorHandler::handle(
            error::redeclaration_of_symbol_in_same_scope,
            ident_loc,
            ident->c_str()
        );
        decl->has_error = true;
    }

    Symbol *sym = insert_symbol(*scope, *ident, type);
    decl->sym = sym;

    ASTNode *res = decl;
    return res;
}

ASTNode *SemanticAnalyzer::analyze_var_decl(
    Scope **scope,
    Type *type,
    std::string const *ident,
    SourceLocation ident_loc,
    ASTNode *rhs,
    SourceLocation eqloc
) {
    ASTNode *decl_node = analyze_var_decl(scope, type, ident, ident_loc);
    decl_node->children.push_back(rhs);
    return decl_node;
}

ASTNode *SemanticAnalyzer::analyze_type_alias(
    Scope **scope,
    Type *type,
    std::string const *ident,
    SourceLocation ident_loc
) {
    bool err = false;
    Type *alias;

    // ensure no redeclaration
    if (find_type_in_current_scope(ident, *scope)) {
        ErrorHandler::handle(
            error::redeclaration_of_symbol_in_same_scope,
            ident_loc,
            ident->c_str()
        );
        err = true;
        alias = error_type;
    }
    else {

        // add alias to type table
        alias = type_allocator.alloc();
        alias->alias_of = type;
        alias->str = ident;
        alias->kind = type::alias_type;
        alias->canonical = type->canonical;

        insert_type(*scope, *ident, alias);
    }

    ASTNode *stmt = node_allocator.alloc();
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
    ASTNode *expr
) {
    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::ret_stmt,
        expr->type,
        nullptr,
        expr->loc,
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
