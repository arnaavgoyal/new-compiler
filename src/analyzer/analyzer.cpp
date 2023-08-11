#include "analyzer/analyzer.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include "memory/allocator.h"
#include "error/error.h"
#include "analyzer/scope.h"

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
    Scope **scope
) {
    std::map<std::string, Symbol *>::iterator
        iter = (*scope)->sym_table.find(*ident),
        end = (*scope)->sym_table.end();

    if (iter == end) {
        return nullptr;
    }

    return iter->second;
}

Symbol const *SemanticAnalyzer::find_symbol_in_any_active_scope(
    std::string const *ident,
    Scope **scope
) {

    Scope *curr = *scope;
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

void SemanticAnalyzer::insert_symbol(
    Scope **scope,
    std::string const &name,
    Type const *type_ptr
) {
    Symbol *sym = sym_allocator.alloc();
    sym->name = name;
    sym->type_ptr = type_ptr;
    (*scope)->sym_table.insert(std::make_pair(name, sym));
    std::cout << "inserted " << name
        << " of type " << *type_ptr->str
        << " into symtable\n";
}

Type const *SemanticAnalyzer::find_type_in_current_scope(
    std::string const *ident,
    Scope **scope
) {
    std::map<std::string, Type *>::iterator
        iter = (*scope)->type_table.find(*ident),
        end = (*scope)->type_table.end();

    if (iter == end) {
        return nullptr;
    }

    return iter->second;
}

Type const *SemanticAnalyzer::find_type_in_any_active_scope(
    std::string const *ident,
    Scope **scope
) {
    Scope *curr = *scope;
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
    Scope **scope,
    std::string const &key,
    Type *value
) {
    (*scope)->type_table.insert(std::make_pair(key, value));
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
    error_type->type = type::error_type;
    str = str_allocator.alloc();
    *str = std::string("<error-type>");
    error_type->str = str;
    error_type->contains_error = true;

    // create error node
    error_node = node_allocator.alloc();
    error_node->kind = ast::error;
    error_node->str = str_allocator.alloc();
    *(std::string *)error_node->str = std::string("<error-node>");
    error_node->type = error_type;

    // add all primitive types
    Type *type_ptr;
    for (
        std::vector<token::token_type>::const_iterator
            i = primitives.begin(),
            end = primitives.end();
        i != end;
        i++
    ) {
        type_ptr = type_allocator.alloc();
        type_ptr->type = type::primitive_type;
        str = str_allocator.alloc();
        str->assign(token::get_keyword_string(*i));
        type_ptr->str = str;
        type_ptr->contains_error = false;
        primitive_types.push_back(type_ptr);
    }

    // create global scope
    gscope = scope_allocator.alloc();
}

AnalyzedType SemanticAnalyzer::analyze_function_type(
    Scope **scope,
    std::vector<AnalyzedType> param_types,
    AnalyzedType return_type,
    SourceLocation start_loc,
    SourceLocation end_loc
) {

    // make string representation
    std::string rep = "(";
    std::vector<AnalyzedType>::iterator
        i = param_types.begin(),
        end = param_types.end();
    bool start = true;
    for ( ; i != end; i++) {
        if (!start) {
            rep += ",";
        }
        else {
            start = false;
        }
        rep += *i->contents->str;
    }
    rep += ")" + *return_type.contents->str;

    std::cout << "  rep: " << rep << std::endl;

    // find type
    Type *type = (Type *)find_type_in_any_active_scope(&rep, scope);

    // already exists
    if (type) {
        return AnalyzedType(type);
    }

    // does not exist yet

    // create str rep
    std::string *str = str_allocator.alloc();
    *str = rep;

    // create type
    type = type_allocator.alloc();
    type->type = type::function_type;
    for (
        std::vector<AnalyzedType>::iterator
            i = param_types.begin(),
            end = param_types.end();
        i != end;
        i++
    ) {
        std::cout << "pb\n";
        type->add_param(i->contents);
    }
    type->set_inner_type(return_type.contents);
    type->str = str;
    insert_type(scope, *str, type);

    AnalyzedType result = AnalyzedType(type);

    std::cout << "finished func type analyze\n";

    // return result
    return result;
}

AnalyzedType SemanticAnalyzer::analyze_typename(
    Scope **scope,
    std::string const *ident,
    SourceLocation loc
) {
    // get type
    Type const *type = find_type_in_any_active_scope(ident, scope);

    // type does not exist
    if (type == nullptr) {
        ErrorHandler::handle(error::ident_is_not_a_typename, loc, ident->c_str());
        return AnalyzedType(error_type, /** has errors = */ true);
    }

    // type exists
    return AnalyzedType(type);
}

AnalyzedType SemanticAnalyzer::analyze_pointer_type(
    Scope **scope,
    AnalyzedType pointee,
    SourceLocation pointer_modifier_loc
) {

    // make string representation
    std::string rep = "*" + *pointee.contents->str;

    // find type
    Type *type = (Type *)find_type_in_any_active_scope(&rep, scope);

    // already exists
    if (type) {
        return AnalyzedType(type);
    }

    // does not exist yet

    // create str rep
    std::string *str = str_allocator.alloc();
    *str = rep;

    // create type
    type = type_allocator.alloc();
    type->set(
        str,
        pointee.contents->type == type::function_type
            ? type::pointer_to_function_type
            : type::pointer_type,
        pointee.contents
    );
    insert_type(scope, *str, type);

    // return result
    return AnalyzedType(type);
}

AnalyzedType SemanticAnalyzer::analyze_array_type(
    Scope **scope,
    AnalyzedType array_of,
    SourceLocation array_modifier_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, array_modifier_loc, "array types");
    ErrorHandler::prog_exit();
}

AnalyzedType SemanticAnalyzer::analyze_primitive_type(
    token::token_type prim,
    SourceLocation loc
) {
    if (!token::is_primitive_type(prim)) {
        std::cout << "analyze_primitive_type() called with non-primitive type\n";
        ErrorHandler::prog_exit();
    }

    // this must be valid because the set of primitives is given
    unsigned int idx = std::distance(
        primitive_keywords.begin(),
        std::find(
            primitive_keywords.begin(),
            primitive_keywords.end(),
            prim
        )
    );

    return AnalyzedType(primitive_types[idx]);
}

AnalyzedExpr SemanticAnalyzer::analyze_binary_op_expr(
    token::token_type op,
    AnalyzedExpr lhs,
    AnalyzedExpr rhs,
    SourceLocation op_loc
) {
    // ErrorHandler::handle(error::nyi, op_loc, "binary operations");
    // ErrorHandler::prog_exit();

    // lhs.contents->print();
    // rhs.contents->print();

    Type const *op_type;

    bool err = lhs.contents->has_error || rhs.contents->has_error;

    if (lhs.contents->type != rhs.contents->type) {
        op_type = error_type;
        err = true;
    }
    else {
        op_type = lhs.contents->type;
    }


    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::binary_op,
        op_type,
        nullptr,
        op_loc,
        op,
        err
    );
    node->children.push_back(lhs.contents);
    node->children.push_back(rhs.contents);

    return AnalyzedExpr(node);
}

AnalyzedExpr SemanticAnalyzer::analyze_postfix_op_expr(
    token::token_type op,
    AnalyzedExpr expr,
    SourceLocation op_loc
) {
    // ErrorHandler::handle(error::nyi, op_loc, "postfix operations");
    // ErrorHandler::prog_exit();

    op_loc.copy_start(expr.contents->loc);

    // TODO: typecheck for these ops
    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::unary_op,
        expr.contents->type,
        nullptr,
        op_loc,
        op,
        expr.contents->has_error
    );
    node->children.push_back(expr.contents);
    node->print();

    return AnalyzedExpr(node);
}

AnalyzedExpr SemanticAnalyzer::analyze_call_expr(
    AnalyzedExpr expr,
    std::vector<AnalyzerResult> args,
    SourceLocation call_start_loc,
    SourceLocation call_end_loc
) {
    // ErrorHandler::handle(error::nyi, call_start_loc, "call expressions");
    // ErrorHandler::prog_exit();

    expr.contents->print();

    Type const *callable = expr.contents->type;
    bool error = expr.contents->has_error;
    ASTNode *node = node_allocator.alloc();
    node->children.push_back(expr.contents);

    // verify func type
    if (callable->type == type::pointer_to_function_type) {
        callable = callable->points_to;
    }
    else if (callable->type != type::function_type) {
        ErrorHandler::handle(
            error::type_is_not_callable,
            expr.contents->loc,
            "does not have type that is callable"
        );
        node->set(
            ast::call_expr,
            error_type,
            nullptr,
            call_start_loc,
            token::op_leftparen,
            true
        );

        return AnalyzedExpr(node);
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
            node->children.push_back(args[i].contents);
        }
        return AnalyzedExpr(node);
    }

    // verify arg types
    ASTNode const *arg;
    std::vector<Type const *> const paramtypes = expr.contents->type->params;
    for (int i = 0; i < num_args; i++) {
        arg = args[i].contents;
        if (arg->type == error_type) {
            error = true;
        }
        else if (arg->type != paramtypes[i]) {
            error = true;

            ErrorHandler::handle(
                error::argument_type_mismatch,
                arg->loc,
                "argument has wrong type"
            );
        }
        node->children.push_back(args[i].contents);
    }

    node->set(
        ast::call_expr,
        expr.contents->type->returns,
        nullptr,
        call_start_loc,
        token::op_leftparen,
        error
    );

    node->print();

    return AnalyzedExpr(node);
}

AnalyzerResult SemanticAnalyzer::analyze_subscript_expr(
    AnalyzerResult expr,
    AnalyzerResult subscript,
    SourceLocation subscript_start_loc,
    SourceLocation subscript_end_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, subscript_start_loc, "array subscripts");
    ErrorHandler::prog_exit();
}

AnalyzedExpr SemanticAnalyzer::analyze_paren_expr(
    AnalyzedExpr inside,
    SourceLocation loc
) {
    ASTNode *pexpr = node_allocator.alloc();
    pexpr->set(
        ast::paren_expr,
        inside.contents->type,
        nullptr,
        loc,
        token::op_leftparen,
        inside.contents->has_error
    );
    pexpr->children.push_back(inside.contents);
    pexpr->print();

    return AnalyzedExpr(pexpr);
}

AnalyzedExpr SemanticAnalyzer::analyze_ref_expr(
    Scope **scope,
    std::string const *ident,
    SourceLocation loc
) {
    // ErrorHandler::handle(error::nyi, loc, "reference expressions");
    // ErrorHandler::prog_exit();

    // TODO: allow for scope stack climbing to find shadowed symbols
    //       if current scope's symbol is invalid

    // find symbol
    Symbol const *res = find_symbol_in_any_active_scope(ident, scope);

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

    return AnalyzedExpr(expr);
}

AnalyzerResult SemanticAnalyzer::analyze_character_literal(
    std::string const *char_lit,
    SourceLocation loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, loc, "character literals");
    ErrorHandler::prog_exit();
}

AnalyzedExpr SemanticAnalyzer::analyze_numeric_literal(
    std::string const *num_lit,
    SourceLocation loc
) {
    // ErrorHandler::handle(error::nyi, loc, "numeric literals");
    // ErrorHandler::prog_exit();

    // assume no overflow and default size i32 for now
    // TODO: size check and dynamic type assignment
    std::cout << *num_lit << std::endl;
    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::int_lit,
        analyze_primitive_type(token::kw_i32, loc).contents, // SPAGHETTI CODE AT ITS FINEST
        num_lit,
        loc,
        token::numeric_literal,
        false
    );

    return AnalyzedExpr(node);
}

AnalyzerResult SemanticAnalyzer::analyze_string_literal(
    std::string const *str_lit,
    SourceLocation loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, loc, "string literals");
    ErrorHandler::prog_exit();
}

AnalyzerResult SemanticAnalyzer::analyze_prefix_op_expr(
    token::token_type op,
    AnalyzerResult expr,
    SourceLocation op_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, op_loc, "prefix operations");
    ErrorHandler::prog_exit();
}

AnalyzedStmt SemanticAnalyzer::analyze_func_decl(
    Scope **scope,
    AnalyzedType type,
    std::string const *ident,
    std::vector<std::pair<std::string const *, SourceLocation>> params,
    SourceLocation ident_loc,
    SourceLocation param_list_loc
) {
    // ErrorHandler::handle(error::nyi, ident_loc, "function declarations");
    // ErrorHandler::prog_exit();

    // param num mismatch
    if (type.contents->params.size() != params.size()) {
        ErrorHandler::handle(
            error::mismatch_between_func_type_and_param_list,
            param_list_loc,
            "wrong number of parameters"
        );
    }

    // redeclaration in current scope
    if (find_symbol_in_current_scope(ident, scope)) {
        ErrorHandler::handle(
            error::redeclaration_of_symbol_in_same_scope,
            ident_loc,
            "redeclared symbol"
        );
    }

    // add to symbol table
    insert_symbol(scope, *ident, type.contents);

    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::func_decl,
        type.contents,
        ident,
        ident_loc,
        token::unknown,
        type.contents->contains_error
    );

    // enter new scope
    enter_new_scope(scope);

    // add params
    ASTNode *param;
    for (int i = 0; i < params.size(); i++) {

        // add to symbol table
        insert_symbol(
            scope,
            *params[i].first,
            type.contents->params[i]
        );

        // make node
        param = node_allocator.alloc();
        param->set(
            ast::param_decl,
            type.contents->params[i],
            params[i].first,
            params[i].second,
            token::identifier,
            type.contents->params[i]->contains_error
        );

        // add to decl node
        node->children.push_back(param);
    }

    node->print();

    return AnalyzedStmt(node);
}

void SemanticAnalyzer::start_func_define(
    AnalyzedStmt decl,
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

AnalyzedStmt SemanticAnalyzer::analyze_var_decl(
    Scope **scope,
    AnalyzedType type,
    std::string const *ident,
    SourceLocation ident_loc
) {
    ASTNode *decl = node_allocator.alloc();
    decl->set(
        ast::var_decl,
        type.contents,
        ident,
        ident_loc,
        token::identifier,
        type.contents->contains_error
    );

    // ensure no redeclaration
    if (find_symbol_in_current_scope(ident, scope)) {
        ErrorHandler::handle(
            error::redeclaration_of_symbol_in_same_scope,
            ident_loc,
            ident->c_str()
        );
        decl->has_error = true;
    }

    insert_symbol(scope, *ident, type.contents);

    AnalyzedStmt res(decl, decl->has_error);
    return res;
}

AnalyzedStmt SemanticAnalyzer::analyze_var_decl(
    Scope **scope,
    AnalyzedType type,
    std::string const *ident,
    SourceLocation ident_loc,
    AnalyzerResult rhs,
    SourceLocation eqloc
) {
    ErrorHandler::handle(error::nyi, ident_loc, "var decl w/ define");
    ErrorHandler::prog_exit();
}

AnalyzedStmt SemanticAnalyzer::analyze_type_alias(
    Scope **scope,
    AnalyzedType type,
    std::string const *ident,
    SourceLocation ident_loc
) {
    bool err = false;
    Type *alias;

    // ensure no redeclaration
    if (find_type_in_current_scope(ident, scope)) {
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
        alias->set_inner_type(type.contents);
        alias->str = ident;
        alias->type = type::alias_type;
        std::cout << "before!\n";
        insert_type(scope, *ident, alias);
        std::cout << "after!\n";
    }

    ASTNode *stmt = node_allocator.alloc();
    stmt->set(
        ast::typedef_stmt,
        type.contents,
        ident,
        ident_loc,
        token::identifier,
        err
    );

    return AnalyzedStmt(stmt, stmt->has_error);
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

AnalyzedStmt SemanticAnalyzer::analyze_return_stmt(
    AnalyzedExpr expr
) {
    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::ret_stmt,
        expr.contents->type,
        nullptr,
        expr.contents->loc,
        token::kw_return,
        expr.contents->has_error
    );
    node->children.push_back(expr.contents);

    return AnalyzedStmt(node, node->has_error);
}

void SemanticAnalyzer::add_expr_as_stmt(
    AnalyzerResult expr
) {
    // TODO: implement
}
