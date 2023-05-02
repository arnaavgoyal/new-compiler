#include "analyzer/analyzer.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include "memory/allocator.h"
#include "error/error.h"
#include "analyzer/hashtable.h"

SemanticAnalyzer::scope_status SemanticAnalyzer::status(scope_id_t id) {
    return scope_history[id];
}

void SemanticAnalyzer::enter_new_scope() {
    curr_scope_id = scope_id_gen;
    scope_id_gen++;
    scope_history.push_back(open);
    scope_stack.push(curr_scope_id);
}
void SemanticAnalyzer::exit_current_scope() {
    scope_history[curr_scope_id] = closed;
    scope_stack.pop();
    curr_scope_id = scope_stack.top();
}

Symbol const *SemanticAnalyzer::find_symbol_in_current_scope(std::string const *ident) {
    
    std::vector<std::pair<scope_id_t, Symbol const *>> const &
        result = symbol_table.lookup(*ident);
    std::vector<std::pair<scope_id_t, Symbol const *>>::const_reverse_iterator
        start = result.rbegin();
    std::vector<std::pair<scope_id_t, Symbol const *>>::const_reverse_iterator
        end = result.rend();

    if (start == end) {
        return nullptr;
    }

    if (start->first != curr_scope_id) {
        return nullptr;
    }

    return start->second;
}

Symbol const *SemanticAnalyzer::find_symbol_in_any_active_scope(std::string const *ident) {

    std::vector<std::pair<scope_id_t, Symbol const *>>
        result = symbol_table.lookup(*ident);
    std::vector<std::pair<scope_id_t, Symbol const *>>::reverse_iterator
        start = result.rbegin();
    std::vector<std::pair<scope_id_t, Symbol const *>>::reverse_iterator
        end = result.rend();

    while (start != end) {
        if (status(start->first) == open) {
            return start->second;
        }
        else {
            start++;
        }
    }

    // not found
    return nullptr;  
}

void SemanticAnalyzer::insert_symbol(
    std::string const *key,
    std::string const &name,
    Type const *type_ptr
) {
    Symbol *sym = sym_allocator.alloc();
    sym->name = name;
    sym->type_ptr = type_ptr;
    std::vector<std::pair<scope_id_t, Symbol const *>>
        result = symbol_table.lookup(*key);
    result.push_back(std::make_pair(curr_scope_id, sym));
}

Type const *SemanticAnalyzer::find_type_in_current_scope(std::string const *ident) {

    std::vector<std::pair<scope_id_t, Type const *>>
        result = type_table.lookup(*ident);
    std::vector<std::pair<scope_id_t, Type const *>>::reverse_iterator
        start = result.rbegin();
    std::vector<std::pair<scope_id_t, Type const *>>::reverse_iterator
        end = result.rend();

    if (start == end) {
        return nullptr;
    }

    if (start->first != curr_scope_id) {
        return nullptr;
    }

    return start->second;
}

Type const *SemanticAnalyzer::find_type_in_any_active_scope(std::string const *ident) {

    std::vector<std::pair<scope_id_t, Type const *>>
        result = type_table.lookup(*ident);
    std::vector<std::pair<scope_id_t, Type const *>>::reverse_iterator
        start = result.rbegin();
    std::vector<std::pair<scope_id_t, Type const *>>::reverse_iterator
        end = result.rend();

    while (start != end) {
        if (status(start->first) == open) {
            return start->second;
        }
        else {
            start++;
        }
    }

    // not found
    return nullptr; 
}

void SemanticAnalyzer::insert_type(std::string const *key, Type const *value) {

    std::vector<std::pair<scope_id_t, Type const *>>
        result = type_table.lookup(*key);
    result.push_back(std::make_pair(curr_scope_id, value));
}

SemanticAnalyzer::SemanticAnalyzer(
    Allocator<std::string> &str_allocator,
    std::vector<token::token_type> const &primitives
) :
    str_allocator(str_allocator),
    primitive_keywords(primitives) {
    
    // zero all scope id trackers
    scope_id_gen = 0;
    curr_scope_id = 0;

    // create error type
    error_type = type_allocator.alloc();
    error_type->type = type::error_type;
    error_type->str = str_allocator.alloc();
    *error_type->str = std::string("<error-type>");

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
        type_ptr->str = str_allocator.alloc();
        type_ptr->str->assign(token::get_keyword_string(*i));
        primitive_types.push_back(type_ptr);
    }

    // enter global scope
    enter_new_scope();
}

AnalyzedType SemanticAnalyzer::analyze_function_type(
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
    Type *type = (Type *)find_type_in_any_active_scope(&rep);

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
        type->params.push_back(i->contents);
    }
    type->returns = return_type.contents;
    type->str = str;
    insert_type(str, type);

    AnalyzedType result = AnalyzedType(type);

    std::cout << "finished func type analyze\n";

    // return result
    return result;
}

AnalyzedType SemanticAnalyzer::analyze_typename(
    std::string const *ident,
    SourceLocation loc
) {
    // get type
    Type const *type = find_type_in_any_active_scope(ident);

    // type does not exist
    if (type == nullptr) {
        ErrorHandler::handle(error::ident_is_not_a_typename, loc, ident->c_str());
        return AnalyzedType(error_type, /** has errors = */ true);
    }

    // type exists
    return AnalyzedType(type);
}

AnalyzedType SemanticAnalyzer::analyze_pointer_type(
    AnalyzedType pointee,
    SourceLocation pointer_modifier_loc
) {

    // make string representation
    std::string rep = "*" + *pointee.contents->str;

    // find type
    Type *type = (Type *)find_type_in_any_active_scope(&rep);

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
    type->type = type::pointer_type;
    type->points_to = pointee.contents;
    type->str = str;
    insert_type(str, type);

    // return result
    return AnalyzedType(type);
}

AnalyzedType SemanticAnalyzer::analyze_array_type(
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

AnalyzerResult SemanticAnalyzer::analyze_postfix_op_expr(
    token::token_type op,
    AnalyzerResult expr,
    SourceLocation op_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, op_loc, "postfix operations");
    ErrorHandler::prog_exit();
}

AnalyzerResult SemanticAnalyzer::analyze_call_expr(
    AnalyzerResult expr,
    std::vector<AnalyzerResult> args,
    SourceLocation call_start_loc,
    SourceLocation call_end_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, call_start_loc, "call expressions");
    ErrorHandler::prog_exit();
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

AnalyzerResult SemanticAnalyzer::analyze_paren_expr(
    AnalyzerResult inside,
    SourceLocation left_paren_loc,
    SourceLocation right_paren_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, left_paren_loc, "parenthesized expressions");
    ErrorHandler::prog_exit();
}

AnalyzedExpr SemanticAnalyzer::analyze_ref_expr(
    std::string const *ident,
    SourceLocation loc
) {
    // ErrorHandler::handle(error::nyi, loc, "reference expressions");
    // ErrorHandler::prog_exit();

    // TODO: allow for scope stack climbing to find shadowed symbols
    //       if current scope's symbol is invalid

    // find symbol
    Symbol const *res = find_symbol_in_any_active_scope(ident);

    Type const *sym_type;
    bool err = false;

    // no symbol
    if (res == nullptr) {
        sym_type = error_type;
        err = true;
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
    AnalyzedType type,
    std::string const *ident,
    std::vector<std::string const *> params,
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
    if (find_symbol_in_current_scope(ident)) {
        ErrorHandler::handle(
            error::redeclaration_of_symbol_in_same_scope,
            ident_loc,
            "redeclared symbol"
        );
    }

    ASTNode *node = node_allocator.alloc();
    node->set(
        ast::func_decl,
        type.contents,
        ident,
        ident_loc,
        token::unknown,
        false
    );
    node->print();

    return AnalyzedStmt(node);
}

void SemanticAnalyzer::start_func_define(
    AnalyzerResult decl,
    SourceLocation define_start_loc
) {
    // TODO: implement
}

void SemanticAnalyzer::end_func_define(
    SourceLocation define_end_loc
) {
    // TODO: implement
}

void SemanticAnalyzer::analyze_var_decl(
    AnalyzedType type,
    std::string const *ident,
    SourceLocation ident_loc
) {
    // TODO: implement
}

void SemanticAnalyzer::analyze_var_decl(
    AnalyzedType type,
    std::string const *ident,
    SourceLocation ident_loc,
    AnalyzerResult rhs,
    SourceLocation eqloc
) {
    // TODO: implement
}

void SemanticAnalyzer::analyze_type_alias(
    AnalyzedType type,
    std::string const *ident,
    SourceLocation ident_loc
) {
    // TODO: implement
}

void SemanticAnalyzer::start_scoped_block(
    SourceLocation block_start_loc
) {
    // TODO: implement
}

void SemanticAnalyzer::end_scoped_block(
    SourceLocation block_end_loc
) {
    // TODO: implement
}

void SemanticAnalyzer::analyze_return_stmt(
    AnalyzerResult expr
) {
    // TODO: implement
}

void SemanticAnalyzer::add_expr_as_stmt(
    AnalyzerResult expr
) {
    // TODO: implement
}
