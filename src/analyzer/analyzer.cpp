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
    *error_type->str = "<error-type>";

    // create error node
    error_node = node_allocator.alloc();
    error_node->type = ast::error;

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

AnalyzerResult SemanticAnalyzer::analyze_function_type(
    std::vector<AnalyzerResult> param_types,
    AnalyzerResult return_type,
    SourceLocation start_loc,
    SourceLocation end_loc
) {

    // make string representation
    std::string rep = "(";
    std::vector<AnalyzerResult>::iterator
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
        rep += *i->type->str;
    }
    rep += ")" + *return_type.type->str;

    std::cout << "  rep: " << rep << std::endl;

    // find type
    Type *type = (Type *)find_type_in_any_active_scope(&rep);

    // already exists
    if (type) {
        return AnalyzerResult(type);
    }

    // does not exist yet

    // create str rep
    std::string *str = str_allocator.alloc();
    *str = rep;

    // create type
    type = type_allocator.alloc();
    type->type = type::function_type;
    for (
        std::vector<AnalyzerResult>::iterator
            i = param_types.begin(),
            end = param_types.end();
        i != end;
        i++
    ) {
        type->params.push_back(i->type);
    }
    type->returns = return_type.type;
    type->str = str;
    insert_type(str, type);

    AnalyzerResult result = AnalyzerResult(type);

    std::cout << "finished func type analyze\n";

    // return result
    return result;
}

AnalyzerResult SemanticAnalyzer::analyze_typename(
    std::string const *ident,
    SourceLocation loc
) {
    // get type
    Type const *type = find_type_in_any_active_scope(ident);

    // type does not exist
    if (type == nullptr) {
        ErrorHandler::handle(error::ident_is_not_a_typename, loc, ident->c_str());
        return AnalyzerResult(error_type, /** has errors = */ true);
    }

    // type exists
    return AnalyzerResult(type);
}

AnalyzerResult SemanticAnalyzer::analyze_pointer_type(
    AnalyzerResult pointee,
    SourceLocation pointer_modifier_loc
) {

    // make string representation
    std::string rep = "*" + *pointee.type->str;

    // find type
    Type *type = (Type *)find_type_in_any_active_scope(&rep);

    // already exists
    if (type) {
        return AnalyzerResult(type);
    }

    // does not exist yet

    // create str rep
    std::string *str = str_allocator.alloc();
    *str = rep;

    // create type
    type = type_allocator.alloc();
    type->type = type::pointer_type;
    type->points_to = pointee.type;
    type->str = str;
    insert_type(str, type);

    // return result
    return AnalyzerResult(type);
}

AnalyzerResult SemanticAnalyzer::analyze_array_type(
    AnalyzerResult array_of,
    SourceLocation array_modifier_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, array_modifier_loc, "array types");
    ErrorHandler::prog_exit();
}

AnalyzerResult SemanticAnalyzer::analyze_primitive_type(
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

    return AnalyzerResult(primitive_types[idx]);
}

AnalyzerResult SemanticAnalyzer::analyze_binary_op_expr(
    token::token_type op,
    AnalyzerResult lhs,
    AnalyzerResult rhs,
    SourceLocation op_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, op_loc, "binary operations");
    ErrorHandler::prog_exit();
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

AnalyzerResult SemanticAnalyzer::analyze_ref_expr(
    std::string const *ident,
    SourceLocation loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, loc, "reference expressions");
    ErrorHandler::prog_exit();
}

AnalyzerResult SemanticAnalyzer::analyze_character_literal(
    std::string const *char_lit,
    SourceLocation loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, loc, "character literals");
    ErrorHandler::prog_exit();
}

AnalyzerResult SemanticAnalyzer::analyze_numeric_literal(
    std::string const *num_lit,
    SourceLocation loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, loc, "numeric literals");
    ErrorHandler::prog_exit();
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

AnalyzerResult SemanticAnalyzer::analyze_func_decl(
    AnalyzerResult type,
    std::string const *ident,
    std::vector<std::string const *> params,
    SourceLocation ident_loc,
    SourceLocation lparen_loc,
    SourceLocation rparen_loc
) {
    // TODO: implement
    ErrorHandler::handle(error::nyi, ident_loc, "function declarations");
    ErrorHandler::prog_exit();
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
    AnalyzerResult type,
    std::string const *ident,
    SourceLocation ident_loc
) {
    // TODO: implement
}

void SemanticAnalyzer::analyze_var_decl(
    AnalyzerResult type,
    std::string const *ident,
    SourceLocation ident_loc,
    AnalyzerResult rhs,
    SourceLocation eqloc
) {
    // TODO: implement
}

void SemanticAnalyzer::analyze_type_alias(
    AnalyzerResult type,
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
