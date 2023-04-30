#ifndef ANALYZER_H
#define ANALYZER_H

#include <string>
#include "analyzer/symbol.h"
#include "analyzer/hashtable.h"
#include <stack>
#include "analyzer/type.h"
#include "ast/ast.h"
#include "memory/allocator.h"
#include <string>

class AnalyzerResult {

    friend class SemanticAnalyzer;

private:

    union {
        ASTNode const *node;
        Type const *type;
    };
    bool has_error;

    AnalyzerResult(Type const *ty, bool err = false)
        { type = ty; has_error = err; }

    AnalyzerResult(ASTNode const *nd, bool err = false)
        { node = nd; has_error = err; }

public:

    AnalyzerResult() { }

};

class SemanticAnalyzer {
private:

    enum scope_status { open, closed };
    using scope_id_t = unsigned long int;

    /** symbol (vars and funcs) table */
    HashTable<std::pair<scope_id_t, Symbol const *>> symbol_table;

    /** type table */
    HashTable<std::pair<scope_id_t, Type const *>> type_table;

    /** scope stack */
    std::stack<scope_id_t> scope_stack;

    /** list of all scopes and their status */
    std::vector<scope_status> scope_history;

    /** the current scope's id */
    scope_id_t curr_scope_id;

    /** the id generator for new scopes */
    scope_id_t scope_id_gen;

    /**  */
    Allocator<std::string> &str_allocator;

    /**  */
    Allocator<Symbol> sym_allocator;

    /**  */
    Allocator<Type> type_allocator;

    /**  */
    Allocator<ASTNode> node_allocator;

    /**  */
    Type *error_type;

    /**  */
    ASTNode *error_node;

    /**  */
    std::vector<token::token_type> primitive_keywords;

    /**  */
    std::vector<Type const *> primitive_types;

    /**
     * Determines the status of the scope with given scope id.
     * 
     * @param id the scope id
     * @return the status
    */
    scope_status status(scope_id_t id);

    /**
     * Generates a new scope id, pushes it onto the scope stack,
     * and set it as the current scope.
    */
    void enter_new_scope();

    /**
     * Pops the current scope off the stack and sets the new top of
     * the stack as the current scope.
    */
    void exit_current_scope();
    
    Symbol const *find_symbol_in_current_scope(std::string const *ident);
    Symbol const *find_symbol_in_any_active_scope(std::string const *ident);
    void insert_symbol(
        std::string const *key,
        std::string const &name,
        Type const *type_ptr
    );

    Type const *find_type_in_current_scope(std::string const *ident);
    Type const *find_type_in_any_active_scope(std::string const *ident);
    void insert_type(
        std::string const *key,
        Type const *value
    );

public:

    SemanticAnalyzer(
        Allocator<std::string> &str_allocator,
        std::vector<token::token_type> const &primitives
    );

    AnalyzerResult analyze_function_type(
        std::vector<AnalyzerResult> param_types,
        AnalyzerResult return_type,
        SourceLocation start_loc,
        SourceLocation end_loc
    );

    AnalyzerResult analyze_typename(
        std::string const *ident,
        SourceLocation loc
    );

    AnalyzerResult analyze_pointer_type(
        AnalyzerResult pointee,
        SourceLocation pointer_modifier_loc
    );

    AnalyzerResult analyze_array_type(
        AnalyzerResult array_of,
        SourceLocation array_modifier_loc
    );

    AnalyzerResult analyze_primitive_type(
        token::token_type prim,
        SourceLocation loc
    );

    AnalyzerResult analyze_binary_op_expr(
        token::token_type op,
        AnalyzerResult lhs,
        AnalyzerResult rhs,
        SourceLocation op_loc
    );

    AnalyzerResult analyze_postfix_op_expr(
        token::token_type op,
        AnalyzerResult expr,
        SourceLocation op_loc
    );

    AnalyzerResult analyze_call_expr(
        AnalyzerResult expr,
        std::vector<AnalyzerResult> args,
        SourceLocation call_start_loc,
        SourceLocation call_end_loc
    );

    AnalyzerResult analyze_subscript_expr(
        AnalyzerResult expr,
        AnalyzerResult subscript,
        SourceLocation subscript_start_loc,
        SourceLocation subscript_end_loc
    );

    AnalyzerResult analyze_paren_expr(
        AnalyzerResult inside,
        SourceLocation left_paren_loc,
        SourceLocation right_paren_loc
    );

    AnalyzerResult analyze_ref_expr(
        std::string const *ident,
        SourceLocation loc
    );

    AnalyzerResult analyze_character_literal(
        std::string const *char_lit,
        SourceLocation loc
    );

    AnalyzerResult analyze_numeric_literal(
        std::string const *num_lit,
        SourceLocation loc
    );

    AnalyzerResult analyze_string_literal(
        std::string const *str_lit,
        SourceLocation loc
    );

    AnalyzerResult analyze_prefix_op_expr(
        token::token_type op,
        AnalyzerResult expr,
        SourceLocation op_loc
    );

    AnalyzerResult analyze_func_decl(
        AnalyzerResult type,
        std::string const *ident,
        std::vector<std::string const *> params,
        SourceLocation ident_loc,
        SourceLocation lparen_loc,
        SourceLocation rparen_loc
    );

    void start_func_define(
        AnalyzerResult decl,
        SourceLocation define_start_loc
    );

    void end_func_define(
        SourceLocation define_end_loc
    );

    void analyze_var_decl(
        AnalyzerResult type,
        std::string const *ident,
        SourceLocation ident_loc
    );

    void analyze_var_decl(
        AnalyzerResult type,
        std::string const *ident,
        SourceLocation ident_loc,
        AnalyzerResult rhs,
        SourceLocation eqloc
    );

    void analyze_type_alias(
        AnalyzerResult type,
        std::string const *ident,
        SourceLocation ident_loc
    );

    void start_scoped_block(
        SourceLocation block_start_loc
    );

    void end_scoped_block(
        SourceLocation block_end_loc
    );

    void analyze_return_stmt(
        AnalyzerResult expr
    );

    void add_expr_as_stmt(
        AnalyzerResult expr
    );

};

#endif