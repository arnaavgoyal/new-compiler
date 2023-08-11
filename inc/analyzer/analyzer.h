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
#include "analyzer/scope.h"

template <typename T>
class AnalyzedGeneric {

    friend class SemanticAnalyzer;

private:
public:
    T contents;

    AnalyzedGeneric(T t, bool err = false)
        { contents = t; }



    AnalyzedGeneric() { }

    AnalyzedGeneric(AnalyzedGeneric const &other) {
        contents = other.contents;
    }

    AnalyzedGeneric const &operator=(AnalyzedGeneric const &other) {
        contents = other.contents;
        return *this;
    }

};

typedef AnalyzedGeneric<ASTNode *> AnalyzerResult;
typedef AnalyzedGeneric<ASTNode *> AnalyzedStmt;
typedef AnalyzedGeneric<ASTNode *> AnalyzedExpr;
typedef AnalyzedGeneric<Type const *> AnalyzedType;

class SemanticAnalyzer {
private:

    /**  */
    Allocator<std::string> &str_allocator;

    /**  */
    Allocator<Symbol> sym_allocator;

    /**  */
    Allocator<Type> type_allocator;

    /**  */
    Allocator<ASTNode> node_allocator;

    /**  */
    Allocator<Scope> scope_allocator;

    /**  */
    Type *error_type;

    /**  */
    ASTNode *error_node;

    /**  */
    std::vector<token::token_type> primitive_keywords;

    /**  */
    std::vector<Type const *> primitive_types;

    Scope *gscope;

public:

    Scope *global_scope();

private:

    /**
     * Generates a new scope id, pushes it onto the scope stack,
     * and set it as the current scope.
    */
    void enter_new_scope(Scope **curr);

    /**
     * Pops the current scope off the stack and sets the new top of
     * the stack as the current scope.
    */
    void exit_current_scope(Scope **curr);
    
    Symbol const *find_symbol_in_current_scope(std::string const *ident, Scope **scope);
    Symbol const *find_symbol_in_any_active_scope(std::string const *ident, Scope **scope);
    void insert_symbol(
        Scope **scope,
        std::string const &name,
        Type const *type_ptr
    );

    Type const *find_type_in_current_scope(std::string const *ident, Scope **scope);
    Type const *find_type_in_any_active_scope(std::string const *ident, Scope **scope);
    void insert_type(
        Scope **scope,
        std::string const &key,
        Type *value
    );

public:

    SemanticAnalyzer(
        Allocator<std::string> &str_allocator,
        std::vector<token::token_type> const &primitives
    );

    AnalyzedType analyze_function_type(
        Scope **scope,
        std::vector<AnalyzedType> param_types,
        AnalyzedType return_type,
        SourceLocation start_loc,
        SourceLocation end_loc
    );

    AnalyzedType analyze_typename(
        Scope **scope,
        std::string const *ident,
        SourceLocation loc
    );

    AnalyzedType analyze_pointer_type(
        Scope **scope,
        AnalyzedType pointee,
        SourceLocation pointer_modifier_loc
    );

    AnalyzedType analyze_array_type(
        Scope **scope,
        AnalyzedType array_of,
        SourceLocation array_modifier_loc
    );

    AnalyzedType analyze_primitive_type(
        token::token_type prim,
        SourceLocation loc
    );

    AnalyzedExpr analyze_binary_op_expr(
        token::token_type op,
        AnalyzedExpr lhs,
        AnalyzedExpr rhs,
        SourceLocation op_loc
    );

    AnalyzedExpr analyze_postfix_op_expr(
        token::token_type op,
        AnalyzedExpr expr,
        SourceLocation op_loc
    );

    AnalyzedExpr analyze_call_expr(
        AnalyzedExpr expr,
        std::vector<AnalyzedExpr> args,
        SourceLocation call_start_loc,
        SourceLocation call_end_loc
    );

    AnalyzedExpr analyze_subscript_expr(
        AnalyzedExpr expr,
        AnalyzedExpr subscript,
        SourceLocation subscript_start_loc,
        SourceLocation subscript_end_loc
    );

    AnalyzedExpr analyze_paren_expr(
        AnalyzedExpr inside,
        SourceLocation loc
    );

    AnalyzedExpr analyze_ref_expr(
        Scope **scope,
        std::string const *ident,
        SourceLocation loc
    );

    AnalyzedExpr analyze_character_literal(
        std::string const *char_lit,
        SourceLocation loc
    );

    AnalyzedExpr analyze_numeric_literal(
        std::string const *num_lit,
        SourceLocation loc
    );

    AnalyzedExpr analyze_string_literal(
        std::string const *str_lit,
        SourceLocation loc
    );

    AnalyzedExpr analyze_prefix_op_expr(
        token::token_type op,
        AnalyzedExpr expr,
        SourceLocation op_loc
    );

    AnalyzedStmt analyze_func_decl(
        Scope **scope,
        AnalyzedType type,
        std::string const *ident,
        std::vector<std::pair<std::string const *, SourceLocation>> params,
        SourceLocation ident_loc,
        SourceLocation param_list_loc
    );

    void start_func_define(
        AnalyzerResult decl,
        SourceLocation define_start_loc
    );

    void end_func_define(
        Scope **scope,
        SourceLocation define_end_loc
    );

    AnalyzedStmt analyze_var_decl(
        Scope **scope,
        AnalyzedType type,
        std::string const *ident,
        SourceLocation ident_loc
    );

    AnalyzedStmt analyze_var_decl(
        Scope **scope,
        AnalyzedType type,
        std::string const *ident,
        SourceLocation ident_loc,
        AnalyzerResult rhs,
        SourceLocation eqloc
    );

    AnalyzedStmt analyze_type_alias(
        Scope **scope,
        AnalyzedType type,
        std::string const *ident,
        SourceLocation ident_loc
    );

    void start_scoped_block(
        Scope **scope,
        SourceLocation block_start_loc
    );

    void end_scoped_block(
        Scope **scope,
        SourceLocation block_end_loc
    );

    AnalyzedStmt analyze_return_stmt(
        AnalyzerResult expr
    );

    void add_expr_as_stmt(
        AnalyzerResult expr
    );

};

#endif