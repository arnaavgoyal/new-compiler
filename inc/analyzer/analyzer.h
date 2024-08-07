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

namespace fe {

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
    ASTNode *error_node;

    /**  */
    std::vector<token::token_type> primitive_keywords;

    /**  */
    std::vector<Type *> primitive_types;

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
    
    Symbol *find_symbol_in_current_scope(std::string *ident, Scope *scope);
    Symbol *find_symbol_in_any_active_scope(std::string *ident, Scope *scope);
    Symbol *insert_symbol(
        Scope *scope,
        std::string const &name,
        Type *type_ptr,
        ASTNode *decl
    );

    Type *find_type_in_current_scope(std::string *ident, Scope *scope);
    Type *find_type_in_any_active_scope(std::string *ident, Scope *scope);
    void insert_type(
        Scope *scope,
        std::string const &key,
        Type *value
    );

    bool typecheck_assignment(ASTNode *lhs, ASTNode *rhs);

public:

    SemanticAnalyzer(
        Allocator<std::string> &str_allocator,
        std::vector<token::token_type> const &primitives
    );

    ~SemanticAnalyzer();

    ASTNode *analyze_add_op_expr(
        ASTNode *lhs,
        ASTNode *rhs,
        token::token_type tok,
        SourceLocation op_loc
    );

    ASTNode *analyze_sub_op_expr(
        ASTNode *lhs,
        ASTNode *rhs,
        token::token_type tok,
        SourceLocation op_loc
    );

    ASTNode *analyze_binary_op_expr(
        op::kind op,
        ASTNode *lhs,
        ASTNode *rhs,
        token::token_type tok,
        SourceLocation op_loc
    );

    ASTNode *analyze_cast_expr(
        ASTNode *casted_expr,
        Type *cast_type,
        SourceLocation loc
    );

    FunctionType *analyze_function_type(
        Scope **scope,
        std::vector<Type *> param_types,
        Type *return_type,
        SourceLocation start_loc,
        SourceLocation end_loc
    );

    AliasType *analyze_typename(
        Scope **scope,
        std::string *ident,
        SourceLocation loc
    );

    PointerType *analyze_pointer_type(
        Scope **scope,
        Type *pointee,
        SourceLocation pointer_modifier_loc
    );

    ArrayType *analyze_array_type(
        Scope **scope,
        Type *array_of,
        SourceLocation array_modifier_loc
    );

    PrimitiveType *analyze_primitive_type(
        token::token_type prim,
        SourceLocation loc
    );

    ASTNode *analyze_postfix_op_expr(
        op::kind op,
        token::token_type tk,
        ASTNode *expr,
        SourceLocation loc
    );

    ASTNode *analyze_call_expr(
        ASTNode *expr,
        std::vector<ASTNode *> args,
        SourceLocation call_start_loc,
        SourceLocation call_end_loc
    );

    ASTNode *analyze_subscript_expr(
        ASTNode *expr,
        ASTNode *subscript,
        SourceLocation subscript_start_loc,
        SourceLocation subscript_end_loc
    );

    ASTNode *analyze_paren_expr(
        ASTNode *inside,
        SourceLocation loc
    );

    ASTNode *analyze_ref_expr(
        Scope **scope,
        std::string *ident,
        SourceLocation loc
    );

    ASTNode *analyze_character_literal(
        std::string *char_lit,
        SourceLocation loc
    );

    ASTNode *analyze_numeric_literal(
        std::string *num_lit,
        SourceLocation loc
    );

    ASTNode *analyze_string_literal(
        std::string *str_lit,
        SourceLocation loc
    );

    ASTNode *analyze_prefix_op_expr(
        op::kind op,
        token::token_type tk,
        ASTNode *expr,
        SourceLocation loc
    );

    ASTNode *analyze_func_decl(
        Scope **scope,
        Type *type,
        std::string *ident,
        std::vector<std::pair<std::string *, SourceLocation>> params,
        SourceLocation ident_loc,
        SourceLocation param_list_start_loc,
        SourceLocation param_list_end_loc
    );

    void start_func_define(
        ASTNode *decl,
        SourceLocation define_start_loc
    );

    void end_func_define(
        Scope **scope,
        SourceLocation define_end_loc
    );

    ASTNode *analyze_var_decl(
        Scope **scope,
        Type *type,
        std::string *ident,
        SourceLocation ident_loc
    );

    ASTNode *analyze_var_decl(
        Scope **scope,
        Type *type,
        std::string *ident,
        SourceLocation ident_loc,
        ASTNode *rhs,
        SourceLocation eqloc
    );

    ASTNode *analyze_type_alias(
        Scope **scope,
        Type *type,
        std::string *ident,
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

    ASTNode *analyze_return_stmt(
        SourceLocation retloc,
        ASTNode *expr
    );

    ASTNode *analyze_loop_stmt(
        ASTNode *cond,
        SourceLocation loc
    );

    ASTNode *analyze_if_stmt(
        ASTNode *cond,
        SourceLocation loc
    );

    void add_expr_as_stmt(
        ASTNode *expr
    );

};

}

#endif