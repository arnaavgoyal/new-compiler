#ifndef AST_H
#define AST_H

#include <vector>
#include <string>
#include "source/source.h"
#include "lexer/token.h"
#include "analyzer/type.h"

namespace ast {

    enum node_type {

        // translation unit
        translation_unit,

        // type
        type,

        // integer literal
        int_lit,

        // character literal
        char_lit,

        // string literal
        str_lit,

        // variable declaration
        var_decl,

        // function parameter declaration
        param_decl,

        // function declaration
        func_decl,

        // a variable declaration statement
        decl_stmt,

        // return statement
        ret_stmt,

        // typedef statement
        typedef_stmt,

        // a scoped block of statements (func def, loop body, etc)
        stmt_block,

        // reference expression (var ref)
        ref_expr,

        // function call expression
        call_expr,

        // subscript expression (pointer, array)
        subscript_expr,

        // parenthesized expression
        paren_expr,

        // unary operation
        unary_op,

        // binary operation
        binary_op,

        // error recovery
        recovery,

        // error (for debugging purposes)
        error
    };

}

class ASTNode {

    friend class SemanticAnalyzer;

private:
public:
    ast::node_type kind;

    std::vector<ASTNode const *> children;

    Type const *type;

    std::string const *str;

    SourceLocation loc;

    token::token_type tok;

    bool has_error;

    ASTNode(
        ast::node_type kind,
        Type const *type,
        std::string const *str,
        SourceLocation loc,
        token::token_type tok,
        bool has_error
    ) { set(kind, type, str, loc, tok, has_error); }

    void set(
        ast::node_type kind,
        Type const *type,
        std::string const *str,
        SourceLocation loc,
        token::token_type tok,
        bool has_error
    );

    void print_ast(ASTNode const *tree, std::string str) const;



    ASTNode() : children(std::vector<ASTNode const *>())
        { kind = ast::error; type = nullptr; str = nullptr; }

    void print() const;

};

#endif