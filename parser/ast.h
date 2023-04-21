#ifndef AST_H
#define AST_H

#include <vector>
#include <string>
#include "symbol/symbol.h"
#include "source/source.h"

namespace ast {

    enum node_type {

        // translation unit
        translation_unit,

        // integer literal
        int_lit,

        // character literal
        char_lit,

        // string literal
        str_lit,

        // variable declaration
        var_decl,

        // function declaration
        func_decl,

        // a variable declaration statement
        decl_stmt,

        // return statement
        ret_stmt,

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

        // unknown (for debugging purposes)
        unknown
    };

}

class ASTNode {

    // Parser edits these nodes directly when creating them
    friend class Parser;

private:

    /** the list of child nodes */
    std::vector<ASTNode *> list;

    /** the type of the node */
    ast::node_type type;

    /** source loc of this node. */
    SourceLocation loc;

    /** the type of the node data */
    token::token_type token_type;

    /**
     * node data is different based on data_type:
     * a) identifier     --  (std::string *)     owned by Allocator
     * b) literal        --  (std::string *)     owned by Allocator
     * c) operator       --  (char const *)      static
     * c) keyword        --  (char const *)      static
    */
    void *data;

    /**
     * Internal overload for print_ast().
    */
    static void print_ast(ASTNode *tree, std::string str);

public:

    /**
     * Constructs an ast node with type unknown.
    */
    ASTNode() {
        set(ast::unknown, token::unknown, nullptr);
    }

    void set(ast::node_type type, token::token_type token_type, void *data);

    std::vector<ASTNode *> const get_list() { return list; };
    ast::node_type get_type() { return type; };

    /**
     * Prints an ast node and all of its contents in a tree format.
    */
    void print();

};

#endif