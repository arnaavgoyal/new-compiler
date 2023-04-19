#ifndef AST_H
#define AST_H

#include <vector>

namespace ast {

    enum node_type {

        // integer literal
        int_lit,

        // character literal
        char_lit,

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

        // unary operation
        unary_op,

        // binary operation
        binary_op,

        // unknown (for debugging purposes)
        unknown
    };

}

class ASTNode {
public:
    std::vector<ASTNode *> list;
    ast::node_type type;
    void *data;
};

void print_ast(ASTNode *tree);

#endif