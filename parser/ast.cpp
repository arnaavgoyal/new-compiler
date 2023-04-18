#include "ast.h"
#include <iostream>

void print_ast(ASTNode *tree, std::string str) {
    if (tree != nullptr) {

        std::string type_str;
        switch (tree->type) {
            case ast::binary_op:
                type_str = "bin op";
                break;
            case ast::call_expr:
                type_str = "call expr";
                break;
            case ast::char_lit:
                type_str = "char lit";
                break;
            case ast::decl_stmt:
                type_str = "decl stmt";
                break;
            case ast::func_decl:
                type_str = "func decl";
                break;
            case ast::int_lit:
                type_str = "int lit";
                break;
            case ast::ref_expr:
                type_str = "ref expr";
                break;
            case ast::ret_stmt:
                type_str = "ret stmt";
                break;
            case ast::stmt_block:
                type_str = "stmt block";
                break;
            case ast::subscript_expr:
                type_str = "subscript expr";
                break;
            case ast::unary_op:
                type_str = "unary op";
                break;
            case ast::var_decl:
                type_str = "var decl";
                break;
        }

        std::cout << str << "`" << type_str << " '"
            << *(std::string *)tree->data << "'" << std::endl;
        str.push_back(' ');
        str.push_back('|');
        int size = tree->list.size();
        for (int i = 0; i < size; i++) {
            if (i == size - 1) {
                str.pop_back();
                str.push_back(' ');
            }
            print_ast(tree->list[i], str);
        }

    }
}

void print_ast(ASTNode *tree) {
    std::cout << "root" << std::endl;
    std::string s(" ");
    print_ast(tree, s);
}