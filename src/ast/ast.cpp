#include "ast/ast.h"
#include <iostream>
#include <iomanip>
#include "lexer/tokentypes.h"
#include "lexer/token.h"
#include "source/source.h"

static void print_err_loc_preamble(SourceLocation loc) {
#define LOC_NUM_WIDTH 2
    std::cout
        << std::setfill('0')        << " @ "
        << std::setw(LOC_NUM_WIDTH) << loc.start_row  << ":"
        << std::setw(LOC_NUM_WIDTH) << loc.start_col  << "::"
        << std::setw(LOC_NUM_WIDTH) << loc.end_row    << ":"
        << std::setw(LOC_NUM_WIDTH) << loc.end_col
        ;
#undef LOC_NUM_WIDTH
}

/** ------------------- ASTNode ------------------- */

void ASTNode::set(ast::node_type type, token::token_type token_type, void *data) {
    this->type = type;
    this->token_type = token_type;
    this->data = data;
}

void ASTNode::print_ast(ASTNode *tree, std::string str) {

    if (tree != nullptr) {

        std::string type_str;
        switch (tree->type) {
            case ast::translation_unit:
                type_str = "translation unit";
                break;
            case ast::binary_op:
                type_str = "bin op";
                break;
            case ast::call_expr:
                type_str = "call expr";
                break;
            case ast::char_lit:
                type_str = "char lit";
                break;
            case ast::str_lit:
                type_str = "str lit";
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
            case ast::paren_expr:
                type_str = "paren expr";
                break;
            case ast::typedef_stmt:
                type_str = "typedef stmt";
                break;
            case ast::type:
                type_str = "type";
                break;
            case ast::param_decl:
                type_str = "param decl";
                break;
            default:
                type_str = "?";
                break;
        }

        if (tree->type != ast::recovery) {

            std::cout << str << "`" << type_str << " '";

            if (token::is_keyword(tree->token_type) || token::is_operator(tree->token_type)) {
                std::cout << (char const *)tree->data;
            }
            else if (token::is_literal(tree->token_type) || tree->token_type == token::identifier) {
                std::cout << *(std::string *)tree->data;
            }

            std::cout << "' ";
            fflush(stdout);
            print_err_loc_preamble(tree->loc);
            std::cout << std::endl;

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

        // error recovery node
        else  {
            std::cout << str << "`" << "invalid -- error recovery" << std::endl;
        }
    }

    else {
        std::cout << str << "`" << "invalid -- nullptr" << std::endl;
    }
}

void ASTNode::print() {
    std::cout << "root" << std::endl;
    std::string s(" ");
    print_ast(this, s);
}