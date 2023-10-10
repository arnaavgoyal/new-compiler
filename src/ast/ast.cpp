#include "ast/ast.h"
#include <iostream>
#include <iomanip>
#include "lexer/tokentypes.h"
#include "lexer/token.h"
#include "source/source.h"

static void print_err_loc_preamble(SourceLocation loc) {
#define LOC_NUM_WIDTH 2
    std::cout
        << std::setfill('0')
        << std::setw(LOC_NUM_WIDTH) << loc.start_row  << ":"
        << std::setw(LOC_NUM_WIDTH) << loc.start_col  << "::"
        << std::setw(LOC_NUM_WIDTH) << loc.end_row    << ":"
        << std::setw(LOC_NUM_WIDTH) << loc.end_col
        ;
#undef LOC_NUM_WIDTH
}

/** ------------------- ASTNode ------------------- */

void ASTNode::set(
    ast::node_type kind,
    Type const *type,
    std::string const *str,
    SourceLocation loc,
    token::token_type tok,
    bool has_error
) {
    this->kind = kind;
    this->type = type;
    this->str = str;
    this->loc = loc;
    this->tok = tok;
    this->has_error = has_error;
}

void ASTNode::print_ast(ASTNode const *tree, std::string str) const {

    if (tree != nullptr) {

        std::string type_str;
        switch (tree->kind) {
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
            case ast::cast_expr:
                type_str = "typecast";
                break;
            case ast::loop_stmt:
                type_str = "loop";
                break;
            case ast::error:
                type_str = "error";
                break;
            default:
                type_str = "?";
                break;
        }

        if (tree->kind != ast::recovery) {

            std::cout << str << "`" << "\e[0;97m" << type_str
                << "\e[0;93m" << " ";
            if (tree->str != nullptr) {
                std::cout << *tree->str << " ";
            }

            if (tree->type != nullptr) {
                std::cout
                    << "\e[0;92m"
                    << *(tree->type->get_str())
                    << "\e[1;34m" << " "
                    << *(tree->type->canonical->get_str());
            }

            std::cout << "\e[0;95m" << " <";
            print_err_loc_preamble(tree->loc);
            std::cout << ">";
            if (tree->has_error) {
                std::cout << "\e[0;91m" << " {contains errors}";
            }
            std::cout << "\e[0m" << std::endl;

            str.push_back(' ');
            str.push_back('|');
            int size = tree->children.size();
            //std::cout << "size: " << size << std::endl;
            for (int i = 0; i < size; i++) {
                if (i == size - 1) {
                    str.pop_back();
                    str.push_back(' ');
                }
                print_ast(tree->children[i], str);
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

void ASTNode::print() const {
    std::cout << "root" << std::endl;
    std::string s(" ");
    print_ast(this, s);
}