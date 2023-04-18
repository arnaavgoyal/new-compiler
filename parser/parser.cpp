#include "parser/parser.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "lexer/lexer.h"
#include <iostream>

ASTNode *Parser::parse_postfix() {

    // Expect for current token to be the identifier
    // Make a node for the identifier
    ASTNode *node = new ASTNode;
    node->type = ast::ref_expr;
    node->data = tk.get_ptr();

    // Iterate over every postfix operator (if any)
    ASTNode *n = nullptr;
    ASTNode *j = nullptr;
    while (true) {
        lexer.lex(tk);
        switch (tk.get_type()) {
            case token::op_plusplus:
            case token::op_minusminus:
                // unary postfix operators
                n = new ASTNode;
                n->data = tk.get_ptr();
                n->type = ast::unary_op;
                n->list.push_back(node);
                node = n;
                break;
            
            case token::op_leftparen:
                // func call
                n = new ASTNode;
                n->data = tk.get_ptr();
                n->type = ast::call_expr;
                n->list.push_back(node);
                lexer.lex(tk);
                node = n;

                // TODO: parse func call arguments
                //while (true) {
                    parse_comma(node);
                    // if (n == nullptr || tk.get_type() == token::op_rightparen) {
                    //     break;
                    // }
                //}
                break;

            case token::op_leftbracket:
                // array subscript
                n = new ASTNode;
                n->data = tk.get_ptr();
                n->type = ast::subscript_expr;
                n->list.push_back(node);
                lexer.lex(tk);

                // Parse expression inside of subscript
                j = parse_expr(token::op_rightbracket);
                n->list.push_back(j);
                node = n;
                break;

            default:
                // no postfix operators left
                return node;
        }
    }
}

ASTNode *Parser::parse_prefix() {
    token::token_type type = tk.get_type();
    ASTNode *node = nullptr;
    switch (type) {
        case token::op_leftparen:
            // TODO: check for typecast
            // parenthesized expr
            lexer.lex(tk);
            node = parse_expr(token::op_rightparen);
            lexer.lex(tk);
            break;

        case token::character_literal:
            // char literal
            node = new ASTNode;
            node->type = ast::char_lit;
            node->data = tk.get_ptr();
            lexer.lex(tk);
            break;

        case token::numeric_literal:
            // numeric literal
            node = new ASTNode;
            node->data = tk.get_ptr();
            node->type = ast::int_lit;
            lexer.lex(tk);
            break;

        case token::identifier:
            // identifier with no prefix operators
            // could have postfix operators
            node = parse_postfix();
            break;

        default:
            // prefix unary operator
            node = new ASTNode;
            node->data = tk.get_ptr();
            node->type = ast::unary_op;
            lexer.lex(tk);
            node->list.push_back(parse_prefix());
            break;
    }

    return node;
}

ASTNode *Parser::parse_multiplicative() {
    ASTNode *node = parse_prefix();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_asterisk:
            case token::op_slash:
            case token::op_percent:
                // bin mult op
                n = new ASTNode;
                n->type = ast::binary_op;
                n->data = tk.get_ptr();
                n->list.push_back(node);
                lexer.lex(tk);
                n->list.push_back(parse_prefix());
                node = n;
                break;

            default:
                // no bin mult ops left
                return node;
        }
    }
}

ASTNode *Parser::parse_additive() {
    ASTNode *node = parse_multiplicative();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_plus:
            case token::op_minus:
                // bin add op
                n = new ASTNode;
                n->type = ast::binary_op;
                n->data = tk.get_ptr();
                n->list.push_back(node);
                lexer.lex(tk);
                n->list.push_back(parse_multiplicative());
                node = n;
                break;

            default:
                // no bin add ops left
                return node;
        }
    }
}

ASTNode *Parser::parse_gl_relational() {
    ASTNode *node = parse_additive();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_greater:
            case token::op_greaterequal:
            case token::op_less:
            case token::op_lessequal:
                // bin relational op
                n = new ASTNode;
                n->type = ast::binary_op;
                n->data = tk.get_ptr();
                n->list.push_back(node);
                lexer.lex(tk);
                n->list.push_back(parse_additive());
                node = n;
                break;

            default:
                // no bin relational ops left
                return node;
        }
    }
}

ASTNode *Parser::parse_eq_relational() {
    ASTNode *node = parse_gl_relational();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_equalequal:
            case token::op_exclamationequal:
                // bin relational op
                n = new ASTNode;
                n->type = ast::binary_op;
                n->data = tk.get_ptr();
                n->list.push_back(node);
                lexer.lex(tk);
                n->list.push_back(parse_gl_relational());
                node = n;
                break;

            default:
                // no bin relational ops left
                return node;
        }
    }
}

ASTNode *Parser::parse_logical_and() {
    ASTNode *node = parse_eq_relational();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_ampamp:
                // logical AND
                n = new ASTNode;
                n->type = ast::binary_op;
                n->data = tk.get_ptr();
                n->list.push_back(node);
                lexer.lex(tk);
                n->list.push_back(parse_eq_relational());
                node = n;
                break;

            default:
                // none left
                return node;
        }
    }
}

ASTNode *Parser::parse_logical_or() {
    ASTNode *node = parse_logical_and();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_pipepipe:
                // logical AND
                n = new ASTNode;
                n->type = ast::binary_op;
                n->data = tk.get_ptr();
                n->list.push_back(node);
                lexer.lex(tk);
                n->list.push_back(parse_logical_and());
                node = n;
                break;

            default:
                // none left
                return node;
        }
    }
}

ASTNode *Parser::parse_assignment() {
    ASTNode *node = parse_logical_or();
    ASTNode *n;
    switch (tk.get_type()) {
        case token::op_equal:
            // assignment
            n = new ASTNode;
            n->type = ast::binary_op;
            n->data = tk.get_ptr();
            n->list.push_back(node);
            lexer.lex(tk);
            n->list.push_back(parse_assignment());
            node = n;
            break;
        default:
            break;
    }
    return node;
}

ASTNode *Parser::parse_comma(ASTNode *cn) {

    ASTNode *node;

    // func call arg separator
    if (cn != nullptr) {
        while (true) {
            node = parse_assignment();
            if (node == nullptr) {
                break;
            }
            cn->list.push_back(node);
            if (tk.get_type() == token::op_comma) {
                lexer.lex(tk);
            }
            else {
                break;
            }
        }
        return cn;
    }

    // binary op
    node = parse_assignment();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_comma:
                // comma
                n = new ASTNode;
                n->type = ast::binary_op;
                n->data = tk.get_ptr();
                n->list.push_back(node);
                lexer.lex(tk);
                n->list.push_back(parse_assignment());
                node = n;
                break;

            default:
                // none left
                return node;
        }
    }
}

ASTNode *Parser::parse_expr(token::token_type stop = token::op_semicolon) {
    ASTNode *ret = parse_comma();
    //std::cout << token::get_token_string(tk.get_type()) << std::endl;
    return ret;
}

Parser::Parser() { }

Parser::Parser(Lexer &l) {
    this->lexer = l;
}

ASTNode *Parser::parse() {

    ASTNode *node;
    ASTNode *temp;

    lexer.lex(tk);
    switch (tk.get_type()) {

        // return statement
        case token::kw_return:
            node = new ASTNode;
            node->type = ast::ret_stmt;
            node->data = tk.get_ptr();
            lexer.lex(tk);
            temp = parse_expr();
            if (temp == nullptr) {
                printf("syntax error\n");
                exit(EXIT_FAILURE);
            }
            node->list.push_back(temp);
            break;

        // primitive type var decl
        case token::kw_i16:
        case token::kw_u16:
        case token::kw_i32:
        case token::kw_u32:
        case token::kw_i64:
        case token::kw_u64:
        case token::kw_f32:
        case token::kw_f64:
            node = new ASTNode;
            node->type = ast::var_decl;
            node->data = tk.get_ptr();
            lexer.lex(tk);
            temp = parse_expr();
            if (temp == nullptr) {
                printf("syntax error\n");
                exit(EXIT_FAILURE);
            }
            node->list.push_back(temp);
            break;

        default:
            node = parse_expr(token::op_semicolon);
            break;
    }

    std::cout << token::get_token_string(tk.get_type()) << std::endl;

    return node;
}