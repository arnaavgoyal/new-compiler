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

            // unary postfix operators
            case token::op_plusplus:
            case token::op_minusminus:
                n = new ASTNode;
                n->data = tk.get_ptr();
                n->type = ast::unary_op;
                n->list.push_back(node);
                node = n;
                break;
            
            // func call
            case token::op_leftparen:
                n = new ASTNode;
                n->data = tk.get_ptr();
                n->type = ast::call_expr;
                n->list.push_back(node);
                lexer.lex(tk);
                node = n;

                // parse func call arguments
                parse_comma(token::op_rightparen, node);
                break;

            // array subscript
            case token::op_leftbracket:
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

            // no postfix operators left
            default:
                return node;
        }
    }
}

ASTNode *Parser::parse_prefix() {
    token::token_type type = tk.get_type();

    ASTNode *node = nullptr;
    switch (type) {

        // parenthesized expr or typecast
        case token::op_leftparen:
            // TODO: check for typecast
            lexer.lex(tk);
            node = parse_expr(token::op_rightparen);
            lexer.lex(tk);
            break;

        // char literal
        case token::character_literal:
            node = new ASTNode;
            node->type = ast::char_lit;
            node->data = tk.get_ptr();
            lexer.lex(tk);
            break;
        
        // numeric literal
        case token::numeric_literal:
            node = new ASTNode;
            node->data = tk.get_ptr();
            node->type = ast::int_lit;
            lexer.lex(tk);
            break;

        // identifier with no prefix operators
        case token::identifier:
            // parse postfix operators
            node = parse_postfix();
            break;

        // prefix unary operators
        case token::op_plusplus:
        case token::op_minusminus:
        case token::op_plus:
        case token::op_minus:
        case token::op_exclamation:
        case token::op_asterisk:
        case token::op_amp:
            node = new ASTNode;
            node->data = tk.get_ptr();
            node->type = ast::unary_op;
            lexer.lex(tk);
            node->list.push_back(parse_prefix());
            break;

        // empty
        default:
            node = nullptr;
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

ASTNode *Parser::parse_comma(token::token_type stop, ASTNode *cn) {

    ASTNode *node;

    // func decl param / call expr arg separator
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
        node = cn;
    }

    // binary op
    else {
        node = parse_assignment();
        ASTNode *n;
        bool br = true;
        while (br) {
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
                    br = false;
                    break;
            }
        }
    }
    
    if (tk.get_type() != stop) {
        printf("syntax error -- expected %s\n", token::get_token_string(stop));
    }

    return node;
}

ASTNode *Parser::parse_expr(token::token_type stop) {
    return parse_comma(stop, nullptr);
}

Parser::Parser() { }

Parser::Parser(Lexer &l) {
    lexer = l;

    // initial lex to ensure that tk contains the first token
    lexer.lex(tk);
}

ASTNode *Parser::parse_stmt() {

    ASTNode *node;
    ASTNode *temp;
    std::string *decl;

    switch (tk.get_type()) {

        case token::eof:
            node = nullptr;
            break;

        // scoped statement block
        case token::op_leftbrace:
            node = new ASTNode;
            node->type = ast::stmt_block;
            node->data = tk.get_ptr();

            // consume left brace
            lexer.lex(tk);

            // parse stmts
            while (true) {

                // found end brace
                if (tk.get_type() == token::op_rightbrace) {
                    break;
                }

                // reached eof without encountering end brace
                else if (tk.get_type() == token::eof) {
                    printf("syntax error\n");
                    exit(EXIT_FAILURE);
                }

                // statement
                node->list.push_back(parse_stmt());

            }

            // consume right brace
            lexer.lex(tk);

            break;

        // return statement
        case token::kw_return:
            node = new ASTNode;
            node->type = ast::ret_stmt;
            node->data = tk.get_ptr();

            // make curr token the start of expr
            lexer.lex(tk);

            // parse expr until semi
            temp = parse_expr(token::op_semicolon);

            // consume semi
            lexer.lex(tk);

            // check if there was an expr
            if (temp == nullptr) {
                printf("syntax error\n");
                exit(EXIT_FAILURE);
            }

            // add expr to ret stmt
            node->list.push_back(temp);
            break;

        // primitive type decl
        case token::kw_bool:
        case token::kw_i16:
        case token::kw_u16:
        case token::kw_i32:
        case token::kw_u32:
        case token::kw_i64:
        case token::kw_u64:
        case token::kw_f32:
        case token::kw_f64:

            // generic decl -- type unknown so far
            node = new ASTNode;
            node->data = tk.get_ptr();

            // lex next -- expecting identifier
            lexer.lex(tk);
            if (tk.get_type() != token::identifier) {
                printf("syntax error\n");
                exit(EXIT_FAILURE);
            }

            // keep identifier
            decl = new std::string(*(std::string *)node->data + " " + *(std::string *)tk.get_ptr());
            node->data = decl;

            // lex next -- determines type of decl
            lexer.lex(tk);

            // func decl
            if (tk.get_type() == token::op_leftparen) {
                node->type = ast::func_decl;

                // consume left paren
                lexer.lex(tk);

                // parse expr til right paren
                parse_comma(token::op_rightparen, node);

                // consume right paren
                lexer.lex(tk);

                // func decl
                if (tk.get_type() == token::op_semicolon) {
                    // consume semi
                    lexer.lex(tk);
                }

                // func decl with define
                else if (tk.get_type() == token::op_leftbrace) {
                    // already on left brace so recursive call will handle it
                    node->list.push_back(parse_stmt());
                }

                // unknown
                else {
                    printf("syntax error\n");
                    exit(EXIT_FAILURE);
                }
            }

            // var decl with define
            else if (tk.get_type() == token::op_equal) {
                node->type = ast::var_decl;

                // make curr token the start of expr
                lexer.lex(tk);

                // parse expr until semi
                node->list.push_back(parse_expr(token::op_semicolon));

                // consume semi
                lexer.lex(tk);
            }

            // var decl
            else if (tk.get_type() == token::op_semicolon) {
                node->type = ast::var_decl;

                // consume semi
                lexer.lex(tk);
            }

            // unknown
            else {
                printf("syntax error\n");
                exit(EXIT_FAILURE);
            }
            break;

        // assume that it is an expr by default
        default:

            // parse expr until semi
            node = parse_expr(token::op_semicolon);

            // consume semi
            lexer.lex(tk);
            break;
    }

    //std::cout << token::get_token_string(tk.get_type()) << std::endl;

    return node;
}