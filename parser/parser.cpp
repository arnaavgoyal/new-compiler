#include "parser/parser.h"
#include "parser/ast.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "lexer/lexer.h"
#include "symbol/symbol.h"
#include "memory/allocator.h"
#include "error/error.h"
#include <iostream>
#include <iomanip>
#include <algorithm>

void Parser::consume() {
    prev_tk_loc = tk.get_src_loc();
    lexer.lex(tk);
}

ASTNode *Parser::parse_postfix(ASTNode *node) {

    // Iterate over every postfix operator (if any)
    ASTNode *n = nullptr;
    ASTNode *j = nullptr;
    while (true) {
        switch (tk.get_type()) {

            // unary postfix operators
            case token::op_plusplus:
            case token::op_minusminus:

                // Make new node
                n = node_allocator.alloc();
                n->set(
                    ast::node_type::unary_op,
                    tk.get_type(),
                    (void *)tk.get_operator_str()
                );
                n->loc = node->loc;
                n->loc.copy_end(tk.get_src_loc());
                n->list.push_back(node);
                node = n;

                // consume operator
                consume();
                break;
            
            // func call
            case token::op_leftparen:
                n = node_allocator.alloc();
                n->set(
                    ast::call_expr,
                    tk.get_type(),
                    (void *)tk.get_operator_str()
                );
                n->loc = node->loc;
                n->list.push_back(node);
                node = n;

                // consume left paren
                consume();

                // parse func call arguments
                parse_expr(token::op_rightparen, node);

                // get end loc
                node->loc.copy_end(tk.get_src_loc());

                // consume right paren
                consume();
                break;

            // array subscript
            case token::op_leftbracket:
                n = node_allocator.alloc();
                n->set(
                    ast::subscript_expr,
                    tk.get_type(),
                    (void *)tk.get_operator_str()
                );
                n->loc = node->loc;
                n->list.push_back(node);

                // consume left bracket
                consume();

                // Parse expression inside of subscript
                j = parse_expr(token::op_rightbracket);
                n->list.push_back(j);

                n->loc.copy_end(tk.get_src_loc());
                node = n;

                // consume right bracket
                consume();
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
    ASTNode *temp;
    SourceLocation t_loc;
    switch (type) {

        // parenthesized expr or typecast
        case token::op_leftparen:

            // TODO: check for typecast

            // make node for paren expr
            node = node_allocator.alloc();
            node->set(
                ast::paren_expr,
                tk.get_type(),
                (void *)tk.get_operator_str()
            );

            // save start_* loc
            node->loc = tk.get_src_loc();

            // consume left paren
            consume();

            // parse expr
            node->list.push_back(parse_expr(token::op_rightparen));

            // save end_* loc
            node->loc.copy_end(tk.get_src_loc());

            // consume right paren
            consume();

            // parse postfix
            node = parse_postfix(node);
            break;

        // identifier with no prefix operators
        case token::identifier:

            // create node for identifier
            node = node_allocator.alloc();
            node->set(
                ast::ref_expr,
                tk.get_type(),
                (void *)tk.get_identifier_str()
            );
            node->loc = tk.get_src_loc();

            // consume identifier
            consume();

            // parse postfix operators
            node = parse_postfix(node);
            break;

        // char literal
        case token::character_literal:

            // make node
            node = node_allocator.alloc();
            node->set(
                ast::node_type::char_lit,
                tk.get_type(),
                (void *)tk.get_literal_str()
            );
            node->loc = tk.get_src_loc();

            // next
            consume();
            break;
        
        // numeric literal
        case token::numeric_literal:

            // make node
            node = node_allocator.alloc();
            node->set(
                // numeric_literal type is any type of number,
                // meaning it could be an int or a float.
                // TODO: fix this
                ast::node_type::int_lit,
                tk.get_type(),
                (void *)tk.get_literal_str()
            );
            node->loc = tk.get_src_loc();

            // next
            consume();
            break;

        // string literal
        case token::string_literal:

            // make node
            node = node_allocator.alloc();
            node->set(
                ast::node_type::str_lit,
                tk.get_type(),
                (void *)tk.get_literal_str()
            );
            node->loc = tk.get_src_loc();

            // next
            consume();
            break;

        // prefix unary operators
        case token::op_plusplus:
        case token::op_minusminus:
        case token::op_plus:
        case token::op_minus:
        case token::op_exclamation:
        case token::op_asterisk:
        case token::op_amp:

            // make node
            node = node_allocator.alloc();
            node->set(
                ast::node_type::unary_op,
                tk.get_type(),
                (void *)tk.get_operator_str()
            );
            node->loc = tk.get_src_loc();

            // next
            consume();

            // recursively descend
            temp = parse_prefix();
            node->list.push_back(temp);

            // update loc with ending point
            node->loc.copy_end(temp->loc);
            break;

        // empty
        default:
            
            // this nullptr propagates up the precedence chain
            node = nullptr;
            break;
    }

    return node;
}

ASTNode *Parser::parse_multiplicative() {
    static std::vector<token::token_type> types {
        token::op_asterisk,
        token::op_slash,
        token::op_percent
    };
    return left_assoc_bin_op(parse_prefix, types);
}

ASTNode *Parser::parse_additive() {
    static std::vector<token::token_type> types {
        token::op_plus,
        token::op_minus
    };
    return left_assoc_bin_op(parse_multiplicative, types);
}

ASTNode *Parser::parse_gl_relational() {
    static std::vector<token::token_type> types {
        token::op_greater,
        token::op_greaterequal,
        token::op_less,
        token::op_lessequal
    };
    return left_assoc_bin_op(parse_additive, types);
}

ASTNode *Parser::parse_eq_relational() {
    static std::vector<token::token_type> types {
        token::op_equalequal,
        token::op_exclamationequal
    };
    return left_assoc_bin_op(parse_gl_relational, types);
}

ASTNode *Parser::parse_logical_and() {
    static std::vector<token::token_type> types {
        token::op_ampamp
    };
    return left_assoc_bin_op(parse_eq_relational, types);
}

ASTNode *Parser::parse_logical_or() {
    static std::vector<token::token_type> types {
        token::op_pipepipe
    };
    return left_assoc_bin_op(parse_logical_and, types);
}

ASTNode *Parser::parse_assignment() {
    static std::vector<token::token_type> types {
        token::op_equal
    };
    return right_assoc_bin_op(parse_logical_or, types);
}

ASTNode *Parser::parse_comma(token::token_type stop, ASTNode *cn) {

    // for binary op
    static std::vector<token::token_type> types {
        token::op_comma
    };

    ASTNode *node;

    // call expr arg separator
    if (cn) {
        while (true) {
            node = parse_assignment();
            if (node == nullptr) {
                break;
            }
            cn->list.push_back(node);
            if (tk.get_type() == token::op_comma) {
                consume();
            }
            else {
                break;
            }
        }
        node = cn;
    }

    // binary op
    else {
        node = left_assoc_bin_op(parse_assignment, types);  
    }

    return node;
}

ASTNode *Parser::parse_expr(token::token_type stop, ASTNode *cn) {
    ASTNode *node = parse_comma(stop, cn);
    if (tk.get_type() != stop) {
        std::string missing = "'";
        missing = missing + token::get_operator_string(stop) + "'";
        ErrorHandler::handle_missing(tk.get_src_loc(), missing.c_str());
        exit(EXIT_FAILURE);
    }
    return node;
}

ASTNode *Parser::left_assoc_bin_op(
    ASTNode *(Parser::*higher_prec)(),
    std::vector<token::token_type> const &types
) {
    ASTNode *node = (this->*higher_prec)();
    ASTNode *n;
    bool go = true;
    while (go) {
        if (std::find(types.begin(), types.end(), tk.get_type()) != types.end()) {
            n = node_allocator.alloc();
            n->set(
                ast::node_type::binary_op,
                tk.get_type(),
                (void *)tk.get_operator_str()
            );
            n->loc = node->loc;
            n->list.push_back(node);
            consume();
            node = (this->*higher_prec)();
            if (node == nullptr) {
                ErrorHandler::handle_missing(tk.get_src_loc(), "expression");
                exit(EXIT_FAILURE);
            }
            n->loc.copy_end(node->loc);
            n->list.push_back(node);
            node = n;
        }
        else {
            go = false;
        }
    }
    return node;
}

ASTNode *Parser::right_assoc_bin_op(
    ASTNode *(Parser::*higher_prec)(),
    std::vector<token::token_type> const &types
) {
    ASTNode *node = (this->*higher_prec)();
    ASTNode *n;
    if (std::find(types.begin(), types.end(), tk.get_type()) != types.end()) {
        n = node_allocator.alloc();
        n->set(
            ast::node_type::binary_op,
            tk.get_type(),
            (void *)tk.get_operator_str()
        );
        n->loc = node->loc;
        n->list.push_back(node);
        consume();
        node = right_assoc_bin_op(higher_prec, types);
        if (node == nullptr) {
            ErrorHandler::handle_missing(tk.get_src_loc(), "expression");
            exit(EXIT_FAILURE);
        }
        n->loc.copy_end(node->loc);
        n->list.push_back(node);
        node = n;
    }
    return node;
}

ASTNode *Parser::parse_var_decl() {
    // assumes that current token is a type
    // FOR NOW check if is ident or keyword
    // TODO: change this when type checking is implemented
    if (tk.get_type() != token::identifier && !token::is_keyword(tk.get_type())) {
        ErrorHandler::handle_missing(tk.get_src_loc(), "type");
        exit(EXIT_FAILURE);
    }

    ASTNode *decl_node = node_allocator.alloc();
    decl_node->set(
        ast::var_decl,
        token::identifier,
        nullptr
    );

    // get start_*
    decl_node->loc = tk.get_src_loc();
    
    // next
    consume();

    // expect ident
    if (tk.get_type() != token::identifier) {
        ErrorHandler::handle_missing(tk.get_src_loc(), "identifier");
        exit(EXIT_FAILURE);
    }

    decl_node->data = (void *)tk.get_identifier_str();

    decl_node->loc.copy_end(tk.get_src_loc());

    // next
    consume();

    return decl_node;
}

void Parser::parse_func_params(ASTNode *func, bool call) {

    // expects current token to be left paren,
    // so consume left paren
    consume();

    if (tk.get_type() == token::op_rightparen) {
        // no params/args
        consume();
        return;
    }

    // parse expr til right paren
    ASTNode *node;
    while (true) {

        // call expr args
        if (call) {
            // expect an expr
            node = parse_assignment();
        }

        // func decl params
        else {
            // expect a var decl
            // TODO: this func assumes that the current token is a type.
            //       need to make sure it is a type to catch syntax errors.
            node = parse_var_decl();
        }

        // no expr or decl was parsed
        if (node == nullptr) {
            // do nothing
        }

        // something was parsed
        else {
            func->list.push_back(node);
        }

        switch (tk.get_type()) {
            case token::op_comma:
                consume();
                break;
            case token::op_rightparen:
                // consume right paren
                consume();
                return;
            default:
                // no right paren
                ErrorHandler::handle_missing(tk.get_src_loc(), "')'");
                exit(EXIT_FAILURE);
        }
    }
}

ASTNode *Parser::parse_stmt() {
    
    ASTNode *node;
    ASTNode *temp;

    // used for decls
    std::string ident_str;

    switch (tk.get_type()) {

        case token::eof:
        
            node = nullptr;
            break;

        // scoped statement block
        case token::op_leftbrace:
        
            node = node_allocator.alloc();
            node->set(
                ast::node_type::stmt_block,
                tk.get_type(),
                (void *)tk.get_operator_str()
            );

            // cache loc, we will only use start_*
            node->loc = tk.get_src_loc();

            // consume left brace
            consume();

            // parse stmts
            while (true) {

                // found end brace
                if (tk.get_type() == token::op_rightbrace) {
                    break;
                }

                // reached eof without encountering end brace
                else if (tk.get_type() == token::eof) {
                    ErrorHandler::handle_missing(tk.get_src_loc(), "'}'");
                    exit(EXIT_FAILURE);
                }

                // statement
                node->list.push_back(parse_stmt());

            }

            // get loc end_*
            node->loc.copy_end(tk.get_src_loc());

            // consume right brace
            consume();

            break;

        // return statement
        case token::kw_return:

            node = node_allocator.alloc();
            node->set(
                ast::ret_stmt,
                tk.get_type(),
                (void *)tk.get_keyword_str()
            );
            
            // cache loc, we will only use start_*
            node->loc = tk.get_src_loc();

            // make curr token the start of expr
            consume();

            // parse expr until semi
            temp = parse_expr(token::op_semicolon);

            // get loc end_*
            node->loc.copy_end(prev_tk_loc);

            // consume semi
            consume();

            // check if there was an expr
            if (temp == nullptr) {
                ErrorHandler::handle_missing(tk.get_src_loc(), "expression");
                exit(EXIT_FAILURE);
            }

            // add expr to ret stmt
            node->list.push_back(temp);
            break;

        // primitive type decl
        // does not account for non-primitive types.
        // also does not parse types, simply consumes the keyword token
        // TODO: update this to work with parsed types
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
            node = node_allocator.alloc();

            // cache loc, we will only use start_*
            node->loc = tk.get_src_loc();

            // lex next -- expecting identifier
            consume();

            if (tk.get_type() != token::identifier) {
                ErrorHandler::handle_missing(tk.get_src_loc(), "identifier");
                exit(EXIT_FAILURE);
            }

            node->token_type = token::identifier;

            // grab identifier string
            node->data = (void *)tk.get_identifier_str();

            // lex next -- determines type of decl
            consume();

            // func decl
            if (tk.get_type() == token::op_leftparen) {

                // set node type
                node->type = ast::func_decl;

                // parse func decl params
                parse_func_params(node, false);

                // func decl
                if (tk.get_type() == token::op_semicolon) {

                    // get loc end_*
                    node->loc.copy_end(tk.get_src_loc());

                    // consume semi
                    consume();
                }

                // func decl with define
                else if (tk.get_type() == token::op_leftbrace) {

                    // already on left brace so recursive call will handle it
                    node->list.push_back(parse_stmt());

                    // get loc end_*
                    node->loc.copy_end(prev_tk_loc);
                }

                // unknown
                else {
                    ErrorHandler::handle_missing(tk.get_src_loc(), "';' or '{'");
                    exit(EXIT_FAILURE);
                }
            }

            // var decl
            else {
                
                // set node type
                node->type = ast::var_decl;

                // var decl with define
                if (tk.get_type() == token::op_equal) {

                    // make curr token the start of expr
                    consume();

                    // parse expr until semi
                    node->list.push_back(parse_expr(token::op_semicolon));
                }

                // var decl, no define
                else if (tk.get_type() == token::op_semicolon) {
                    // nothing to do
                }

                // unknown
                else {
                    ErrorHandler::handle_missing(tk.get_src_loc(), "';' or '='");
                    exit(EXIT_FAILURE);
                }

                // get loc end_*
                node->loc.copy_end(prev_tk_loc);

                // consume semi
                consume();

            }

            break;

        // assume that it is an expr by default
        default:

            // parse expr until semi
            node = parse_expr(token::op_semicolon);

            // consume semi
            consume();
            break;
    }

    return node;
}

/** ------------------- PUBLIC ------------------- */

Parser::Parser(Lexer &lexer, Allocator<ASTNode> &node_allocator)
    : lexer(lexer),
    node_allocator(node_allocator) {

    // initial lex so that tk contains the first token
    consume();
}

Parser::~Parser() {
    std::cout << "Parser destroyed." << std::endl;
}

ASTNode *Parser::parse() {
    //std::cout << "parse:START\n";
    ASTNode *n;
    ASTNode *tree = node_allocator.alloc();
    // TODO: translation unit IDs
    tree->data = (void *)"tu1";
    tree->token_type = token::unknown;
    tree->type = ast::translation_unit;
    // the translation unit gets the default (invalid) src loc
    tree->loc = SourceLocation();
    while ((n = parse_stmt()) != nullptr) {
        tree->list.push_back(n);
    }
    //std::cout << "  parse:DONE\n";
    return tree;
}
