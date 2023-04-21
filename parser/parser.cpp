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

void Parser::consume() {
    prev_tk_loc = tk.get_src_loc();
    lexer.lex(tk);
}

ASTNode *Parser::parse_postfix(ASTNode *node) {

    //std::cout << "parse_postfix:START\n";

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
                parse_comma(token::op_rightparen, node);

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
                //std::cout << "  parse_postfix:DONE\n";
                return node;
        }
    }
}

ASTNode *Parser::parse_prefix() {
    //std::cout << "parse_prefix:START\n";
    //std::cout << tk.get_print_str() << std::endl;
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
    //std::cout << "  parse_prefix:DONE\n";
    return node;
}

ASTNode *Parser::parse_multiplicative() {
    //std::cout << "parse_mult:START\n";
    ASTNode *node = parse_prefix();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_asterisk:
            case token::op_slash:
            case token::op_percent:
                // bin mult op
                n = node_allocator.alloc();
                n->set(
                    ast::node_type::binary_op,
                    tk.get_type(),
                    (void *)tk.get_operator_str()
                );
                n->loc = node->loc;
                n->list.push_back(node);
                consume();
                node = parse_prefix();
                if (node == nullptr) {
                    ErrorHandler::handle_missing_expr(tk.get_src_loc());
                    exit(EXIT_FAILURE);
                }
                n->loc.copy_end(node->loc);
                n->list.push_back(node);
                node = n;
                break;

            default:
                // no bin mult ops left
                //std::cout << "  parse_mult:DONE\n";
                return node;
        }
    }
}

ASTNode *Parser::parse_additive() {
    //std::cout << "parse_add:START\n";
    ASTNode *node = parse_multiplicative();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_plus:
            case token::op_minus:
                // bin add op
                n = node_allocator.alloc();
                n->set(
                    ast::node_type::binary_op,
                    tk.get_type(),
                    (void *)tk.get_operator_str()
                );
                n->loc = node->loc;
                n->list.push_back(node);
                consume();
                node = parse_multiplicative();
                if (node == nullptr) {
                    ErrorHandler::handle_missing_expr(tk.get_src_loc());
                    exit(EXIT_FAILURE);
                }
                n->loc.copy_end(node->loc);
                n->list.push_back(node);
                node = n;
                break;

            default:
                // no bin add ops left
                //std::cout << "  parse_add:DONE\n";
                return node;
        }
    }
}

ASTNode *Parser::parse_gl_relational() {
    //std::cout << "parse_gl:START\n";
    ASTNode *node = parse_additive();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_greater:
            case token::op_greaterequal:
            case token::op_less:
            case token::op_lessequal:
                // bin relational op
                n = node_allocator.alloc();
                n->set(
                    ast::node_type::binary_op,
                    tk.get_type(),
                    (void *)tk.get_operator_str()
                );
                n->loc = node->loc;
                n->list.push_back(node);
                consume();
                node = parse_additive();
                if (node == nullptr) {
                    ErrorHandler::handle_missing_expr(tk.get_src_loc());
                    exit(EXIT_FAILURE);
                }
                n->loc.copy_end(node->loc);
                n->list.push_back(node);
                node = n;
                break;

            default:
                // no bin relational ops left
                return node;
        }
    }
}

ASTNode *Parser::parse_eq_relational() {
    //std::cout << "parse_eq:START\n";
    ASTNode *node = parse_gl_relational();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_equalequal:
            case token::op_exclamationequal:
                n = node_allocator.alloc();
                n->set(
                    ast::node_type::binary_op,
                    tk.get_type(),
                    (void *)tk.get_operator_str()
                );
                n->loc = node->loc;
                n->list.push_back(node);
                consume();
                node = parse_gl_relational();
                if (node == nullptr) {
                    ErrorHandler::handle_missing_expr(tk.get_src_loc());
                    exit(EXIT_FAILURE);
                }
                n->loc.copy_end(node->loc);
                n->list.push_back(node);
                node = n;
                break;

            default:
                // no bin relational ops left
                return node;
        }
    }
}

ASTNode *Parser::parse_logical_and() {
    //std::cout << "parse_and:START\n";
    ASTNode *node = parse_eq_relational();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_ampamp:
                n = node_allocator.alloc();
                n->set(
                    ast::node_type::binary_op,
                    tk.get_type(),
                    (void *)tk.get_operator_str()
                );
                n->loc = node->loc;
                n->list.push_back(node);
                consume();
                node = parse_eq_relational();
                if (node == nullptr) {
                    ErrorHandler::handle_missing_expr(tk.get_src_loc());
                    exit(EXIT_FAILURE);
                }
                n->loc.copy_end(node->loc);
                n->list.push_back(node);
                node = n;
                break;

            default:
                // none left
                return node;
        }
    }
}

ASTNode *Parser::parse_logical_or() {
    //std::cout << "parse_or:START\n";
    ASTNode *node = parse_logical_and();
    ASTNode *n;
    while (true) {
        switch (tk.get_type()) {
            case token::op_pipepipe:
                n = node_allocator.alloc();
                n->set(
                    ast::node_type::binary_op,
                    tk.get_type(),
                    (void *)tk.get_operator_str()
                );
                n->loc = node->loc;
                n->list.push_back(node);
                consume();
                node = parse_logical_and();
                if (node == nullptr) {
                    ErrorHandler::handle_missing_expr(tk.get_src_loc());
                    exit(EXIT_FAILURE);
                }
                n->loc.copy_end(node->loc);
                n->list.push_back(node);
                node = n;
                break;

            default:
                // none left
                return node;
        }
    }
}

ASTNode *Parser::parse_assignment() {
    //std::cout << "parse_assign:START\n";
    ASTNode *node = parse_logical_or();
    ASTNode *n;
    switch (tk.get_type()) {
        case token::op_equal:
            n = node_allocator.alloc();
            n->set(
                ast::node_type::binary_op,
                tk.get_type(),
                (void *)tk.get_operator_str()
            );
            n->loc = node->loc;
            n->list.push_back(node);
            consume();
            node = parse_assignment();
            if (node == nullptr) {
                ErrorHandler::handle_missing_expr(tk.get_src_loc());
                exit(EXIT_FAILURE);
            }
            n->loc.copy_end(node->loc);
            n->list.push_back(node);
            node = n;
            break;
        default:
            break;
    }
    return node;
}

ASTNode *Parser::parse_comma(token::token_type stop, ASTNode *cn) {
    //std::cout << "parse_comma:START\n";
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

        node = parse_assignment();

        ASTNode *n;
        bool br = true;
        while (br) {
            switch (tk.get_type()) {
                case token::op_comma:
                    n = node_allocator.alloc();
                    n->set(
                        ast::node_type::binary_op,
                        tk.get_type(),
                        (void *)tk.get_operator_str()
                    );
                    n->loc = node->loc;
                    n->list.push_back(node);
                    consume();
                    node = parse_assignment();
                    if (node == nullptr) {
                        ErrorHandler::handle_missing_expr(tk.get_src_loc());
                        exit(EXIT_FAILURE);
                    }
                    n->loc.copy_end(node->loc);
                    n->list.push_back(node);
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
        ErrorHandler::handle_missing_token(tk.get_src_loc(), token::get_token_string(stop));
        exit(EXIT_FAILURE);
    }

    return node;
}

ASTNode *Parser::parse_expr(token::token_type stop) {
    return parse_comma(stop, nullptr);
}

ASTNode *Parser::parse_var_decl() {
    // assumes that current token is a type
    // FOR NOW check if is ident or keyword
    // TODO: change this when type checking is implemented
    if (tk.get_type() != token::identifier && !token::is_keyword(tk.get_type())) {
        ErrorHandler::handle_missing_token(tk.get_src_loc(), "type");
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
        ErrorHandler::handle_missing_token(tk.get_src_loc(), "identifier");
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
                ErrorHandler::handle_missing_token(tk.get_src_loc(), "')'");
                exit(EXIT_FAILURE);
        }
    }
}

ASTNode *Parser::parse_stmt() {
    //std::cout << "parse_stmt:START\n";
    ASTNode *node;
    ASTNode *temp;

    // used for decls
    std::string ident_str;

    switch (tk.get_type()) {

        case token::eof:
            //std::cout << "parse_stmt:eof\n";
            node = nullptr;
            break;

        // scoped statement block
        case token::op_leftbrace:
            //std::cout << "parse_stmt:scoped block\n";
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
                    ErrorHandler::handle_missing_token(tk.get_src_loc(), "'}'");
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
            //std::cout << "parse_stmt:return\n";
            //std::cout << tk.get_keyword_str() << std::endl;
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
                ErrorHandler::handle_missing_expr(tk.get_src_loc());
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
            //std::cout << "parse_stmt:type decl\n";
        
            // generic decl -- type unknown so far
            node = node_allocator.alloc();

            // cache loc, we will only use start_*
            node->loc = tk.get_src_loc();

            // lex next -- expecting identifier
            consume();

            if (tk.get_type() != token::identifier) {
                ErrorHandler::handle_missing_token(tk.get_src_loc(), "identifier");
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
                    ErrorHandler::handle_missing_token(tk.get_src_loc(), "';' or '{'");
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
                    ErrorHandler::handle_missing_token(tk.get_src_loc(), "';' or '='");
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
            //std::cout << "parse_stmt:default expr\n";

            // parse expr until semi
            node = parse_expr(token::op_semicolon);

            // consume semi
            consume();
            break;
    }

    //std::cout << token::get_token_string(tk.get_type()) << std::endl;
    //std::cout << "  parse_stmt:DONE\n";
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
