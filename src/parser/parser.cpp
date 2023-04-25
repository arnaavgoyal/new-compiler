#include "parser/parser.h"
#include "parser/ast.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "lexer/lexer.h"
#include "memory/allocator.h"
#include "error/error.h"
#include <iostream>
#include <iomanip>
#include <algorithm>
#include "analyzer/analyzer.h"
#include "parser/type.h"

//#define DEBUG

void Parser::consume() {
    prev_tk_loc = tk.get_src_loc();
    lexer.lex(tk);
}

ASTNode *Parser::recovery() {
    ASTNode *node = node_allocator.alloc();
    node->type = ast::recovery;
    node->loc = tk.get_src_loc();
    return node;
}

void Parser::skip_to(token::token_type type) {

    bool kw = false;

    if (!(kw = token::is_keyword(type)) && !token::is_operator(type)) {
        std::cout << "Parser::skip_to() called with !op && !kw token type\n";
        return;
    }

    while (tk.get_type() != type) {
        if (tk.get_type() == token::eof) {

            // fatal error
            std::string *msg = str_allocator.alloc();
            *msg += '\'';
            *msg += kw ? token::get_keyword_string(type) : token::get_operator_string(type);
            *msg += '\'';
            ErrorHandler::handle(error::missing, tk.get_src_loc(), msg->c_str());
            ErrorHandler::dump();
            exit(EXIT_FAILURE);
        }
        consume();
    }
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
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "expression");
                node = recovery();
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
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "expression");
            node = recovery();
        }
        n->loc.copy_end(node->loc);
        n->list.push_back(node);
        node = n;
    }
    return node;
}

void Parser::n_operand_op(
        ASTNode *(Parser::*higher_prec)(),
        std::vector<token::token_type> const &types,
        ASTNode *node
) {
    ASTNode *operand = (this->*higher_prec)();
    if (operand == nullptr) {
        return;
    }
    while (operand != nullptr) {
        node->list.push_back(operand);
        if (std::find(types.begin(), types.end(), tk.get_type()) != types.end()) {
            consume();
            operand = (this->*higher_prec)();
        }
        else {
            return;
        }
    }
    ErrorHandler::handle(error::missing, tk.get_src_loc(), "expression");
    node->list.push_back(recovery());
    return;
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

                // don't consume left paren
                // parse func call arguments
                parse_call_args(node);
                // this ^ call consumes the right paren

                // get end loc
                node->loc.copy_end(prev_tk_loc);
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
                j = parse_expr();
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
            node->list.push_back(parse_expr());

            // save end_* loc
            node->loc.copy_end(tk.get_src_loc());

            // consume right paren
            consume();

            // parse postfix
            node = parse_postfix(node);
            break;

        // identifier with no prefix operators
        case token::identifier:

            // identifier reference is not valid
            if (!analyzer.declared_in_any_scope(tk.get_identifier_str())) {

                ErrorHandler::handle(error::undeclared, tk.get_src_loc(), tk.get_identifier_str()->c_str());
                node = recovery();
            }

            // valid
            else {

                // create node for identifier
                node = node_allocator.alloc();
                node->set(
                    ast::ref_expr,
                    tk.get_type(),
                    (void *)tk.get_identifier_str()
                );
                node->loc = tk.get_src_loc();
            }

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

ASTNode *Parser::parse_comma() {
    static std::vector<token::token_type> types {
        token::op_comma
    };
    return left_assoc_bin_op(parse_assignment, types);
}

ASTNode *Parser::parse_expr() {
    ASTNode *node = parse_comma();
    return node;
}

void Parser::parse_call_args(ASTNode *node) {

    // consume left paren
    consume();

    // no args
    if (tk.get_type() == token::op_rightparen) {
        
        // consume right paren
        consume();
        return;
    }

    ASTNode *arg = nullptr;
    bool go = true;
    while (go) {

        // parse expr
        arg = parse_assignment();

        // expects an expression to have been parsed
        if (arg == nullptr) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "expression");
            arg = recovery();
            std::cout << "call arg - recovered: token: " << tk.get_print_str() << std::endl;
        }
        
        // add to node
        node->list.push_back(arg);

        // next action based on current token
        switch (tk.get_type()) {

            // another arg
            case token::op_comma:
                // consume comma
                consume();
                break;

            // no more args
            case token::op_rightparen:
                go = false;
                break;
            
            // error - something else
            default:
                std::cout << "call arg, unknown\n";
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "')'");
                //skip_to(token::op_rightparen);
                //break;
                return;
        }
    }

    // consume right paren (or unknown token)
    consume();
}

Type *Parser::parse_type() {

    Type *type = nullptr;
    Type *temp = nullptr;
    bool go;

    // expects curr token to be the first token of type
    switch (tk.get_type()) {

        // func
        case token::op_leftparen:

            // param list
            //   VALID:
            //   1        ()
            //   2        (<type>)
            //   3        (<type>, <type>, ... )
            //   INVALID:
            //   1        ( ..., <type>,)
            //   2        ( ...

            // make func type
            type = type_allocator.alloc();
            type->type = type::function_type;
            type->str = str_allocator.alloc();
            type->params = type_vec_allocator.alloc();

            // consume left paren
            *type->str += tk.get_operator_str();
            consume();

            // VALID 1
            if (tk.get_type() == token::op_rightparen) {
                // do nothing
            }

            // VALID 2 & 3
            else {

                go = true;
                while (go) {

                    // parse type
                    temp = parse_type();

                    // expects a type to have been found
                    if (temp == nullptr) {
                        ErrorHandler::handle(error::missing, tk.get_src_loc(), "type");
                        exit(EXIT_FAILURE);
                    }

                    // update string representation
                    *(std::string *)type->str += *temp->str;

                    // add to func type params list
                    type->params->push_back(temp);

                    // next action based on current token
                    switch (tk.get_type()) {

                        // another type in param list
                        case token::op_comma:

                            // consume comma
                            *type->str += tk.get_operator_str();
                            consume();
                            break;

                        // end of param list
                        case token::op_rightparen:

                            // consume right paren
                            go = false;
                            break;
                        
                        // error - something else
                        default:

                            ErrorHandler::handle(error::missing, tk.get_src_loc(), "')'");
                            exit(EXIT_FAILURE);
                            break;
                    }
                }
            }

            // consume right paren
            *type->str += tk.get_operator_str();
            consume();

            // return type
            temp = parse_type();

            // expects a type to have been parsed
            if (temp == nullptr) {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "type");
                exit(EXIT_FAILURE);
            }
            
            // update string representation
            *(std::string *)type->str += *temp->str;

            // set return type of func type
            type->returns = temp;

            break;

        // identifier
        case token::identifier:

            // get corresponding type, if it exists
            // TODO: fix const workaround
            type = (Type *)analyzer.get_type_by_string(tk.get_identifier_str());

            // check if type was found
            if (type == nullptr) {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "type");
                exit(EXIT_FAILURE);
            }

            // consume type
            consume();

            return type;

        // pointer
        case token::op_asterisk:

            // make pointer type
            type = type_allocator.alloc();
            type->str = str_allocator.alloc();
            type->type = type::pointer_type;

            // consume asterisk
            *type->str += tk.get_operator_str();
            consume();

            // get pointee type
            temp = parse_type();

            // expects a type to have been parsed
            if (temp == nullptr) {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "type");
                exit(EXIT_FAILURE);
            }

            // add to string representation
            *(std::string *)type->str += *temp->str;

            // set pointee type of pointer type
            type->returns = temp;

            break;

        // array
        case token::op_leftbracket:

            // make array type
            type = type_allocator.alloc();
            type->str = str_allocator.alloc();
            type->type = type::array_type;

            // consume left bracket
            *type->str += tk.get_operator_str();
            consume();

            // expects right bracket
            if (tk.get_type() != token::op_rightbracket) {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "']'");
                exit(EXIT_FAILURE);
            }

            // consume right bracket
            *type->str += tk.get_operator_str();
            consume();

            // get type that this is array of
            temp = parse_type();

            // expects a type to have been parsed
            if (temp == nullptr) {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "type");
                exit(EXIT_FAILURE);
            }

            // add to string representation
            *(std::string *)type->str += *temp->str;

            // set type this array is of
            type->array_of = temp;

            break;

        // anything else
        default:

            // primitive type
            if (token::is_primitive_type(tk.get_type())) {

                // parse primitive type
                type = (Type *)analyzer.get_type_by_string(tk.get_identifier_str());

                // consume prim type
                consume();
            }
            
            // no point in doing the lookup at the end of the function
            return type;
    }

    if ((temp = (Type *)analyzer.get_type_by_string(type->str)) != nullptr) {
        type = temp;
    }
    else {
        analyzer.add_type(type->str, type);
    }

    return type;

}

ASTNode *Parser::parse_decl() {

    ASTNode *node = nullptr;
    Type *type = nullptr;
    SourceLocation loc;
    bool func = false;

    // cache start loc
    loc = tk.get_src_loc();

    // expects to be on type
    // so parse type
    type = parse_type();

    // expects type to have been found
    if (type == nullptr) {
        ErrorHandler::handle(error::missing, tk.get_src_loc(), "type");
        return recovery();
    }

    // the first type type determines the type of decl
    if (type->type == type::function_type) {
        func = true;

        // 

        // func decls must have '=>' token
        if (tk.get_type() != token::op_equalgreater) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "'=>'");
            return recovery();
        }

        // consume '=>'
        consume();
    }

    // expects identifier
    if (tk.get_type() != token::identifier) {
        ErrorHandler::handle(error::missing, tk.get_src_loc(), "identifier");
        return recovery();
    }

    // make node
    node = node_allocator.alloc();
    node->type = func ? ast::func_decl : ast::var_decl;
    node->token_type = token::identifier;
    node->data = (void *)tk.get_identifier_str();
    loc.copy_end(tk.get_src_loc());
    node->loc = loc;

    // consume identifier
    consume();

    if (func) {
        // handle param list
        // TODO: implement
        //   param list vars have to be declared in function scope,
        //   but currently scope is entered after this function returns....
        consume();
        consume();
    }

    else {
        // handle array length
        // TODO: implement
    }


    // update semantic analyzer with decl
    analyzer.act_on_declaration(
        func ? symbol::func : symbol::var,
        (std::string *)node->data,
        type,
        loc
    );


    return node;
}

ASTNode *Parser::parse_stmt() {
    
    ASTNode *node;
    ASTNode *temp;
    std::string ident_str;
    token::token_type tk_type = tk.get_type();

    if (tk_type == token::eof) {

        // skip semicolon check
        return nullptr;
    }

    // scoped statement block
    else if (tk_type == token::op_leftbrace) {
    
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

        // enter new scope
        analyzer.enter_scope();

        // parse stmts
        while (true) {

            // found end brace
            if (tk.get_type() == token::op_rightbrace) {
                break;
            }

            // reached eof without encountering end brace
            else if (tk.get_type() == token::eof) {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "'}'");
                return recovery();
            }

            // statement
            node->list.push_back(parse_stmt());
        }

        // get loc end_*
        node->loc.copy_end(tk.get_src_loc());

        // consume right brace
        consume();

        // exit scope
        analyzer.exit_scope();

        // this is to skip the semicolon check
        return node;
    }

    // using
    else if (tk_type == token::kw_using) {

        node = node_allocator.alloc();

        // consume keyword
        consume();

        // expecting identifier
        if (tk.get_type() != token::identifier) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "identifier");
            skip_to(token::op_semicolon);
            consume();
            return recovery();
        }

        node->set(
            ast::typedef_stmt,
            tk.get_type(),
            (void *)tk.get_identifier_str()
        );
        node->loc = tk.get_src_loc();

        // consume identifier
        consume();

        // expecting '='
        if (tk.get_type() != token::op_equal) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "'='");
            return recovery();
        }

        // consume '='
        consume();

        // cache type loc
        SourceLocation tloc = tk.get_src_loc();

        // parse type
        Type *t = parse_type();

        // expecting type
        if (t == nullptr) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "type");
            return recovery();
        }

        // append "type" node to node
        temp = node_allocator.alloc();
        temp->set(
            ast::type,
            tk.get_type(),
            (void *)t->str
        );
        tloc.copy_end(prev_tk_loc);
        temp->loc = tloc;
        temp->token_type = token::identifier;
        node->list.push_back(temp);

        // notify analyzer
        analyzer.act_on_type_alias((std::string *)node->data, t, node->loc);
    }

    // return statement
    else if (tk_type == token::kw_return) {

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
        temp = parse_expr();

        // check if there was an expr
        if (temp == nullptr) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "expression");
            return recovery();
        }

        // get loc end_*
        node->loc.copy_end(prev_tk_loc);

        // add expr to ret stmt
        node->list.push_back(temp);
    }

    // decl
    else if (tk_type == token::kw_decl) {

        // consume decl keyword
        consume();

        // parse the decl stmt
        node = parse_decl();

        // handle post-decl for func
        if (node->type == ast::func_decl) {

            // handle definition
            if (tk.get_type() == token::op_leftbrace) {

                // parse compound stmt and append to node
                node->list.push_back(parse_stmt());

                // skip semicolon check
                return node;
            }

            // just declaration
            // TODO: make this allowed
            else if (tk.get_type() == token::op_semicolon) {
                ErrorHandler::handle(error::nyi, tk.get_src_loc(), "function prototyping");
                return recovery();
            }

            // other
            else {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "'{'");
                return recovery();
            }
        }

        // handle post-decl for var
        else {

            // handle potential definition
            if (tk.get_type() == token::op_equal) {
                
                // consume '='
                consume();

                // parse expr
                temp = parse_expr();

                // ensure expr was parsed
                if (temp == nullptr) {
                    ErrorHandler::handle(error::missing, tk.get_src_loc(), "expr");
                    return recovery();
                }
                
                // add expr to decl node
                node->list.push_back(temp);
            }
        }
    }

    // func (DEPRECATED for now)
    else if  (tk_type == token::kw_func) {
        ErrorHandler::handle(error::deprecated, tk.get_src_loc(), tk.get_keyword_str());
        node = recovery();
        skip_to(token::op_semicolon);
    }

    // assume that it is an expr by default
    else {

        // parse expr
        node = parse_expr();

        if (node == nullptr) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "expression");
            node = recovery();
        }
    }

    // require semicolon at end of stmt
    if (tk.get_type() == token::op_semicolon) {

        // consume semi
        consume();
    }
    else {
        ErrorHandler::handle(error::missing, tk.get_src_loc(), "';'");

        // known token
        if (tk.get_type() != token::unknown) {
            // do not consume
        }

        // unknown token
        else {
            consume();
        }
    }

    return node;
}

/** ------------------- PUBLIC ------------------- */

Parser::Parser(Lexer &lexer,
    SemanticAnalyzer &analyzer,
    Allocator<ASTNode> &node_allocator,
    Allocator<Type> &type_allocator,
    Allocator<std::string> &str_allocator,
    Allocator<std::vector<Type *>> &type_vec_allocator,
    std::vector<token::token_type> &primitives
) :
    lexer(lexer),
    analyzer(analyzer),
    node_allocator(node_allocator),
    type_allocator(type_allocator),
    str_allocator(str_allocator),
    type_vec_allocator(type_vec_allocator) {

    // add all primitive types
    Type *type;
    std::string *str;
    std::vector<token::token_type>::const_iterator start = primitives.begin();
    while (start != primitives.end()) {
        str = str_allocator.alloc();
        *str = std::string(token::get_keyword_string(*start));
        type = type_allocator.alloc();
        type->str = str;
        type->type = type::primitive_type;
        analyzer.add_type(str, type);
        start++;
    }

    // initial lex so that tk contains the first token
    consume();
}

Parser::~Parser() {
#ifdef DEBUG
    std::cout << "Parser destroyed." << std::endl;
#endif
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
