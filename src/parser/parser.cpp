#include "parser/parser.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "lexer/lexer.h"
#include "memory/allocator.h"
#include "error/error.h"
#include <iostream>
#include <iomanip>
#include <algorithm>
#include "analyzer/analyzer.h"

//#define DEBUG

void Parser::consume() {
    prev_tk_loc = tk.get_src_loc();
    lexer.lex(tk);
}

// void Parser::match(token::token_type expected) {
//     bool kw = false;

//     if (!(kw = token::is_keyword(expected)) && !token::is_operator(expected)) {
//         std::cout << "Parser::match() called with !op && !kw token type\n";
//         return;
//     }

//     if (tk.get_type() == expected) {
//         return;
//     }

//     std::string *msg = str_allocator.alloc();
//     *msg += '\'';
//     *msg += kw ? token::get_keyword_string(expected) : token::get_operator_string(expected);
//     *msg += '\'';
//     ErrorHandler::handle(error::missing, tk.get_src_loc(), msg->c_str());
// }

// void Parser::skip_to(token::token_type type) {

//     bool kw = false;

//     if (!(kw = token::is_keyword(type)) && !token::is_operator(type)) {
//         std::cout << "Parser::skip_to() called with !op && !kw token type\n";
//         return;
//     }

//     while (tk.get_type() != type) {
//         if (tk.get_type() == token::eof) {

//             // fatal error
//             std::string *msg = str_allocator.alloc();
//             *msg += '\'';
//             *msg += kw ? token::get_keyword_string(type) : token::get_operator_string(type);
//             *msg += '\'';
//             ErrorHandler::handle(error::missing, tk.get_src_loc(), msg->c_str());
//             ErrorHandler::dump();
//             exit(EXIT_FAILURE);
//         }
//         consume();
//     }
// }

AnalyzedExpr Parser::left_assoc_bin_op(
    AnalyzedExpr (Parser::*higher_prec)(),
    std::vector<token::token_type> const &types
) {
    // get result of left side
    AnalyzedExpr lhs = (this->*higher_prec)();

    AnalyzedExpr rhs;
    AnalyzedExpr binop = lhs;
    SourceLocation op_loc;
    token::token_type op_type;
    bool go = true;
    while (go) {
        if (std::find(types.begin(), types.end(), tk.get_type()) != types.end()) {

            // cache type and loc of op token
            op_type = tk.get_type();
            op_loc = tk.get_src_loc();

            // consume op token
            consume();

            // get result of right side (higher prec)
            rhs = (this->*higher_prec)();

            // std::cout << "l_a_b_o lhs\n";
            // lhs.contents->print();
            // std::cout << "l_a_b_o rhs: " << rhs.contents->str << std::endl;
            // rhs.contents->print();

            // get result of binary op
            binop = analyzer.analyze_binary_op_expr(op_type, lhs, rhs, op_loc);

            // std::cout << "l_a_b_o final\n";
            // binop.contents->print();
        }
        else {
            go = false;
        }
    }
    return binop;
}

AnalyzedExpr Parser::right_assoc_bin_op(
    AnalyzedExpr (Parser::*higher_prec)(),
    std::vector<token::token_type> const &types
) {
    // get lhs result
    AnalyzedExpr lhs = (this->*higher_prec)();
    
    AnalyzedExpr rhs;
    AnalyzedExpr binop = lhs;
    SourceLocation op_loc;
    token::token_type op_type;
    if (std::find(types.begin(), types.end(), tk.get_type()) != types.end()) {

        // cache type and loc of op token
        op_type = tk.get_type();
        op_loc = tk.get_src_loc();

        // consume op token
        consume();

        // get result of rhs (same prec)
        rhs = right_assoc_bin_op(higher_prec, types);

        // get result of binary op
        binop = analyzer.analyze_binary_op_expr(op_type, lhs, rhs, op_loc);
    }
    return binop;
}

// void Parser::n_operand_op(
//         ASTNode *(Parser::*higher_prec)(),
//         std::vector<token::token_type> const &types,
//         ASTNode *node
// ) {
//     ASTNode *operand = (this->*higher_prec)();
//     if (operand == nullptr) {
//         return;
//     }
//     while (operand != nullptr) {
//         node->list.push_back(operand);
//         if (std::find(types.begin(), types.end(), tk.get_type()) != types.end()) {
//             consume();
//             operand = (this->*higher_prec)();
//         }
//         else {
//             return;
//         }
//     }
//     // parse error
//     ErrorHandler::handle(error::missing, tk.get_src_loc(), "expression");
//     ErrorHandler::prog_exit();
//     return;
// }

AnalyzedExpr Parser::parse_postfix(AnalyzedExpr pre) {

    // Iterate over every postfix operator (if any)
    AnalyzedExpr res = pre;
    SourceLocation op_loc;
    std::vector<AnalyzedExpr> args;
    while (true) {
        switch (tk.get_type()) {

            // unary postfix operators
            case token::op_plusplus:
            case token::op_minusminus:

                // get result for postfix op
                res = analyzer.analyze_postfix_op_expr(
                    tk.get_type(),
                    pre,
                    tk.get_src_loc()
                );

                // consume op token
                consume();
                break;
            
            // func call
            case token::op_leftparen:

                // cache call start loc
                op_loc = tk.get_src_loc();

                // parse call args
                args = parse_call_args();

                // get result of call expr
                res = analyzer.analyze_call_expr(
                    pre,
                    args,
                    op_loc,
                    prev_tk_loc
                );
                break;

            // array subscript
            case token::op_leftbracket:
                
                // save op loc
                op_loc = tk.get_src_loc();

                // consume left bracket
                consume();

                // get result for expr inside brackets
                res = parse_expr();

                // get result for array subscript
                res = analyzer.analyze_subscript_expr(
                    pre,
                    res,
                    op_loc,
                    tk.get_src_loc()
                );

                // consume right bracket
                consume();
                break;

            // no postfix operators left
            default:
                return res;
        }
    }
}

AnalyzedExpr Parser::parse_prefix() {

    token::token_type type = tk.get_type();

    AnalyzedExpr res;
    SourceLocation op_loc;
    token::token_type op_type;
    switch (type) {

        // parenthesized expr
        case token::op_leftparen:

            // save left paren loc
            op_loc = tk.get_src_loc();

            // consume left paren
            consume();

            // parse expr
            res = parse_expr();

            // copy end loc
            op_loc.copy_end(tk.get_src_loc());

            // get result
            res = analyzer.analyze_paren_expr(res, op_loc);

            // consume right paren
            consume();

            // parse postfix
            res = parse_postfix(res);
            break;

        // identifier with no prefix operators
        case token::identifier:

            // get result for reference
            res = analyzer.analyze_ref_expr(
                tk.get_identifier_str(),
                tk.get_src_loc()
            );

            // consume identifier
            consume();

            // parse postfix operators
            res = parse_postfix(res);
            break;

        // char literal
        case token::character_literal:

            // get result for char lit
            res = analyzer.analyze_character_literal(
                tk.get_literal_str(),
                tk.get_src_loc()
            );

            // next
            consume();
            break;
        
        // numeric literal
        case token::numeric_literal:

            // get result for num lit
            res = analyzer.analyze_numeric_literal(
                tk.get_literal_str(),
                tk.get_src_loc()
            );

            // next
            consume();
            break;

        // string literal
        case token::string_literal:

            // get result for char lit
            res = analyzer.analyze_string_literal(
                tk.get_literal_str(),
                tk.get_src_loc()
            );

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

            // save op token loc
            op_loc = tk.get_src_loc();

            // consume op token
            consume();

            // recursively descend
            res = parse_prefix();

            // get result for prefix op
            res = analyzer.analyze_prefix_op_expr(type, res, op_loc);
            break;

        // empty
        default:
            
            // error -- no expr
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "expression");
            ErrorHandler::prog_exit();
            break;
    }

    return res;
}

AnalyzedExpr Parser::parse_multiplicative() {
    static std::vector<token::token_type> types {
        token::op_asterisk,
        token::op_slash,
        token::op_percent
    };
    return left_assoc_bin_op(parse_prefix, types);
}

AnalyzedExpr Parser::parse_additive() {
    static std::vector<token::token_type> types {
        token::op_plus,
        token::op_minus
    };
    return left_assoc_bin_op(parse_multiplicative, types);
}

AnalyzedExpr Parser::parse_gl_relational() {
    static std::vector<token::token_type> types {
        token::op_greater,
        token::op_greaterequal,
        token::op_less,
        token::op_lessequal
    };
    return left_assoc_bin_op(parse_additive, types);
}

AnalyzedExpr Parser::parse_eq_relational() {
    static std::vector<token::token_type> types {
        token::op_equalequal,
        token::op_exclamationequal
    };
    return left_assoc_bin_op(parse_gl_relational, types);
}

AnalyzedExpr Parser::parse_logical_and() {
    static std::vector<token::token_type> types {
        token::op_ampamp
    };
    return left_assoc_bin_op(parse_eq_relational, types);
}

AnalyzedExpr Parser::parse_logical_or() {
    static std::vector<token::token_type> types {
        token::op_pipepipe
    };
    return left_assoc_bin_op(parse_logical_and, types);
}

AnalyzedExpr Parser::parse_assignment() {
    static std::vector<token::token_type> types {
        token::op_equal
    };
    return right_assoc_bin_op(parse_logical_or, types);
}

AnalyzedExpr Parser::parse_comma() {
    static std::vector<token::token_type> types {
        token::op_comma
    };
    return left_assoc_bin_op(parse_assignment, types);
}

AnalyzedExpr Parser::parse_expr() {
    AnalyzedExpr node = parse_comma();
    return node;
}

std::vector<AnalyzedExpr> Parser::parse_call_args() {

    std::vector<AnalyzedExpr> args;

    // consume left paren
    consume();

    // no args
    if (tk.get_type() == token::op_rightparen) {
        
        // consume right paren
        consume();
        return args;
    }

    AnalyzedExpr arg;
    bool go = true;
    while (go) {

        // parse expr
        arg = parse_assignment();
        
        // add to node
        args.push_back(arg);

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
                ErrorHandler::prog_exit();
        }
    }

    // consume right paren
    consume();

    return args;
}

AnalyzedType Parser::parse_type() {

    AnalyzedType type;
    AnalyzedType temp;
    std::vector<AnalyzedType> param_list;
    SourceLocation loc_cache;
    bool go;

    std::cout << "beginning type parse\n";

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

            // cache start loc
            loc_cache = tk.get_src_loc();

            // consume left paren
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

                    // add to func type params list
                    param_list.push_back(temp);

                    // next action based on current token
                    switch (tk.get_type()) {

                        // another type in param list
                        case token::op_comma:

                            // consume comma
                            consume();
                            break;

                        // end of param list
                        case token::op_rightparen:

                            // break loop
                            go = false;
                            break;
                        
                        // error - something else
                        default:
                            
                            // parse error
                            ErrorHandler::handle(error::missing, tk.get_src_loc(), "')'");
                            ErrorHandler::prog_exit();
                            break;
                    }
                }
            }

            // consume right paren
            consume();

            // parse return type
            temp = parse_type();

            std::cout << "got return type\n";

            // analyze func type
            type = analyzer.analyze_function_type(
                param_list,
                temp,
                loc_cache,
                prev_tk_loc
            );

            std::cout << "finished func type parse\n";

            break;

        // identifier
        case token::identifier:

            // get corresponding type
            type = analyzer.analyze_typename(
                tk.get_identifier_str(),
                tk.get_src_loc()
            );

            // consume type
            consume();

            std::cout << "finished typename parse\n";

            break;

        // pointer
        case token::op_asterisk:

            // cache asterisk loc
            loc_cache = tk.get_src_loc();

            // consume asterisk
            consume();

            // get pointee type
            temp = parse_type();

            // analyze
            type = analyzer.analyze_pointer_type(
                temp,
                loc_cache
            );

            std::cout << "finished pointer type parse\n";

            break;

        // array
        case token::op_leftbracket:

            // cache left bracket loc
            loc_cache = tk.get_src_loc();

            // consume left bracket
            consume();

            // expects right bracket
            if (tk.get_type() != token::op_rightbracket) {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "']'");
                ErrorHandler::prog_exit();
            }

            // update bracket loc
            loc_cache.copy_end(tk.get_src_loc());

            // consume right bracket
            consume();

            // get type that this is array of
            temp = parse_type();

            // analyze
            type = analyzer.analyze_array_type(
                temp,
                loc_cache
            );

            std::cout << "finished array type parse\n";

            break;

        // anything else
        default:

            // primitive type
            if (token::is_primitive_type(tk.get_type())) {

                // parse primitive type
                type = analyzer.analyze_primitive_type(
                    tk.get_type(),
                    tk.get_src_loc()
                );

                // consume prim type
                consume();
            }

            // error type
            else {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "type");
                ErrorHandler::prog_exit();
            }

            std::cout << "finished prim type parse\n";

            break;
    }

    return type;
}

void Parser::parse_decl() {

    AnalyzedType type;
    SourceLocation loc;

    // consume decl keyword
    consume();

    // cache start loc
    loc = tk.get_src_loc();

    std::cout << "before decl type parse\n";

    // expects to be on type
    // so parse type
    type = parse_type();

    std::cout << "after decl type parse\n";

    // expects identifier
    if (tk.get_type() != token::identifier) {
        ErrorHandler::handle(error::missing, tk.get_src_loc(), "identifier");
        ErrorHandler::prog_exit();
    }

    // save identifier
    std::string const *ident = tk.get_identifier_str();

    // save identifier loc
    SourceLocation ident_loc = tk.get_src_loc();

    // consume identifier
    consume();

    std::cout << "past decl ident parse\n";

    // func decl
    if (tk.get_type() == token::op_leftparen) {

        // handle param list
        //   1: ()
        //   2: (ident)
        //   3: (ident, ident, ... , ident)

        std::vector<std::pair<std::string const *, SourceLocation>> params;

        // expect left paren
        if (tk.get_type() != token::op_leftparen) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "'('");
            ErrorHandler::prog_exit();
        }

        // save left paren loc
        SourceLocation lparen_loc = tk.get_src_loc();

        // consume left paren
        consume();

        bool go = tk.get_type() == token::op_rightparen ? false : true;
        while (go) {

            // expect identifier
            if (tk.get_type() != token::identifier) {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "identifier");
                ErrorHandler::prog_exit();
            }

            else {

                // add param to list
                params.push_back(std::make_pair(tk.get_identifier_str(), tk.get_src_loc()));

                // consume ident
                consume();
            }

            // next action based on current token
            switch (tk.get_type()) {

                // another type in param list
                case token::op_comma:
                    // consume comma
                    consume();
                    break;

                // end of param list
                case token::op_rightparen:
                    // consume right paren
                    std::cout << "RIGHT PAREN" << std::endl;
                    go = false;
                    break;
                
                // error - something else
                default:
                    ErrorHandler::handle(error::missing, tk.get_src_loc(), "')'");
                    ErrorHandler::prog_exit();
                    break;
            }
            std::cout << tk.get_print_str() << std::endl;
        }

        // save right paren loc
        lparen_loc.copy_end(tk.get_src_loc());

        // consume right paren
        consume();

        // analyze func decl
        AnalyzedStmt func_decl = analyzer.analyze_func_decl(
            type,
            ident,
            params,
            ident_loc,
            lparen_loc
        );

        // expect left brace for definition
        if (tk.get_type() != token::op_leftbrace) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "'{'");
            ErrorHandler::prog_exit();
        }

        // handle func definition start
        // this call enters new function scope
        analyzer.start_func_define(func_decl, tk.get_src_loc());

        // consume left brace
        consume();

        // parse func body
        while (tk.get_type() != token::op_rightbrace) {
            if (tk.get_type() == token::eof) {
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "'}'");
                ErrorHandler::prog_exit();
            }
            parse_stmt();
        }

        // handle func definition end
        // this call exits function scope
        analyzer.end_func_define(tk.get_src_loc());

        // consume right brace
        consume();

        std::cout << "finished func define parse\n";
    }

    // var decl
    else {
        // TODO: handle arrays (length and/or definition)

        // handle definition
        if (tk.get_type() == token::op_equal) {
            std::cout << "starting var define parse\n";

            // cache loc of '='
            SourceLocation eqloc = tk.get_src_loc();

            // consume equal
            consume();

            // parse expr
            AnalyzedExpr rhs = parse_expr();

            // analyze var decl
            analyzer.analyze_var_decl(
                type,
                ident,
                ident_loc,
                rhs,
                eqloc
            );
            std::cout << "finished var define parse\n";
        }

        // just declaration
        else {
            // analyze var decl
            analyzer.analyze_var_decl(
                type,
                ident,
                ident_loc
            );
        }
    }
}

bool Parser::parse_stmt() {

    SourceLocation loc_cache;
    token::token_type tk_type = tk.get_type();

    std::cout << tk.get_print_str() << std::endl;

    if (tk_type == token::eof) {

        // skip semicolon check
        return true;
    }

    // scoped statement block
    else if (tk_type == token::op_leftbrace) {

        // cache start loc
        loc_cache = tk.get_src_loc();

        // consume left brace
        consume();

        // start scoped block
        analyzer.start_scoped_block(prev_tk_loc);

        // parse stmts
        while (true) {

            // found end brace
            if (tk.get_type() == token::op_rightbrace) {
                break;
            }

            // parse stmt
            if (parse_stmt()) {
                // reached eof without end brace
                ErrorHandler::handle(error::missing, tk.get_src_loc(), "'}'");
                ErrorHandler::prog_exit();
            }
        }

        // consume right brace
        consume();

        // end scoped block
        analyzer.end_scoped_block(prev_tk_loc);

        // this is to skip the semicolon check
        return false;
    }

    // using
    else if (tk_type == token::kw_using) {

        // consume keyword
        consume();

        // expecting identifier
        if (tk.get_type() != token::identifier) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "identifier");
            ErrorHandler::prog_exit();
        }

        // cache ident loc
        loc_cache = tk.get_src_loc();

        std::string const *ident = tk.get_identifier_str();

        // consume identifier
        consume();

        // expecting '='
        if (tk.get_type() != token::op_equal) {
            ErrorHandler::handle(error::missing, tk.get_src_loc(), "'='");
            ErrorHandler::prog_exit();
        }

        // consume '='
        consume();

        // parse type
        AnalyzedType temp = parse_type();

        // notify analyzer
        analyzer.analyze_type_alias(
            temp,
            ident,
            loc_cache
        );
    }

    // return statement
    else if (tk_type == token::kw_return) {
        
        // cache start loc
        loc_cache = tk.get_src_loc();

        // make curr token the start of expr
        consume();

        // parse expr
        AnalyzedExpr temp = parse_expr();

        // analyze return stmt
        analyzer.analyze_return_stmt(temp);
    }

    // decl
    else if (tk_type == token::kw_decl) {

        // parse the declaration
        parse_decl();
    }

    // func (DEPRECATED for now)
    else if  (tk_type == token::kw_func) {
        ErrorHandler::handle(error::deprecated, tk.get_src_loc(), tk.get_keyword_str());
        ErrorHandler::prog_exit();
    }

    // assume that it is an expr by default
    else {

        // parse expr
        AnalyzedExpr temp = parse_expr();

        // add to current scope
        analyzer.add_expr_as_stmt(temp);
    }

    // require semicolon at end of stmt
    if (tk.get_type() == token::op_semicolon) {

        // consume semi
        consume();
    }
    else {
        ErrorHandler::handle(error::missing, tk.get_src_loc(), "';'");
        ErrorHandler::prog_exit();
    }

    return tk.get_type() == token::eof;
}

/** ------------------- PUBLIC ------------------- */

Parser::Parser(
    Lexer &lexer,
    SemanticAnalyzer &analyzer
) :
    lexer(lexer),
    analyzer(analyzer) {

    // initial lex so that tk contains the first token
    consume();
}

Parser::~Parser() {
#ifdef DEBUG
    std::cout << "Parser destroyed." << std::endl;
#endif
}

void Parser::parse() {
    //std::cout << "parse:START\n";
    while (!parse_stmt()) { }
    //std::cout << "  parse:DONE\n";
}
