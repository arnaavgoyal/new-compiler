#include "parser/parser.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "lexer/lexer.h"
#include "memory/allocator.h"
#include "diagnostic/diagnostic.h"
#include <iostream>
#include <stack>
#include <iomanip>
#include <algorithm>
#include "analyzer/analyzer.h"
#include "analyzer/op.h"
#include <setjmp.h>
#include <cassert>

//#define DEBUG

namespace fe {

ASTNode *Parser::make_node(
    ast::node_type kind,
    std::string *str,
    SourceLocation loc,
    token::token_type tok
) {
    ASTNode *node = node_allocator.alloc();
    node->set(kind, nullptr, str, loc, tok, false);
    return node;
}

void Parser::consume() {
    prev_tk_loc = tk.get_src_loc();
    lexer.lex(tk);
}

void Parser::fatal_abort() {
    longjmp(env, 1);
}

void Parser::match(token::token_type expected) {
    bool kw = false;

    if (!(kw = token::is_keyword(expected)) && !token::is_operator(expected)) {
        std::cout << "Parser::match() called with !op && !kw token type\n";
        return;
    }

    if (tk.get_type() == expected) {
        return;
    }
    
    DiagnosticHandler::make(diag::id::expected_token, tk.get_src_loc())
        .add(kw ? token::get_keyword_string(expected) : token::get_operator_string(expected))
        .finish();

    fatal_abort();
}

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

ASTNode *Parser::left_assoc_bin_op(
    ASTNode *(Parser::*higher_prec)(),
    std::vector<token::token_type> const &types,
    std::vector<op::kind> const &ops
) {
    // get result of left side
    ASTNode *lhs = (this->*higher_prec)();

    ASTNode *rhs;
    ASTNode *binop = lhs;
    SourceLocation op_loc;
    token::token_type op_type;
    op::kind op_kind;
    bool go = true;
    while (go) {
        auto it = std::find(types.begin(), types.end(), tk.get_type());
        if (it != types.end()) {

            // cache type and loc of op token
            op_type = tk.get_type();
            op_loc = tk.get_src_loc();

            // consume op token
            consume();

            // get result of right side (higher prec)
            rhs = (this->*higher_prec)();
            //op_loc.copy_end(tk.get_src_loc());

            // get the operation kind
            op_kind = ops[it - types.begin()];

            // get result of binary op
            binop = analyzer.analyze_binary_op_expr(op_kind, lhs, rhs, op_type, op_loc);
        }
        else {
            go = false;
        }
    }
    return binop;
}

ASTNode *Parser::right_assoc_bin_op(
    ASTNode *(Parser::*higher_prec)(),
    std::vector<token::token_type> const &types,
    std::vector<op::kind> const &ops
) {
    // get lhs result
    ASTNode *lhs = (this->*higher_prec)();
    
    ASTNode *rhs;
    ASTNode *binop = lhs;
    SourceLocation op_loc;
    token::token_type op_type;
    op::kind op_kind;
    auto it = std::find(types.begin(), types.end(), tk.get_type());
    if (it != types.end()) {

        // cache type and loc of op token
        op_type = tk.get_type();
        op_loc = tk.get_src_loc();

        // consume op token
        consume();

        // get result of rhs (same prec)
        rhs = right_assoc_bin_op(higher_prec, types, ops);
        //op_loc.copy_end(tk.get_src_loc());

        // get corresponding op
        op_kind = ops[it - types.begin()];

        // get result of binary op
        binop = analyzer.analyze_binary_op_expr(op_kind, lhs, rhs, op_type, op_loc);
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

ASTNode *Parser::parse_postfix(ASTNode *pre) {

    // Iterate over every postfix operator (if any)
    ASTNode *res = pre;
    SourceLocation op_loc;
    std::vector<ASTNode *> args;
    while (true) {
        switch (tk.get_type()) {

            // unary postfix operators
            case token::op_plusplus:

                // get result for postfix op
                res = analyzer.analyze_postfix_op_expr(
                    op::postincr,
                    tk.get_type(),
                    res,
                    tk.get_src_loc()
                );

                // consume op token
                consume();
                break;
            
            case token::op_minusminus:

                // get result for postfix op
                res = analyzer.analyze_postfix_op_expr(
                    op::postdecr,
                    tk.get_type(),
                    res,
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
                    res,
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

ASTNode *Parser::parse_prefix() {

    token::token_type type = tk.get_type();

    ASTNode *res;
    op::kind op;
    SourceLocation op_loc;
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
                &curr_scope,
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
            op = op::preincr;
            goto finally;
        case token::op_minusminus:
            op = op::predecr;
            goto finally;
        case token::op_minus:
            op = op::neg;
            goto finally;
        case token::op_exclamation:
            op = op::lnot;
            goto finally;
        case token::op_asterisk:
            op = op::indirect;
            goto finally;
        case token::op_amp:
            op = op::addr;
        finally:
            // save op token loc
            op_loc = tk.get_src_loc();

            // consume op token
            consume();

            // recursively descend
            res = parse_prefix();

            // get result for prefix op
            res = analyzer.analyze_prefix_op_expr(op, type, res, op_loc);
            break;

        // empty
        default:
            
            // error -- no expr
            DiagnosticHandler::make(diag::id::expected_expression, tk.get_src_loc())
                .finish();
            fatal_abort();
            break;
    }

    return res;
}

ASTNode *Parser::parse_cast() {

    // parse current expr
    ASTNode *expr = parse_prefix();

    // return if not cast expr
    if (tk.get_type() != token::kw_as) {
        return expr;
    }

    // new src loc for cast expr
    SourceLocation sloc = expr->loc;
    
    // consume 'as' token
    consume();

    // parse type
    Type *ty = parse_type();

    // update sloc
    sloc.copy_end(prev_tk_loc);

    // send to analyzer
    expr = analyzer.analyze_cast_expr(expr, ty, sloc);

    return expr;
}

ASTNode *Parser::parse_multiplicative() {
    static std::vector<token::token_type> types {
        token::op_asterisk,
        token::op_slash,
        token::op_percent
    };
    static std::vector<op::kind> ops {
        op::mult,
        op::div,
        op::mod
    };
    return left_assoc_bin_op(parse_cast, types, ops);
}

ASTNode *Parser::parse_additive() {
    static std::vector<token::token_type> types {
        token::op_plus,
        token::op_minus
    };
    static std::vector<op::kind> ops {
        op::add,
        op::sub
    };
    return left_assoc_bin_op(parse_multiplicative, types, ops);
}

ASTNode *Parser::parse_gl_relational() {
    static std::vector<token::token_type> types {
        token::op_greater,
        token::op_greaterequal,
        token::op_less,
        token::op_lessequal
    };
    static std::vector<op::kind> ops {
        op::gt,
        op::gte,
        op::lt,
        op::lte
    };
    return left_assoc_bin_op(parse_additive, types, ops);
}

ASTNode *Parser::parse_eq_relational() {
    static std::vector<token::token_type> types {
        token::op_equalequal,
        token::op_exclamationequal
    };
    static std::vector<op::kind> ops {
        op::eq,
        op::neq
    };
    return left_assoc_bin_op(parse_gl_relational, types, ops);
}

ASTNode *Parser::parse_logical_and() {
    static std::vector<token::token_type> types {
        token::op_ampamp
    };
    static std::vector<op::kind> ops {
        op::land
    };
    return left_assoc_bin_op(parse_eq_relational, types, ops);
}

ASTNode *Parser::parse_logical_or() {
    static std::vector<token::token_type> types {
        token::op_pipepipe
    };
    static std::vector<op::kind> ops {
        op::lor
    };
    return left_assoc_bin_op(parse_logical_and, types, ops);
}

ASTNode *Parser::parse_assignment() {
    static std::vector<token::token_type> types {
        token::op_equal
    };
    static std::vector<op::kind> ops {
        op::assign
    };
    return right_assoc_bin_op(parse_logical_or, types, ops);
}

ASTNode *Parser::parse_comma() {
    static std::vector<token::token_type> types {
        token::op_comma
    };
    static std::vector<op::kind> ops {
        op::group
    };
    return left_assoc_bin_op(parse_assignment, types, ops);
}

ASTNode *Parser::parse_expr() {
    ASTNode *node = parse_comma();
    return node;
}

std::vector<ASTNode *> Parser::parse_call_args() {

    std::vector<ASTNode *> args;

    // consume left paren
    consume();

    // no args
    if (tk.get_type() == token::op_rightparen) {
        
        // consume right paren
        consume();
        return args;
    }

    ASTNode *arg;
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
                match(token::op_rightparen);
        }
    }

    // consume right paren
    consume();

    return args;
}

Type *Parser::parse_type() {

    Type *type;
    Type *temp;
    std::vector<Type *> param_list;
    SourceLocation loc_cache;
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
                            match(token::op_rightparen);
                            break;
                    }
                }
            }

            // consume right paren
            consume();

            // parse return type
            temp = parse_type();

            // analyze func type
            type = analyzer.analyze_function_type(
                &curr_scope,
                param_list,
                temp,
                loc_cache,
                prev_tk_loc
            );

            break;

        // identifier
        case token::identifier:

            // get corresponding type
            type = analyzer.analyze_typename(
                &curr_scope,
                tk.get_identifier_str(),
                tk.get_src_loc()
            );

            // consume type
            consume();

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
                &curr_scope,
                temp,
                loc_cache
            );

            break;

        // array
        case token::op_leftbracket:

            // cache left bracket loc
            loc_cache = tk.get_src_loc();

            // consume left bracket
            consume();

            // expects right bracket
            match(token::op_rightbracket);

            // update bracket loc
            loc_cache.copy_end(tk.get_src_loc());

            // consume right bracket
            consume();

            // get type that this is array of
            temp = parse_type();

            // analyze
            type = analyzer.analyze_array_type(
                &curr_scope,
                temp,
                loc_cache
            );

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
                DiagnosticHandler::make(diag::id::expected_type, tk.get_src_loc())
                    .finish();
                fatal_abort();
            }

            break;
    }

    return type;
}

ASTNode *Parser::parse_var_decl() {

    Type *type;
    SourceLocation loc;

    // consume var keyword
    consume();

    // cache start loc
    loc = tk.get_src_loc();

    // expects to be on type
    // so parse type
    type = parse_type();

    std::string *ident = nullptr;
    bool err = false;

    // expects identifier
    if (tk.get_type() != token::identifier) {
        DiagnosticHandler::make(diag::id::decl_expected_identifier, tk.get_src_loc())
            .finish();
        fatal_abort();
        ident = new std::string("<error>");
        err = true;
    }

    // save identifier
    ident = tk.get_identifier_str();

    // save identifier loc
    SourceLocation ident_loc = tk.get_src_loc();

    // consume identifier
    consume();

    // TODO: handle arrays (length and/or definition)
    // handle definition
    if (tk.get_type() == token::op_equal) {

        // cache loc of '='
        SourceLocation eqloc = tk.get_src_loc();

        // consume equal
        consume();

        // parse expr
        ASTNode *rhs = parse_expr();

        // analyze var decl
        ASTNode *res = analyzer.analyze_var_decl(
            &curr_scope,
            type,
            ident,
            ident_loc,
            rhs,
            eqloc
        );
        if (err) res->has_error = true;

        return res;
    }

    // just declaration
    else {

        // analyze var decl
        return analyzer.analyze_var_decl(
            &curr_scope,
            type,
            ident,
            ident_loc
        );
    }
}

ASTNode *Parser::parse_func_decl() {

    Type *type;
    SourceLocation loc;

    // consume def keyword
    consume();

    // cache start loc
    loc = tk.get_src_loc();

    // expects to be on type
    // so parse type
    type = parse_type();

    // expects identifier
    if (tk.get_type() != token::identifier) {
        DiagnosticHandler::make(diag::id::decl_expected_identifier, tk.get_src_loc())
            .finish();
        fatal_abort();
    }

    // save identifier
    std::string *ident = tk.get_identifier_str();

    // save identifier loc
    SourceLocation ident_loc = tk.get_src_loc();

    // consume identifier
    consume();

    // handle param list
    //   1: ()
    //   2: (ident)
    //   3: (ident, ident, ... , ident)

    std::vector<std::pair<std::string *, SourceLocation>> params;

    // expect left paren
    match(token::op_leftparen);

    // save left paren loc
    SourceLocation lparen_loc = tk.get_src_loc();

    // consume left paren
    consume();

    bool go = tk.get_type() == token::op_rightparen ? false : true;
    while (go) {

        // expect identifier
        if (tk.get_type() != token::identifier) {
            DiagnosticHandler::make(diag::id::decl_expected_identifier, tk.get_src_loc())
                .finish();
            fatal_abort();
        }
        
        // add param to list
        params.push_back(std::make_pair(tk.get_identifier_str(), tk.get_src_loc()));
        
        // consume ident
        consume();

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
                go = false;
                break;
            
            // error - something else
            default:

                match(token::op_rightparen);
                break;
        }

    }

    // save right paren loc
    auto rparen_loc = tk.get_src_loc();

    // consume right paren
    consume();

    std::cout << "func decl " << *ident << " w/ type " << type->stringify() << std::endl;

    // analyze func decl
    ASTNode *func_decl = analyzer.analyze_func_decl(
        &curr_scope,
        type,
        ident,
        params,
        ident_loc,
        lparen_loc,
        rparen_loc
    );

    // expect left brace for definition
    match(token::op_leftbrace);

    // handle func definition start
    // this call enters new function scope
    analyzer.start_func_define(func_decl, tk.get_src_loc());

    // parse func body
    ASTNode *func_body = parse_stmt_block(false);
    func_decl->children.push_back(func_body);

    // handle func definition end
    // this call exits function scope
    analyzer.end_func_define(&curr_scope, tk.get_src_loc());

    return func_decl;
}

ASTNode *Parser::parse_type_decl() {
    assert(tk.get_type() == token::kw_type);

    // consume keyword
    consume();

    // expecting identifier
    if (tk.get_type() != token::identifier) {
        DiagnosticHandler::make(diag::id::decl_expected_identifier, tk.get_src_loc())
            .finish();
        fatal_abort();
    }

    // cache ident loc
    auto loc_cache = tk.get_src_loc();
    std::string *ident = tk.get_identifier_str();

    // consume identifier
    consume();

    // expecting '='
    match(token::op_equal);

    // consume '='
    consume();

    // parse type
    Type *temp = parse_type();

    // notify analyzer
    auto res = analyzer.analyze_type_alias(
        &curr_scope,
        temp,
        ident,
        loc_cache
    );

    return res;
}

ASTNode *Parser::parse_stmt_block(bool make_new_scope) {
    assert(tk.get_type() == token::op_leftbrace);

    // cache start loc
    auto stmt_block_start = tk.get_src_loc();

    // create node
    ASTNode *block = make_node(
        ast::stmt_block,
        nullptr,
        stmt_block_start,
        tk.get_type()
    );

    // consume left brace
    consume();

    if (make_new_scope) {
        analyzer.start_scoped_block(&curr_scope, stmt_block_start);
    }

    // parse stmts
    ASTNode *parsed = nullptr;
    while (true) {

        // found end brace
        if (tk.get_type() == token::op_rightbrace) {
            break;
        }

        // parse stmt
        parsed = parse_stmt();

        if (parsed) {
            block->children.push_back(parsed);
        }
        else {
            // reached eof without end brace
            match(token::op_rightbrace);
        }
    }

    block->loc.copy_end(tk.get_src_loc());

    // std::cout << "Dumping scope tables:\n";
    // curr_scope->dump(2);

    if (make_new_scope) {
        analyzer.end_scoped_block(&curr_scope, tk.get_src_loc());
    }

    // consume right brace
    consume();

    return block;
}

ASTNode *Parser::parse_stmt() {

    SourceLocation loc_cache;
    token::token_type tk_type = tk.get_type();
    ASTNode *res = nullptr;
    bool req_semi = true;

    if (tk_type == token::eof) {

        // skip semicolon check
        req_semi = false;
    }

    // scoped statement block
    else if (tk_type == token::op_leftbrace) {

        res = parse_stmt_block(true);

        // this is to skip the semicolon check
        req_semi = false;
    }

    // typedef
    else if (tk_type == token::kw_type) {

        // parse
        res = parse_type_decl();
    }

    // return statement
    else if (tk_type == token::kw_return) {
        
        // cache start loc
        loc_cache = tk.get_src_loc();

        // make curr token the start of expr
        consume();

        // parse expr
        ASTNode *temp = parse_expr();

        // analyze return stmt
        res = analyzer.analyze_return_stmt(temp);
    }

    // var decl
    else if (tk_type == token::kw_var) {

        // parse the declaration
        res = parse_var_decl();
    }

    // func decl
    else if (tk_type == token::kw_def) {

        // parse the declaration
        res = parse_func_decl();

        req_semi = false;
    }

    // if stmt
    else if (tk_type == token::kw_if) {
        loc_cache = tk.get_src_loc();
        consume();

        match(token::op_leftparen);
        consume();

        // cond
        ASTNode *cond = parse_expr();

        match(token::op_rightparen);
        consume();

        // get end loc of if stmt
        loc_cache.copy_end(prev_tk_loc);

        // analyze
        ASTNode *ifstmt = analyzer.analyze_if_stmt(cond, loc_cache);

        // match {
        match(token::op_leftbrace);

        // parse stmt block
        ASTNode *stmts = parse_stmt_block(true);

        // add stmt block to loop node
        ifstmt->children.push_back(stmts);

        // if has follow-up else block
        if (tk.get_type() == token::kw_else) {

            consume();

            ifstmt->children.push_back(parse_stmt());
        }

        res = ifstmt;

        req_semi = false;
    }

    // while loop
    else if (tk_type == token::kw_while) {

        // cache src loc
        loc_cache = tk.get_src_loc();
        consume();

        match(token::op_leftparen);
        consume();

        // expecting cond
        ASTNode *cond = parse_expr();

        // match and consume )
        match(token::op_rightparen);
        consume();

        // get end loc of loop stmt
        loc_cache.copy_end(prev_tk_loc);

        // analyze
        ASTNode *loop = analyzer.analyze_loop_stmt(cond, loc_cache);

        // match {
        match(token::op_leftbrace);

        // parse stmt block
        ASTNode *stmts = parse_stmt_block(true);

        // add stmt block to loop node
        loop->children.push_back(stmts);

        res = loop;

        req_semi = false;
    }

    // assume that it is an expr by default
    else {

        // parse expr
        res = parse_expr();
    }

    // require semicolon at end of stmt
    if (req_semi) {
        match(token::op_semicolon);
        consume();
    }

    return res;
}

bool Parser::parse_entry() {

    // top level scope is the translation unit
    tunit = node_allocator.alloc();
    tunit->set(
        ast::translation_unit,
        nullptr,
        nullptr,
        SourceLocation(),
        token::unknown,
        false
    );

    // setup jmp buf for syntax error
    if (setjmp(env)) {
        return false;
    }

    // parsing at global scope.
    // global scope is not an execution context, so the
    //   only valid statements at this point are declarations
    //   (vars, funcs, and types)

    bool done = false;
    token::token_type tkty;
    while (!done) {
        
        tkty = tk.get_type();

        // var decl
        if (tkty == token::kw_var) {

            // just parse and add the var decl
            tunit->children.push_back(parse_var_decl());

            // deal with semi
            match(token::op_semicolon);
            consume();
        }

        // func decl
        else if (tkty == token::kw_def) {

            // a func decl is the first valid execution context
            //   but the func decl parse function takes care of this.
            // so just parse and add...
            tunit->children.push_back(parse_func_decl());
        }

        // type decl
        else if (tkty == token::kw_type) {

            // parse and add the type decl
            tunit->children.push_back(parse_type_decl());

            // deal with semi
            match(token::op_semicolon);
            consume();
        }

        // end of program (eof)
        else if (tkty == token::eof) {

            // exit parsing
            break;
        }

        // unknown -- invalid
        else {
            
            // report error
            DiagnosticHandler::make(diag::id::expected_declaration_in_non_execution_context, tk.get_src_loc())
                .finish();

            // abort
            fatal_abort();
        }
    }

    return true;
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

    // init global scope
    curr_scope = analyzer.global_scope();
}

Parser::~Parser() {
#ifdef DEBUG
    std::cout << "Parser destroyed." << std::endl;
#endif
}

bool Parser::parse(ASTNode **ref) {

    // parse
    bool res = parse_entry();

    // return the ast
    *ref = tunit;

    return res;
}

}
