#include <algorithm>
#include <cassert>
#include <iomanip>
#include <iostream>
#include <setjmp.h>
#include <stack>

#include "ast/op.h"
#include "ast/xast.h"
#include "diag/diagnostic.h"
#include "lexer/lexer.h"
#include "utils/memory.h"

#include "parser/parser.h"

struct binop {
    fe::op::kind kind;
    token::token_type tok;
    int left;
    int right;
};

std::optional<binop> get_bp(token::token_type t) {
    switch (t) {

#define BINOP(name, token, left_bp, right_bp) \
        case token: return binop{ name, token, left_bp, right_bp };
#include "parser/opmap"
#undef BINOP

        default: break;
    }
    return std::nullopt;
}

namespace fe {

xast::Node *Parser::make_node(
    xast::nk kind,
    xast::NodeData p,
    SourceLocation loc,
    token::token_type
) {
    xast::Node *node = nodes(kind);
    node->data = p;
    node->sloc = loc;
    return node;
}

void Parser::consume() {
    prev_tk_loc = tk.sloc;
    tk = lexer.next();
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
    if (tk.type == expected) {
        return;
    }
    DiagnosticHandler::make(diag::id::expected_token, prev_tk_loc ^ tk.sloc)
        .add(kw ? token::get_keyword_string(expected) : token::get_operator_string(expected))
        .finish();
    fatal_abort();
}

xast::Node *Parser::parse_postfix(xast::Node *pre) {
    xast::Node *res = pre;
    SourceLocation op_loc;
    while (true) {
        op_loc = tk.sloc;
        auto type = tk.type;
        switch (tk.type) {
            case token::op_plusplus:
            case token::op_minusminus: {
                xast::Node *unary = make_node(xast::nk::unary_op, {}, pre->sloc >> op_loc, tk.type);
                unary->data = {
                    (tk.type == token::op_plusplus) ? op::postincr : op::postdecr
                    , op_loc
                    , tk.type
                };
                xast::c::unary_op::operand(unary) = res;
                res = unary;
                consume();
                break;
            }
            case token::op_leftparen: {
                xast::Node *call = make_node(xast::nk::call, {}, res->sloc, tk.type);
                consume();
                xast::c::call::callable(call) = res;
                if (tk.type != token::op_rightparen) {
                    xast::c::call::args(call) = parse_comma(token::op_rightparen);
                }
                match(token::op_rightparen);
                call->sloc >>= tk.sloc;
                consume();
                res = call;
                break;
            }
            case token::op_leftbracket: {
                xast::Node *subscript = make_node(xast::nk::subscript, {}, op_loc, tk.type);
                consume();
                xast::Node *sub_expr = parse_expr();
                xast::c::subscript::array(subscript) = res;
                xast::c::subscript::index(subscript) = sub_expr;
                res = subscript;
                consume();
                break;
            }
            case token::op_exclamation: {
                // exact same as call expr, just with the meta flag set
                consume();
                match(token::op_leftparen);
                xast::Node *call = make_node(xast::nk::call, {}, res->sloc, tk.type);
                consume();
                xast::c::call::callable(call) = res;
                xast::c::call::args(call) = parse_comma(token::op_rightparen);
                match(token::op_rightparen);
                call->sloc >>= tk.sloc;
                consume();
                call->meta = true;
                res = call;
                break;
            }
            case token::kw_as: {
                op_loc = tk.sloc;
                consume();
                xast::Node *target_expr = parse_unary();
                xast::Node *cast_node = make_node(xast::nk::cast, {}, res->sloc >> target_expr->sloc, token::kw_as);
                xast::c::cast::expr(cast_node) = res;
                xast::c::cast::type(cast_node) = target_expr;
                res = cast_node;
                break;
            }
            case token::op_minusgreater: {
                auto opnode = make_node(xast::nk::binary_op, {}, res->sloc, tk.type);
                opnode->data = { op::arrow, tk.sloc, tk.type };
                consume();
                auto rhs = parse_unary();
                xast::c::binary_op::lhs(opnode) = res;
                xast::c::binary_op::rhs(opnode) = rhs;
                opnode->sloc >>= rhs->sloc;
                res = opnode;
                break;
            }
            case token::op_dot: {
                auto opnode = make_node(xast::nk::binary_op, {}, res->sloc, tk.type);
                opnode->data = { op::dot, tk.sloc, tk.type };
                consume();
                if (tk.type != token::identifier) {
                    DiagnosticHandler::make(diag::id::decl_expected_identifier, tk.sloc)
                        .finish();
                    fatal_abort();
                }
                auto inner = parse_ident();
                xast::c::binary_op::lhs(opnode) = res;
                xast::c::binary_op::rhs(opnode) = inner;
                opnode->sloc >>= inner->sloc;
                res = opnode;
                break;
            }
            default:
                return res;
        }
    }
}

xast::Node *Parser::parse_ident() {
    assert(tk.type == token::identifier);
    auto res = make_node(xast::nk::ref, Identifier{ tk.str, tk.sloc }, tk.sloc, token::identifier);
    consume();
    return parse_postfix(res);
}

xast::Node *Parser::parse_unary() {
    return parse_prefix();
}

xast::Node *Parser::parse_prefix() {
    token::token_type type = tk.type;
    xast::Node *res;
    op::kind op_kind;
    SourceLocation op_loc;
    switch (type) {
        case token::op_leftparen: {
            xast::Node *paren = make_node(xast::nk::paren_expr, {}, tk.sloc, token::op_leftparen);
            consume();
            xast::c::paren_expr::inner(paren) = parse_expr();
            paren->sloc >>= tk.sloc;
            consume();
            res = parse_postfix(paren);
            break;
        }
        case token::identifier: {
            res = parse_ident();
            break;
        }
        case token::kw_type: {
            res = make_node(
                xast::nk::ref,
                Identifier{ token::get_keyword_string(token::kw_type), tk.sloc },
                tk.sloc,
                token::kw_type
            );
            consume();
            break;
        }
        case token::character_literal: {
            res = make_node(xast::nk::char_lit, tk.ival, tk.sloc, token::character_literal);
            consume();
            break;
        }
        case token::numeric_literal: {
            res = make_node(xast::nk::int_lit, tk.ival, tk.sloc, token::numeric_literal);
            consume();
            break;
        }
        case token::string_literal: {
            res = make_node(xast::nk::str_lit, Identifier{ tk.str, tk.sloc }, tk.sloc, token::string_literal);
            consume();
            break;
        }
        case token::op_lrbracket: {
            op_loc = tk.sloc;
            consume();
            xast::Node *element = parse_unary();
            xast::Node *array_expr = make_node(xast::nk::unary_op, {},
                op_loc >> element->sloc, token::op_leftbracket);
            array_expr->data = { op::slice, op_loc, token::op_lrbracket };
            xast::c::unary_op::operand(array_expr) = element;
            res = array_expr;
            break;
        }
        case token::op_plusplus:
            op_kind = op::preincr;
            goto finally;
        case token::op_minusminus:
            op_kind = op::predecr;
            goto finally;
        case token::op_minus:
            op_kind = op::neg;
            goto finally;
        case token::op_exclamation:
            op_kind = op::lnot;
            goto finally;
        case token::op_asterisk:
            op_kind = op::indirect;
            goto finally;
        case token::op_amp:
            op_kind = op::addr;
        finally: {
            op_loc = tk.sloc;
            consume();
            xast::Node *operand = parse_unary();
            xast::Node *unary = make_node(
                xast::nk::unary_op
                , {}
                , op_loc >> operand->sloc
                , type
            );
            unary->data = { op_kind, op_loc, type };
            xast::c::unary_op::operand(unary) = operand;
            res = unary;
            break;
        }
        default:
            DiagnosticHandler::make(diag::id::expected_expression, prev_tk_loc ^ tk.sloc)
                .finish();
            fatal_abort();
            break;
    }
    return res;
}

xast::Node *Parser::parse_binop(int minbp) {

    xast::Node *lhs = parse_prefix();
    lhs = parse_postfix(lhs);
    while (true) {

        auto bp = get_bp(tk.type);
        if (!bp || bp->left < minbp) break;

        auto loc = tk.sloc;
        consume();
        auto rhs = parse_binop(bp->right);
        auto binop = make_node(xast::nk::binary_op, {},
            lhs->sloc >> rhs->sloc, bp->tok);
        binop->data = { bp->kind, loc, bp->tok };
        xast::c::binary_op::lhs(binop) = lhs;
        xast::c::binary_op::rhs(binop) = rhs;
        lhs = binop;
    }

    return lhs;
}

xast::Node *Parser::parse_branch() {
    assert(tk.type == token::kw_if);

    auto loc = tk.sloc;
    consume();

    xast::Node *ifstmt = make_node(xast::nk::branch, {}, loc >> tk.sloc, token::kw_if);

    // Optional "!"
    if (tk.type == token::op_exclamation) {
        ifstmt->meta = true;
        consume();
    }

    xast::Node *cond = parse_binop();
    ifstmt->add(cond);
    xast::c::branch::cond(ifstmt) = cond;
    
    match(token::op_leftbrace);
    
    xast::Node *stmts = parse_stmt_block(true);
    xast::c::branch::then(ifstmt) = stmts;

    if (tk.type == token::kw_else) {
        consume();
        if (tk.type == token::kw_if) {
            xast::c::branch::else_(ifstmt) = parse_branch();
        }
        else {
            match(token::op_leftbrace);
            xast::c::branch::else_(ifstmt) = parse_stmt_block(true);
        }
    }

    return ifstmt;
}

xast::Node *Parser::parse_loop() {
    assert(tk.type == token::kw_while);

    auto loc = tk.sloc;
    consume();

    xast::Node *loop = make_node(xast::nk::loop, {}, loc >> tk.sloc, token::kw_while);
    
    if (tk.type == token::op_exclamation) {
        loop->meta = true;
        consume();
    }

    xast::Node *cond = parse_binop();
    xast::c::loop::cond(loop) = cond;
    
    match(token::op_leftbrace);
    
    xast::Node *body = parse_stmt_block(true);
    xast::c::loop::body(loop) = body;
    loop->sloc >>= body->sloc;

    return loop;
}

xast::Node *Parser::parse_struct() {
    // struct: { field: type, ... }
    auto def = make_node(
        xast::nk::struct_,
        {},
        tk.sloc,
        token::op_leftbrace
    );
    consume();
    
    while (tk.type != token::op_rightbrace) {
        if (tk.type == token::identifier) {
            auto field = make_node(xast::nk::field, Identifier{ tk.str, tk.sloc },
                tk.sloc, token::identifier);
            consume();
            auto annot = parse_type_annotation(true);
            xast::c::field::type_annot(field) = annot;
            if (annot) {
                field->sloc >>= annot->sloc;
            }
            def->add(field);
        }
        else {
            match(token::op_rightbrace);
        }
    }

    def->sloc >>= tk.sloc;
    consume();
    return def;
}

xast::Node *Parser::parse_union() {
    // union: | variant | variant(payload) | ...
    auto loc = tk.sloc;
    auto def = make_node(
        xast::nk::union_,
        {},
        loc,
        token::op_pipe
    );
    while (tk.type == token::op_pipe) {
        consume();
        
        if (tk.type != token::identifier) {
            DiagnosticHandler::make(diag::id::decl_expected_identifier, prev_tk_loc ^ tk.sloc)
                .finish();
            fatal_abort();
        }
        auto variant = make_node(
            xast::nk::variant,
            Identifier{ tk.str, tk.sloc },
            tk.sloc,
            token::identifier
        );
        consume();

        if (tk.type == token::op_leftparen) {
            consume();
            xast::c::variant::payload(variant) = parse_comma(token::op_rightparen);
            match(token::op_rightparen);
            variant->sloc >>= tk.sloc;
            consume();
        }
        def->add(variant);
    }
    def->sloc >>= prev_tk_loc;
    return def;
}

xast::Node *Parser::parse_composite() {
    assert(tk.type == token::op_leftbrace || tk.type == token::op_pipe || tk.type == token::kw_as);

    xast::Node *composite = make_node(
        xast::nk::composite,
        {},
        prev_tk_loc,
        tk.type
    );
    
    // Type expressions are always meta (compile-time constructs)
    composite->meta = true;

    if (tk.type == token::kw_as) {
        // tmp name
        consume();
        if (tk.type != token::identifier) {
            DiagnosticHandler::make(diag::id::decl_expected_identifier, tk.sloc)
                .finish();
            fatal_abort();
        }
        xast::c::composite::tmpname(composite) = make_node(
            xast::nk::ref,
            Identifier{ tk.str, tk.sloc },
            tk.sloc,
            token::identifier
        );
        consume();
    }

    switch (tk.type) {
        case token::op_leftbrace:
            xast::c::composite::layout(composite) = parse_struct();
            break;
        case token::op_pipe:
            xast::c::composite::layout(composite) = parse_union();
            break;
        default:
            assert(false && "did we add another composite type?");
    }

    if (tk.type == token::kw_where) {
        auto ns = make_node(xast::nk::block, {}, tk.sloc, token::kw_where);
        consume();
        match(token::op_leftbrace);
        consume();
        parse_non_execution_scope(ns);
        match(token::op_rightbrace);
        ns->sloc >>= tk.sloc;
        consume();
        xast::c::composite::namespc(composite) = ns;
    }

    return composite;
}

xast::Node *Parser::parse_structural() {
    switch (tk.type) {
        case token::kw_type:
            if(in_type_annot) return parse_binop();
            consume();
            if (tk.type == token::op_leftbrace
            || tk.type == token::op_pipe
            || tk.type == token::kw_as) {
                return parse_composite();
            }
            else
                return parse_binop();
        case token::kw_fn:
            return parse_function();
        case token::kw_while:
            return parse_loop();
        case token::kw_if:
            return parse_branch();
        default:
            return parse_binop();
    }
}

xast::Node *Parser::parse_expr() {
    // we have to handle assignment separately here to make it bind
    //   lower than structural exprs and type expressions
    auto lhs = parse_structural();
    if (tk.type == token::op_equal) {
        auto loc = tk.sloc;
        consume();
        auto rhs = parse_expr();
        auto assign = make_node(xast::nk::binary_op, Op{ op::assign, loc, token::op_equal },
            lhs->sloc >> rhs->sloc, token::op_equal);
        xast::c::binary_op::lhs(assign) = lhs;
        xast::c::binary_op::rhs(assign) = rhs;
        lhs = assign;
    }
    return lhs;
}

xast::Node *Parser::parse_comma(
    std::optional<token::token_type> end) {

    xast::Node *expr = parse_expr();
    if (tk.type != token::op_comma) return expr;

    // we have a comma expr!

    auto comma_expr = make_node(xast::nk::comma_expr, {}, expr->sloc, token::op_comma);
    comma_expr->add(expr);
    
    while (tk.type == token::op_comma) {
        consume();
        if (end && tk.type == *end) break;
        expr = parse_expr();
        comma_expr->add(expr);
    }

    comma_expr->sloc >>= expr->sloc;
    return comma_expr;
}



xast::Node *Parser::parse_type_expr(bool required) {

    // Check if current token can start an expression
    bool can_start = false;
    switch (tk.type) {
        // Primary expressions
        case token::identifier:
        case token::numeric_literal:
        case token::character_literal:
        case token::string_literal:
        // Parenthesized expressions
        case token::op_leftparen:
        // Prefix operators
        case token::op_asterisk:
        case token::op_amp:
        case token::op_exclamation:
        case token::op_plusplus:
        case token::op_minusminus:
        case token::op_leftbracket:
        case token::op_lrbracket:
        case token::op_plus:
        case token::op_minus:
        // Function type and metatype
        case token::kw_fn:
        case token::kw_type:
            can_start = true;
            break;
        default:
            can_start = false;
    }
    
    if (can_start) {
        // FIXME: this is a workaround to skip parsing assignments here
        //   because they interfere with let bindings that have type annotations
        //   AND initializers.
        return parse_structural();
    }
    
    if (!required) {
        return nullptr;
    }
    
    DiagnosticHandler::make(diag::id::expected_type, prev_tk_loc ^ tk.sloc)
        .finish();
    fatal_abort();
    return nullptr;
}


xast::Node *Parser::parse_type_annotation(bool required) {
    in_type_annot = true;
    if (tk.type == token::op_colon) {
        consume();
        required = true;
    }
    auto result = parse_type_expr(required);
    in_type_annot = false;
    return result;
}

xast::Node *Parser::parse_function() {
    // Grammar: "fn", ["!"], [captures], params, type_annotation, stmtblock
    
    assert(tk.type == token::kw_fn);

    auto fn = make_node(xast::nk::func, {}, tk.sloc, token::kw_fn);
    consume();

    // Optional "!" negation
    if (tk.type == token::op_exclamation) {
        fn->meta = true;
        consume();
    }

    // Optional captures: "[", capture, {",", capture}, "]"
    auto captures = make_node(xast::nk::captures, {}, tk.sloc, token::op_leftbracket);
    xast::c::func::captures(fn) = captures;
    if (tk.type == token::op_leftbracket) {
        consume();
        // For now, skip captures parsing - just consume until ']'
        // TODO: properly parse captures
        while (tk.type != token::op_rightbracket) {
            if (tk.type == token::eof) {
                DiagnosticHandler::make(diag::id::expected_token, prev_tk_loc ^ tk.sloc)
                    .add("]")
                    .finish();
                fatal_abort();
            }
            consume();
        }
        match(token::op_rightbracket);
        consume();
    }

    // Parse parameters: "(", identifier, type_annotation, {",", identifier, type_annotation}, ")"
    match(token::op_leftparen);
    auto params = make_node(xast::nk::params, {}, tk.sloc, token::op_leftparen);
    xast::c::func::params(fn) = params;
    consume();

    bool go = tk.type == token::op_rightparen ? false : true;
    while (go) {
        // expects identifier
        if (tk.type != token::identifier) {
            DiagnosticHandler::make(diag::id::decl_expected_identifier, prev_tk_loc ^ tk.sloc)
                .finish();
            fatal_abort();
        }
        xast::Node *param_node = make_node(
            xast::nk::param,
            Identifier{ tk.str, tk.sloc },
            tk.sloc,
            token::identifier
        );
        consume();
        
        auto tyannot = parse_type_annotation();
        xast::c::param::type_annot(param_node) = tyannot;
        if (tyannot) {
            param_node->sloc >>= tyannot->sloc;
        }
        params->add(param_node);

        // handle comma or end
        if (tk.type == token::op_comma) {
            consume();
        } else if (tk.type == token::op_rightparen) {
            go = false;
        } else {
            match(token::op_rightparen);
            go = false;
        }
    }
    consume();

    // Parse return type annotation
    xast::c::func::return_ty(fn) = parse_type_annotation();

    // Parse function body
    match(token::op_leftbrace);
    xast::Node *func_body = parse_stmt_block(true);
    xast::c::func::body(fn) = func_body;
    fn->sloc >>= func_body->sloc;

    return fn;
}

xast::Node *Parser::parse_stmt_block(bool need_new_scope) {
    xast::Node *block = nullptr;
    assert(tk.type == token::op_leftbrace);
    auto stmt_block_start = tk.sloc;
    block = make_node(xast::nk::block, {}, stmt_block_start, tk.type);
    consume();
    xast::Node *parsed = nullptr;
    while (true) {
        if (tk.type == token::op_rightbrace) {
            break;
        }
        parsed = parse_stmt();
        if (parsed) {
            block->add(parsed);
        } else {
            match(token::op_rightbrace);
        }
    }
    block->sloc >>= tk.sloc;
    consume();
    return block;
}

xast::Node *Parser::parse_bind() {
    // "let", ["!"], identifier, [type_annotation], ["=", expr]
    // At least one of type_annotation or initializer is required
    assert(tk.type == token::kw_let);
    auto loc = tk.sloc;
    consume();

    bool meta = false;
    if (tk.type == token::op_exclamation) {
        meta = true;
        consume();
    }

    if (tk.type != token::identifier) {
        DiagnosticHandler::make(diag::id::decl_expected_identifier, prev_tk_loc ^ tk.sloc)
            .finish();
        fatal_abort();
    }

    auto *decl = make_node(xast::nk::bind, Identifier{ tk.str, tk.sloc }, loc, token::kw_let);
    decl->meta = meta;

    consume();

    // Parse optional type annotation
    xast::Node *type_annot = nullptr;
    if (tk.type == token::op_colon) {
        type_annot = parse_type_annotation();
    }

    // Parse optional initializer (required if no type annotation)
    xast::Node *expr = nullptr;
    if (tk.type == token::op_equal) {
        consume();
        expr = parse_expr();
    }

    // Require at least one of type annotation or initializer
    if (!type_annot && !expr) {
        DiagnosticHandler::make(diag::id::decl_expected_identifier, prev_tk_loc ^ tk.sloc)
            .finish();
        fatal_abort();
    }

    if (type_annot) {
        xast::c::bind::type_annot(decl) = type_annot;
    }
    if (expr) {
        xast::c::bind::def(decl) = expr;
        decl->sloc >>= expr->sloc;
    }

    return decl;
}

xast::Node *Parser::parse_stmt() {
    // Grammar: stmt = decl | expr
    // decl = "let", identifier, [type_annotation], "=", expr
    token::token_type tk_type = tk.type;
    xast::Node *res = nullptr;
    
    switch (tk_type) {
    case token::eof:
        break;
    case token::kw_let: {
        res = parse_bind();
        break;
    }
    default:
        // Everything else is an expression
        res = parse_expr();
        break;
    }
    return res;
}

void Parser::parse_non_execution_scope(xast::Node *container) {
    // Grammar: program = { stmt }
    // Note: We allow any statement at parse time. The analyzer will check
    // after AST simplification that only let bindings remain (since meta
    // expressions like if!, while!, call! should evaluate to bindings).
    while (tk.type != token::eof && tk.type != token::op_rightbrace) {
        xast::Node *stmt = parse_stmt();
        if (stmt) {
            container->add(stmt);
        } else {
            break;
        }
    }
}

/** ------------------- PUBLIC ------------------- */

Parser::Parser(Lexer &lexer, Allocator<xast::Node> &a)
: lexer(lexer), nodes(a) {
    consume();
}

Parser::~Parser() {
#ifdef DEBUG
    std::cout << "Parser destroyed." << std::endl;
#endif
}

std::pair<bool, xast::Node *> Parser::parse() {
    auto prog = make_node(
        xast::nk::prog,
        {},
        SourceLocation(),
        token::unknown
    );
    if (setjmp(env)) {
        return { false, prog };
    }
    bool done = false;
    token::token_type tkty;
    parse_non_execution_scope(prog);
    if (tk.type != token::eof) {
        DiagnosticHandler::make(diag::id::expected_declaration_in_non_execution_context,
            prev_tk_loc ^ tk.sloc)
            .finish();
        fatal_abort();
    }
    
    return { true, prog };
}

} // fe
