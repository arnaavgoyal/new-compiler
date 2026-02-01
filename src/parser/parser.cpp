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

namespace fe {

xast::Node *Parser::make_node(
    xast::nk kind,
    xast::Identifier ident,
    SourceLocation loc,
    token::token_type
) {
    xast::Node *node = nodes(kind);
    node->ident = ident;
    node->sloc = loc;
    return node;
}

xast::Node *Parser::make_node(
    xast::nk kind,
    uint64_t ival,
    SourceLocation loc,
    token::token_type
) {
    xast::Node *node = nodes(kind);
    node->ival = ival;
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

xast::Node *Parser::left_assoc_bin_op(
    xast::Node *(Parser::*higher_prec)(),
    std::vector<token::token_type> const &types,
    std::vector<op::kind> const &ops
) {
    xast::Node *lhs = (this->*higher_prec)();
    xast::Node *rhs;
    xast::Node *binop = lhs;
    SourceLocation op_loc;
    token::token_type op_type;
    op::kind op_kind;
    bool go = true;
    while (go) {
        auto it = std::find(types.begin(), types.end(), tk.type);
        if (it != types.end()) {
            op_type = tk.type;
            op_loc = tk.sloc;
            consume();
            rhs = (this->*higher_prec)();
            op_kind = ops[it - types.begin()];
            binop = make_node(xast::nk::binary_op, {}, op_loc, op_type);
            binop->op = { op_kind, op_loc };
            binop->add(lhs);
            binop->add(rhs);
            lhs = binop;
        } else {
            go = false;
        }
    }
    return binop;
}

xast::Node *Parser::right_assoc_bin_op(
    xast::Node *(Parser::*higher_prec)(),
    std::vector<token::token_type> const &types,
    std::vector<op::kind> const &ops
) {
    xast::Node *lhs = (this->*higher_prec)();
    xast::Node *rhs;
    xast::Node *binop = lhs;
    SourceLocation op_loc;
    token::token_type op_type;
    op::kind op_kind;
    auto it = std::find(types.begin(), types.end(), tk.type);
    if (it != types.end()) {
        op_type = tk.type;
        op_loc = tk.sloc;
        consume();
        rhs = right_assoc_bin_op(higher_prec, types, ops);
        op_kind = ops[it - types.begin()];
        binop = make_node(xast::nk::binary_op, {}, op_loc, op_type);
        binop->op = { op_kind, op_loc };
        binop->add(lhs);
        binop->add(rhs);
    }
    return binop;
}

xast::Node *Parser::parse_postfix(xast::Node *pre) {
    xast::Node *res = pre;
    SourceLocation op_loc;
    std::vector<xast::Node *> args;
    while (true) {
        op_loc = tk.sloc;
        switch (tk.type) {
            case token::op_plusplus:
            case token::op_minusminus: {
                xast::Node *unary = make_node(xast::nk::unary_op, {}, pre->sloc >> op_loc, tk.type);
                unary->op = {
                    (tk.type == token::op_plusplus) ? op::postincr : op::postdecr
                    , op_loc
                };
                xast::c::unary_op::operand(unary) = res;
                res = unary;
                consume();
                break;
            }
            case token::op_leftparen: {
                args = parse_call_args();
                xast::Node *call = make_node(xast::nk::call, {}, op_loc, tk.type);
                xast::c::call::called(call) = res;
                for (unsigned i = 0; i < args.size(); i++) {
                    call->add(args[i]);
                }
                res = call;
                break;
            }
            case token::op_leftbracket: {
                consume();
                xast::Node *sub_expr = parse_expr();
                xast::Node *subscript = make_node(xast::nk::subscript, {}, op_loc, tk.type);
                xast::c::subscript::array(subscript) = res;
                xast::c::subscript::index(subscript) = sub_expr;
                res = subscript;
                consume();
                break;
            }
            case token::op_exclamation: {
                auto tmplargs = parse_tmpl_args();
                assert(tmplargs);
                tmplargs->add(res);
                res = tmplargs;
                break;
            }
            default:
                return res;
        }
    }
}

xast::Node *Parser::parse_prefix() {
    token::token_type type = tk.type;
    xast::Node *res;
    op::kind op_kind;
    SourceLocation op_loc;
    switch (type) {
        case token::op_backslash: {
            consume();
            res = parse_function();
            break;
        }
        case token::op_leftparen: {
            op_loc = tk.sloc;
            consume();
            xast::Node *inner = parse_expr();
            op_loc >>= tk.sloc;
            xast::Node *paren = make_node(xast::nk::paren_expr, {}, op_loc, token::op_leftparen);
            xast::c::paren_expr::inner(paren) = inner;
            consume();
            res = parse_postfix(paren);
            break;
        }
        case token::identifier: {
            res = make_node(xast::nk::ref, { tk.str, tk.sloc }, tk.sloc, token::identifier);
            consume();
            res = parse_postfix(res);
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
            res = make_node(xast::nk::str_lit, { tk.str, tk.sloc }, tk.sloc, token::string_literal);
            consume();
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
            xast::Node *operand = parse_prefix();
            xast::Node *unary = make_node(
                xast::nk::unary_op
                , {}
                , op_loc >> operand->sloc
                , type
            );
            unary->op = { op_kind, op_loc };
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

xast::Node *Parser::parse_cast() {
    // NOTE: Cannot parse type here, semantic phase required.
    xast::Node *expr = parse_prefix();
    if (tk.type != token::kw_as) {
        return expr;
    }
    SourceLocation sloc = expr->sloc;
    consume();
    // Cannot parse type here, so just create a cast node with the expr as child.
    xast::Node *cast_node = make_node(xast::nk::cast, {}, sloc, token::kw_as);
    xast::c::cast::expr(cast_node) = expr;
    // Optionally, store the raw token stream for the type as a child or attribute.
    return cast_node;
}

xast::Node *Parser::parse_multiplicative() {
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
    return left_assoc_bin_op(&Parser::parse_cast, types, ops);
}

xast::Node *Parser::parse_additive() {
    static std::vector<token::token_type> types {
        token::op_plus,
        token::op_minus
    };
    static std::vector<op::kind> ops {
        op::add,
        op::sub
    };
    return left_assoc_bin_op(&Parser::parse_multiplicative, types, ops);
}

xast::Node *Parser::parse_gl_relational() {
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
    return left_assoc_bin_op(&Parser::parse_additive, types, ops);
}

xast::Node *Parser::parse_eq_relational() {
    static std::vector<token::token_type> types {
        token::op_equalequal,
        token::op_exclamationequal
    };
    static std::vector<op::kind> ops {
        op::eq,
        op::neq
    };
    return left_assoc_bin_op(&Parser::parse_gl_relational, types, ops);
}

xast::Node *Parser::parse_logical_and() {
    static std::vector<token::token_type> types {
        token::op_ampamp
    };
    static std::vector<op::kind> ops {
        op::land
    };
    return left_assoc_bin_op(&Parser::parse_eq_relational, types, ops);
}

xast::Node *Parser::parse_logical_or() {
    static std::vector<token::token_type> types {
        token::op_pipepipe
    };
    static std::vector<op::kind> ops {
        op::lor
    };
    return left_assoc_bin_op(&Parser::parse_logical_and, types, ops);
}

xast::Node *Parser::parse_assignment() {
    static std::vector<token::token_type> types {
        token::op_equal
    };
    static std::vector<op::kind> ops {
        op::assign
    };
    return right_assoc_bin_op(&Parser::parse_logical_or, types, ops);
}

xast::Node *Parser::parse_comma() {
    static std::vector<token::token_type> types {
        token::op_comma
    };
    static std::vector<op::kind> ops {
        op::group
    };
    return left_assoc_bin_op(&Parser::parse_assignment, types, ops);
}

xast::Node *Parser::parse_expr() {
    xast::Node *node = parse_comma();
    return node;
}

std::vector<xast::Node *> Parser::parse_call_args() {
    std::vector<xast::Node *> args;
    consume();
    if (tk.type == token::op_rightparen) {
        consume();
        return args;
    }
    xast::Node *arg;
    bool go = true;
    while (go) {
        arg = parse_assignment();
        args.push_back(arg);
        switch (tk.type) {
            case token::op_comma:
                consume();
                break;
            case token::op_rightparen:
                go = false;
                break;
            default:
                match(token::op_rightparen);
        }
    }
    consume();
    return args;
}

xast::Node *Parser::parse_type_expr(bool required) {
    SourceLocation loc = tk.sloc;
    xast::Node *type = nullptr;

    switch (tk.type) {
        case token::op_asterisk: { // pointer type
            consume();
            auto pointee = parse_type_expr();
            type = make_node(xast::nk::te_pointer, {}, loc >> pointee->sloc, token::op_asterisk);
            xast::c::te_pointer::pointee(type) = pointee;
            break;
        }
        case token::op_leftbracket: { // array type
            consume();
            match(token::op_rightbracket);
            consume();
            auto element = parse_type_expr();
            type = make_node(xast::nk::te_array, {}, loc >> element->sloc, token::op_leftbracket);
            xast::c::te_array::element_ty(type) = element;
            break;
        }
        case token::op_leftparen: { // function type
            consume();
            type = make_node(
                xast::nk::te_function,
                {},
                loc,
                token::op_leftparen
            );
            if (tk.type != token::op_rightparen) {
                bool go = true;
                while (go) {
                    type->add(parse_type_expr());
                    if (tk.type == token::op_comma) {
                        consume();
                    } else if (tk.type == token::op_rightparen) {
                        go = false;
                    } else {
                        match(token::op_rightparen);
                        go = false;
                    }
                }
            }
            match(token::op_rightparen);
            consume();
            auto ret_type = parse_type_expr();
            xast::c::te_function::return_ty(type) = ret_type;
            type->sloc = loc >> ret_type->sloc;
            break;
        }
        case token::identifier: {
            type = make_node(xast::nk::te_name, { tk.str, loc }, loc, token::identifier);
            consume();
            auto tmplinst = parse_tmpl_args();
            if (tmplinst) {
                // FIXME: template args are added before instantiated_type, but the accessor
                // expects instantiated_type at index 0. Restructuring parse_tmpl_args pattern.
                xast::c::tmplinstantiation::base(tmplinst) = type;
                type = tmplinst;
            }
            break;
        }
        default:
            if (!required) break;
            DiagnosticHandler::make(diag::id::expected_type, prev_tk_loc ^ tk.sloc)
                .finish();
            fatal_abort();
            break;
        
    }
    return type;
}


xast::Node *Parser::parse_type_annotation(bool required) {
    if (tk.type == token::op_colon) {
        consume();
        required = true;
    }
    return parse_type_expr(required);
}

xast::Node *Parser::parse_tmpl_args() {
    if (tk.type != token::op_exclamation) return nullptr;

    SourceLocation loc = tk.sloc;
    consume();

    match(token::op_leftparen);
    consume();

    xast::Node *instantiation = make_node(
        xast::nk::tmplinstantiation,
        {},
        loc,
        token::op_exclamation
    );
    if (tk.type != token::op_rightparen) {
        xast::Node *arg;
        bool go = true;
        while (go) {
            if (tk.type == token::kw_as) {
                // value arg
                // TODO
                DiagnosticHandler::make(diag::id::nyi, tk.sloc)
                    .add("template value parameter instantiation")
                    .finish();
                fatal_abort();
            }
            else {
                // type arg
                instantiation->add(parse_type_expr());
            }
            switch (tk.type) {
                case token::op_comma:
                    consume();
                    break;
                case token::op_rightparen:
                    go = false;
                    break;
                default:
                    match(token::op_rightparen);
            }
        }
    }
    instantiation->sloc >>= tk.sloc;
    consume();
    return instantiation;
}

xast::Node *Parser::parse_tmpl_params() {
    if (tk.type != token::op_exclamation) return nullptr;

    SourceLocation loc = tk.sloc;
    consume();

    match(token::op_leftparen);
    consume();

    xast::Node *tmpl = make_node(xast::nk::tmpldecl, {}, loc, token::op_exclamation);
    if (tk.type != token::op_rightparen) {
        xast::Node *arg;
        bool go = true;
        while (go) {
            if (tk.type != token::identifier) {
                DiagnosticHandler::make(diag::id::decl_expected_identifier, prev_tk_loc ^ tk.sloc)
                    .finish();
                fatal_abort();
            }
            auto iloc = tk.sloc;
            xast::Identifier ident = { tk.str, iloc };
            consume();

            auto type_annot = parse_type_annotation(false);
            if (type_annot) {
                // this is a value param
                auto node = make_node(xast::nk::param, ident,
                    iloc >> type_annot->sloc, token::identifier);
                xast::c::param::type_annot(node) = type_annot;
                tmpl->add(node);
            }
            else {
                // this is a type param
                tmpl->add(make_node(xast::nk::tmplparamdecl, ident,
                    iloc, token::identifier));
            }
            switch (tk.type) {
                case token::op_comma:
                    consume();
                    break;
                case token::op_rightparen:
                    go = false;
                    break;
                default:
                    match(token::op_rightparen);
            }
        }
    }
    tmpl->sloc >>= tk.sloc;
    consume();
    return tmpl;
}

xast::Node *Parser::parse_function() {
    
    assert(tk.type == token::op_leftparen);

    auto loc = tk.sloc;
    auto fn = make_node(xast::nk::func, {}, loc, token::unknown);

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
            { tk.str, {} },
            tk.sloc,
            token::identifier
        );
        consume();
        
        xast::c::param::type_annot(param_node) = parse_type_annotation();
        fn->add(param_node);

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

    // Parse return type
    xast::c::func::return_ty(fn) = parse_type_annotation();

    // TODO: function application?
    match(token::op_leftbrace);
    xast::Node *func_body = parse_stmt_block(true);
    xast::c::func::body(fn) = func_body;

    return fn;
}

xast::Node *Parser::parse_typebind() {
    assert(tk.type == token::kw_type);
    consume();

    if (tk.type != token::identifier) {
        DiagnosticHandler::make(diag::id::decl_expected_identifier, prev_tk_loc ^ tk.sloc)
            .finish();
        fatal_abort();
    }
    auto iloc = tk.sloc;
    xast::Identifier ident = { tk.str, iloc };
    consume();

    auto tmpl_params = parse_tmpl_params();

    xast::Node *def{};
    switch (tk.type) {
        case token::op_equal: { // alias
            consume();
            // TODO: is this valid?
            // ie. is a partial template instantiation a type?
            def = parse_type_expr();
            break;
        }
        case token::op_leftbrace: { // struct
            auto loc = tk.sloc;
            consume();
            def = make_node(
                xast::nk::struct_,
                {},
                loc,
                token::op_amp
            );
            parse_non_execution_scope(def);
            match(token::op_rightbrace);
            def->sloc >>= tk.sloc;
            consume();
            break;
        }
        case token::op_pipe: { // union
            auto loc = tk.sloc;
            def = make_node(
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
                    { tk.str, iloc },
                    tk.sloc,
                    token::identifier
                );
                consume();

                if (tk.type == token::op_leftparen) {
                    consume();

                    while (tk.type != token::op_rightparen) {

                        variant->add(parse_type_expr());

                        switch (tk.type) {
                        case token::op_comma:
                            consume();
                            break;
                        case token::op_rightparen:
                            break;
                        default:
                            match(token::op_rightparen);
                        }
                    }
                    assert(tk.type == token::op_rightparen);
                    consume();

                }
                def->add(variant);
            }
            break;
        }
    }

    auto decl = make_node(xast::nk::typebind, ident, iloc, token::identifier);
    xast::c::typebind::def(decl) = def;

    if (tk.type == token::kw_where) {
        auto ns = make_node(xast::nk::block, {}, tk.sloc, token::kw_where);
        consume();
        match(token::op_leftbrace);
        consume();
        parse_non_execution_scope(ns);
        match(token::op_rightbrace);
        consume();
        xast::c::typebind::namespace_(decl) = ns;
    }

    if (tmpl_params) {
        xast::c::tmpldecl::decl(tmpl_params) = decl;
        tmpl_params->ident = decl->ident;
        decl->ident = {};
        return tmpl_params;
    }
    return decl;
}

xast::Node *Parser::parse_valbind() {

    assert(tk.type == token::kw_let);
    consume();

    if (tk.type != token::identifier) {
        DiagnosticHandler::make(diag::id::decl_expected_identifier, prev_tk_loc ^ tk.sloc)
            .finish();
        fatal_abort();
    }
    auto iloc = tk.sloc;
    xast::Identifier ident = { tk.str, iloc };
    consume();

    auto tmpl_params = parse_tmpl_params();

    xast::Node *decl = nullptr;

    if (tk.type == token::op_leftparen) {
        // function binding
        decl = make_node(xast::nk::funcbind, ident, iloc, token::identifier);
        xast::c::funcbind::def(decl) = parse_function();
    }
    else {
        // value
        decl = make_node(xast::nk::valbind, ident, iloc, token::identifier);
        xast::c::valbind::type_annot(decl) = parse_type_annotation(false);

        // handle definition
        if (tk.type == token::op_equal) {
            SourceLocation eqloc = tk.sloc;
            consume();
            xast::Node *rhs = parse_expr();
            xast::c::valbind::def(decl) = rhs;
        }
    }

    if (tmpl_params) {
        xast::c::tmpldecl::decl(tmpl_params) = decl;
        tmpl_params->ident = decl->ident;
        decl->ident = {};
        return tmpl_params;
    }
    return decl;
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

xast::Node *Parser::parse_stmt() {
    SourceLocation loc_cache;
    token::token_type tk_type = tk.type;
    xast::Node *res = nullptr;
    switch (tk_type) {
    case token::eof: break;
    case token::op_leftbrace: {
        res = parse_stmt_block(true);
        break;
    }
    case token::kw_let: {
        res = parse_valbind();
        break;
    }
    case token::kw_type: {
        res = parse_typebind();
        break;
    }
    case token::kw_if: {
        loc_cache = tk.sloc;
        consume();
        match(token::op_leftparen);
        consume();
        xast::Node *cond = parse_expr();
        match(token::op_rightparen);
        consume();
        loc_cache >>= prev_tk_loc;
        xast::Node *ifstmt = make_node(xast::nk::branch, {}, loc_cache, token::kw_if);
        ifstmt->add(cond);
        xast::c::branch::cond(ifstmt) = cond;
        match(token::op_leftbrace);
        xast::Node *stmts = parse_stmt_block(true);
        xast::c::branch::then(ifstmt) = stmts;
        if (tk.type == token::kw_else) {
            consume();
            xast::c::branch::else_(ifstmt) = parse_stmt();
        }
        res = ifstmt;
        break;
    }
    case token::kw_while: {
        loc_cache = tk.sloc;
        consume();
        match(token::op_leftparen);
        consume();
        xast::Node *cond = parse_expr();
        match(token::op_rightparen);
        consume();
        loc_cache >>= prev_tk_loc;
        xast::Node *loop = make_node(xast::nk::loop, {}, loc_cache, token::kw_while);
        xast::c::loop::cond(loop) = cond;
        match(token::op_leftbrace);
        xast::Node *stmts = parse_stmt_block(true);
        xast::c::loop::body(loop) = stmts;
        res = loop;
        break;
    }
    default:
        res = parse_expr();
        break;
    }
    return res;
}

void Parser::parse_non_execution_scope(xast::Node *container) {
    bool go = true;
    while (go) {
        switch (tk.type) {
        case token::kw_let:
            container->add(parse_valbind());
            break;
        case token::kw_type:
            container->add(parse_typebind());
            break;
        default:
            go = false;
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
