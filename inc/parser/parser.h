#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include <setjmp.h>

#include "ast/xast.h"
#include "lexer/lexer.h"
#include "utils/identifier.h"
#include "utils/memory.h"
#include "utils/source.h"

namespace fe {

class Parser {
private:

    /** ------------------- FIELDS ------------------- */

    /** The lexer to generate tokens from */
    Lexer &lexer;

    /** The current token */
    Token tk;

    /** The previous token's loc */
    SourceLocation prev_tk_loc;

    /** The allocator for AST nodes */
    Allocator<xast::Node> &nodes;

    /** The env for longjmp back to runner on syntax error*/
    jmp_buf env;

    // FIXME: super hacky
    bool in_type_annot = false;

    /** ------------------- UTILS ------------------- */

    xast::Node *make_node(
        xast::nk kind,
        xast::NodeData p,
        SourceLocation loc,
        token::token_type
    );

    /**
     * Caches the current token's source loc, then consumes the current
     * token and advances to the next token.
    */
    void consume();

    /**
     * Aborts the current parse. Only call when a fatal error has been found.
     * DOES NOT RETURN.
    */
    [[noreturn]]
    void fatal_abort();

    /**
     * Matches the current token with the expected token type. If they do
     * not match, queues an error with the diagnostics handler, then
     * triggers a fatal abort.
     * 
     * @param expected the expected type of token to match
    */
    void match(token::token_type expected);

    /** ------------------- EXPRESSION PARSING ------------------- */

    /**
     * Operator precedences taken from 
     * https://en.cppreference.com/w/c/language/operator_precedence
    */

    /**
     * OPERATOR PRECEDENCE [1].
     * 
     * Parses a postfix expression given that the current token is the first token
     * after the "unit" (literal, identifier, or parenthesized expr) it applies to.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant postfix token. For example, if current token
     * contains '[' in 'a[0] * b', then it will contain '*' after this function is done.
     * 
     * @param node the unit node to apply to
     * @return the generated result or nullptr if no expression was found
    */
    xast::Node *parse_postfix(xast::Node *node);

    xast::Node *parse_ident();

    /**
     * OPERATOR PRECEDENCE [1, 2].
     * 
     * Parses an expression given that the current token is the first token of
     * the prefix expression. IDENTIFIERS ARE HANDLED HERE.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant "unit" token. For example, if current token
     * contains '*' in '*&a[0] - b', then it will contain '-' after this function is done.
     * 
     * @return the generated result or nullptr if no expression was found
    */
    xast::Node *parse_prefix();

    /**
     * OPERATOR PRECEDENCE [1, 2] - Unary.
     * 
     * Parses a unary expression (prefix operators).
     * 
     * @return the generated result or nullptr if no expression was found
    */
    xast::Node *parse_unary();

    xast::Node *parse_binop(int minbp = 0);

    xast::Node *parse_loop();

    xast::Node *parse_branch();

    xast::Node *parse_struct();

    xast::Node *parse_union();

    xast::Node *parse_composite();

    xast::Node *parse_type_kw();

    xast::Node *parse_structural();

    xast::Node *parse_expr();

    xast::Node *parse_comma(std::optional<token::token_type> end = std::nullopt);

    /** ------------------- STATEMENT PARSING ------------------- */

    xast::Node *parse_type_expr(bool required = true);

    xast::Node *parse_type_annotation(bool required = true);

    /**
     * Parses a call expression's argument list and appends a node to the given node
     * for every argument.
     * 
     * Expects that the current token when called is the left parenthesis at the start
     * of the argument list.
     * 
     * When this function returns, the current token will be the first token after the
     * end of the call list, WHICH INCLUDES the right parenthesis at the end. For example,
     * when parsing "(a, b);", if the current token before call is '(', then the current
     * token after return will be ';'.
     * 
     * @param node the node to add the arguments to
    */

    xast::Node *parse_stmt_block(bool need_new_scope);

    xast::Node *parse_function();

    xast::Node *parse_bind();

    /**
     * Parses a statement given that the current token is the start of the
     * statement. Recursively descends into scoped statement blocks.
     * 
     * @return the parsed statement
    */
    xast::Node *parse_stmt();

    void parse_non_execution_scope(xast::Node *container);

public:

    /**
     * Constructs a Parser.
     * 
     * @param lexer the lexer to use
    */
    Parser(
        Lexer &lexer,
        Allocator<xast::Node> &a
    );

    /**
     * Destructor.
    */
    ~Parser();

    /**
     * Client-facing parse function. Parses until syntax error
     * or eof, and returns the generated tree.
     * 
     * @return true on success, else false
     *         the generated AST
    */
    std::pair<bool, xast::Node *>  parse();
    
};

} // fe

#endif
