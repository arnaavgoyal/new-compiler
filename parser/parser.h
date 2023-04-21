#ifndef PARSER_H
#define PARSER_H

#include "parser/ast.h"
#include "lexer/lexer.h"
#include "source/source.h"
#include "memory/allocator.h"

class Parser {
private:

    /** ------------------- FIELDS ------------------- */

    /** The lexer to generate tokens from */
    Lexer &lexer;

    /** The current token */
    Token tk;

    /** The previous token's loc */
    SourceLocation prev_tk_loc;

    /** Allocator for AST nodes */
    Allocator<ASTNode> &node_allocator;

    /** ------------------- UTILS ------------------- */

    void consume();

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
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_postfix(ASTNode *node);

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
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_prefix();

    /**
     * OPERATOR PRECEDENCE [1, 3].
     * 
     * Parses a multiplicative expression given that the current token is the first
     * token of the multiplicative expression.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant token. For example, if the current token
     * contains 'a' in 'a / *b - c', then it will contain '-' after this function is done.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_multiplicative();

    /**
     * OPERATOR PRECEDENCE [1, 4].
     * 
     * Parses a additive expression given that the current token is the first
     * token of the additive expression.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant token. For example, if the current token contains
     * 'a' in 'a + b >= c', then it will contain '>=' after this function is done.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_additive();

    /**
     * OPERATOR PRECEDENCE [1, 4] U [6].
     * 
     * Parses a greater/less relational expression given that the current token is the
     * first token of the expression.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant token. For example, if the current token contains
     * 'a' in 'a >= b == c', then it will contain '==' after this function is done.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_gl_relational();

    /**
     * OPERATOR PRECEDENCE [1, 4] U [6, 7].
     * 
     * Parses a equal/not equal relational expression given that the current token is the
     * first token of the relational expression.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant token. For example, if the current token contains
     * 'a' in 'a == b && c', then it will contain '&&' after this function is done.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_eq_relational();

    /**
     * OPERATOR PRECEDENCE [1, 4] U [6, 7] U [11].
     * 
     * Parses a logical AND expression given that the current token is the first
     * token of the expression.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant token. For example, if the current token contains
     * 'a' in 'a && b || c', then it will contain '||' after this function is done.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_logical_and();

    /**
     * OPERATOR PRECEDENCE [1, 4] U [6, 7] U [11, 12].
     * 
     * Parses a logical OR expression given that the current token is the first
     * token of the expression.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant token. For example, if the current token contains
     * 'a' in 'a || b;', then it will contain ';' after this function is done.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_logical_or();

    /**
     * OPERATOR PRECEDENCE [1, 4] U [6, 7] U [11, 12], U [14].
     * 
     * Parses an assignment expression given that the current token is the first
     * token of the expression.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant token. For example, if the current token contains
     * 'a' in 'a = b, c', then it will contain ',' after this function is done.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_assignment();

    /**
     * OPERATOR PRECEDENCE [1, 4] U [6, 7] U [11, 12] U [14, 15].
     * 
     * Parses a comma expression given that the current token is the first
     * token of the expression.
     * 
     * If the given node is nullptr, this function will parse the expr as if comma
     * is a binary operator. However, if the given node is not nullptr, this
     * function will parse the expr as if comma is a separator (func decl,
     * call expr, etc).
     * 
     * ALWAYS consumes all parsed tokens EXCEPT for the stop token. Thus, the current
     * token will be left containing the stop token.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_comma(token::token_type stop, ASTNode *cn);

    /**
     * Parses an expression given that the current token is the first token of the
     * expression.
     * 
     * ALWAYS consumes all parsed tokens, EXCEPT for the stop token.
     * 
     * @param stop the token to stop at
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_expr(token::token_type stop);

    /** ------------------- STATEMENT PARSING ------------------- */

    /**
     * Parses a variable declaration given that the current token is the type.
     * 
     * ALWAYS consumes all parsed tokens. For example, if the current token is 'bool'
     * in 'bool foo = 0;', then the current token will be '=' after this function returns.
     * 
     * @return the generated var_decl node
    */
    ASTNode *parse_var_decl();

    /**
     * Parses a function declaration's parameters OR a call expression's arguments given
     * that the current token is the left parenthesis after the function name.
     * 
     * 
     * ALWAYS consumes all parsed tokens, INCLUDING the end parenthesis. For example,
     * if current token is '(' in 'bool foo(bool x);', then current token will be ';'
     * after this function returns.
     * 
     * @param func the func decl node to add the parsed param nodes to
     * @param call if true, parses as call_expr ; if false, parses as func_decl
    */
    void parse_func_params(ASTNode *func, bool call);

    /**
     * Parses a statement given that the current token is the start of the
     * statement. Recursively descends into scoped statement blocks.
     * 
     * Once done, this function returns the generated abstract syntax tree.
     * 
     * @return pointer to the generated ast
    */
    ASTNode *parse_stmt();

public:

    /**
     * Constructs a Parser.
     * 
     * @param lexer the lexer to use
     * @param node_allocator the allocator to use for generated nodes
    */
    Parser(Lexer &lexer, Allocator<ASTNode> &node_allocator);

    /**
     * Destructor.
    */
    ~Parser();

    /**
     * Client-facing parse function. Parses until error or eof,
     * and returns the generated tree.
     * 
     * @return the tree
    */
    ASTNode *parse();
    
};

#endif