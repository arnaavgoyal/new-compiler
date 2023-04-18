#ifndef PARSER_H
#define PARSER_H

#include "parser/ast.h"
#include "lexer/lexer.h"
#include "source/source.h"

/** Operator precedences taken from https://en.cppreference.com/w/c/language/operator_precedence */

class Parser {
private:

    /** The lexer to generate tokens from */
    Lexer lexer;

    /** The current token */
    Token tk;

    /**
     * OPERATOR PRECEDENCE [1].
     * 
     * Parses a postfix expression given that the current token is a decl_ref.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant postfix token. For example, if current token
     * contains 'a' in 'a[0] * b', then it will contain '*' after this function is done.
     * 
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_postfix();

    /**
     * OPERATOR PRECEDENCE [1, 2].
     * 
     * Parses an expression given that the current token is a "unit",
     * meaning that it is either a literal or a decl_ref with no binary operators.
     * 
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant "unit" token. For example, if current token
     * contains '*' in '*&a[0] - b', then it will contain '-' after this function is done.
     * 
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
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
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
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
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
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
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
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
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
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
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
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
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
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
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
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
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant token. For example, if the current token contains
     * 'a' in 'a, b;', then it will contain ';' after this function is done.
     * 
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_comma(ASTNode *cn = nullptr);

    /**
     * Parses an expression given that the current token is the first token of the
     * expression.
     * 
     * ALWAYS consumes all parsed tokens, EXCEPT for the stop token.
     * 
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
     * 
     * @param stop the token to stop at
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_expr(token::token_type stop);

public:

    /**
     * Empty default constructor.
    */
    Parser();

    /**
     * Constructs a Parser that will use the given Lexer as its source
     * of tokens.
     * @param l the Lexer to use
    */
    Parser(Lexer &l);

    /**
     * Parses as long as the Lexer provides valid tokens.
     * 
     * Once done, returns the generated abstract syntax tree.
     * 
     * Dynamically allocates all generated ast nodes. The caller is responsible for
     * the deallocation of all nodes returned by this function.
     * 
     * @return pointer to the generated ast
    */
    ASTNode *parse();
};

#endif