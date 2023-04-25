#ifndef PARSER_H
#define PARSER_H

#include "lexer/tokentypes.h"
#include "lexer/token.h"
#include "parser/ast.h"
#include "lexer/lexer.h"
#include "source/source.h"
#include "memory/allocator.h"
#include "analyzer/analyzer.h"
#include "parser/type.h"

class Parser {
private:

    /** ------------------- FIELDS ------------------- */

    /** The lexer to generate tokens from */
    Lexer &lexer;

    /** The semantic analyzer to use */
    SemanticAnalyzer &analyzer;

    /** The current token */
    Token tk;

    /** The previous token's loc */
    SourceLocation prev_tk_loc;

    /** Allocator for AST nodes */
    Allocator<ASTNode> &node_allocator;

    /** Allocator for types */
    Allocator<Type> &type_allocator;

    /** Allocator for strings */
    Allocator<std::string> &str_allocator;

    /** Allocator for type vectors */
    Allocator<std::vector<Type *>> &type_vec_allocator;

    /** ------------------- UTILS ------------------- */

    /**
     * Caches the current token's source loc, then consumes the current
     * token and advances to the next token.
    */
    void consume();

    /**
     * Makes a new error recovery node with the current token's
     * source location.
     * 
     * @return the recovery node
    */
    ASTNode *recovery();

    /**
     * Forwards to the next instance of token of given type.
     * 
     * @param type the type of token
    */
    void skip_to(token::token_type type);

    /**
     * Generically implements the parsing of a left-associative binary operation
     * at a precedence level.
     * 
     * @param higher_prec the higher precedence function to call when parsing
     *                    operands
     * @param types list of tokens to consider equal precedence for the operation
    */
    ASTNode *left_assoc_bin_op(
        ASTNode *(Parser::*higher_prec)(),
        std::vector<token::token_type> const &types
    );

    /**
     * Generically implements the parsing of a right-associative binary operation
     * at a precedence level.
     * 
     * @param higher_prec the higher precedence function to call when parsing
     *                    operands
     * @param types list of tokens to consider equal precedence for the operation
    */
    ASTNode *right_assoc_bin_op(
        ASTNode *(Parser::*higher_prec)(),
        std::vector<token::token_type> const &types
    );

    /**
     * Generically implements the parsing of an N-operand operation at a
     * precedence level.
     * 
     * @param higher_prec the higher precedence function to call when parsing
     *                    operands
     * @param types list of tokens to consider equal precedence for the operation
     * @param node the existing node to add all parsed nodes to
    */
    void n_operand_op(
        ASTNode *(Parser::*higher_prec)(),
        std::vector<token::token_type> const &types,
        ASTNode *node
    );

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
     * ALWAYS consumes all parsed tokens. Thus, the current token will be left containing
     * the token AFTER the last relevant token. For example, if the current token contains
     * 'a' in 'a, b, c;', then it will contain ';' after this function is done.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_comma();

    /**
     * This function serves as the entry point for parsing expressions. It
     * will simply call the lowest precedence expression function to start
     * the parsing.
     * 
     * The current token must be the first token of the expression.
     * 
     * ALWAYS consumes all parsed tokens.
     * 
     * @return pointer to the generated ast or nullptr if no expression was found
    */
    ASTNode *parse_expr();

    /** ------------------- STATEMENT PARSING ------------------- */

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
    void parse_call_args(ASTNode *node);

    /**
     * Parses a type given that the current token is the first token of the type.
     * 
     * @return pointer to the parsed type or nullptr if no type was found
    */
    Type *parse_type();

    ASTNode *parse_decl();

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
     * @param analyzer the semantic analyzer to use
     * @param node_allocator the allocator to use for generated nodes
    */
    Parser(
        Lexer &lexer,
        SemanticAnalyzer &analyzer,
        Allocator<ASTNode> &node_allocator,
        Allocator<Type> &type_allocator,
        Allocator<std::string> &str_allocator,
        Allocator<std::vector<Type *>> &type_vec_allocator,
        std::vector<token::token_type> &primitives
    );

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