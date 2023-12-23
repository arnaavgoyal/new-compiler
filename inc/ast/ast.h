#ifndef AST_H
#define AST_H

#include <vector>
#include <string>
#include "source/source.h"
#include "lexer/token.h"
#include "analyzer/type.h"
#include "analyzer/op.h"

namespace fe {

class Symbol;

namespace ast {

    enum node_type {

        // translation unit
        translation_unit,

        // type
        type,

        // integer literal
        int_lit,

        // character literal
        char_lit,

        // string literal
        str_lit,

        // variable declaration
        var_decl,

        // function parameter declaration
        param_decl,

        // function declaration
        func_decl,

        // a variable declaration statement
        decl_stmt,

        // return statement
        ret_stmt,

        // typedef statement
        typedef_stmt,

        // a loop statement
        loop_stmt,

        // an if statement
        if_stmt,

        // a scoped block of statements (func def, loop body, etc)
        stmt_block,

        // reference expression (var ref)
        ref_expr,

        // function call expression
        call_expr,

        // subscript expression (pointer, array)
        subscript_expr,

        // parenthesized expression
        paren_expr,

        // typecast expression
        cast_expr,

        // unary operation
        unary_op,

        // binary operation
        binary_op,

        // error recovery
        recovery,

        // error (for debugging purposes)
        error
    };

}

class ASTNode {

    friend class SemanticAnalyzer;

private:
public:
    ast::node_type kind;
    Type *type;
    bool is_lvalue = false;
    Symbol *sym = nullptr;
    SourceLocation loc;

    // --------------------------------

    std::vector<ASTNode *> children;

    std::string *str;

    op::kind op;

    token::token_type tok;

    bool has_error;

    ASTNode(
        ast::node_type kind,
        Type *type,
        std::string *str,
        SourceLocation loc,
        token::token_type tok,
        bool has_error
    ) { set(kind, type, str, loc, tok, has_error); }

    void set(
        ast::node_type kind,
        Type *type,
        std::string *str,
        SourceLocation loc,
        token::token_type tok,
        bool has_error
    );

    void print_ast(ASTNode *tree, std::string str);



    ASTNode() : children(std::vector<ASTNode *>())
        { kind = ast::error; type = nullptr; str = nullptr; }

    void print();

};

enum class stmtkind {

    // typed statements
    _typed_stmt_start,

        // declarations
        _decl_start,

            // variables
            _var_start,

                localvar,
                globalvar,
                parameter,

            _var_end,

            type,
            function,

        _decl_end,

        // literals
        _literal_start,

            integer,
            character,
            string,

        _literal_end,

        // expressions
        _expr_start,

            loop,
            ifthen,
            reference,
            call,
            subscript,
            parenthesized,
            cast,
            unop,
            binop,

        _expr_end,

    _typed_stmt_end,

    return_stmt,
    break_stmt,
    continue_stmt,

    // special
    stmt_block,
    error
};

enum class valuekind {
    lvalue,
    rvalue
};

class Stmt {
private:
    stmtkind kind;
    SourceLocation loc;
    unsigned num;

protected:
    std::vector<Stmt *> children;

    Stmt(stmtkind kind, SourceLocation loc)
        : kind(kind), loc(loc) { }
    

public:
    stmtkind get_kind() { return kind; }
    SourceLocation get_loc() { return loc; }
};

/**
 * This class only exists to factor out common type logic
 * from Decl and Expr.
*/
class TypedStmt : public Stmt {
private:
    Type *type;

protected:
    TypedStmt(stmtkind kind, SourceLocation loc, Type *type)
        : Stmt(kind, loc), type(type) { }

public:
    Type *get_type() { return type; }
};

/**
 * A language declaration.
 * These can appear in any context.
*/
class Decl : public TypedStmt {
private:
    Symbol *symbol;

protected:
    Decl(stmtkind kind, SourceLocation loc, Type *type, Symbol *symbol)
        : TypedStmt(kind, loc, type), symbol(symbol) { }

public:
    Symbol *get_symbol() { return symbol; }
};

/**
 * A language expression.
 * An expression is every language construct other than declarations.
*/
class Expr : public TypedStmt {
private:
    valuekind vk;

protected:
    Expr(stmtkind kind, SourceLocation loc, Type *type, valuekind valkind)
        : TypedStmt(kind, loc, type), vk(valkind) { }

public:
    valuekind get_valuekind() { return vk; }
};

class TypeDecl : public Decl {
protected:
    TypeDecl(SourceLocation loc, Type *type, Symbol *symbol)
        : Decl(stmtkind::type, loc, type, symbol) { }
};

class VarDecl : public Decl {
protected:
    VarDecl(stmtkind kind, SourceLocation loc, Type *type, Symbol *symbol)
        : Decl(kind, loc, type, symbol) { }

};

class LocalVarDecl : public Decl {
protected:
    LocalVarDecl(SourceLocation loc, Type *type, Symbol *symbol)
        : Decl(stmtkind::localvar, loc, type, symbol) { }

};

class GlobalVarDecl : public Decl {
protected:
    GlobalVarDecl(SourceLocation loc, Type *type, Symbol *symbol)
        : Decl(stmtkind::globalvar, loc, type, symbol) { }

};

class ParamDecl : public VarDecl {
protected:
    ParamDecl(SourceLocation loc, Type *type, Symbol *symbol)
        : VarDecl(stmtkind::parameter, loc, type, symbol) { }
};

/**
 * A declaration context. Only declarations may exist within this context.
*/
class DeclContext {

};

/**
 * An execution context. Statements and declarations may exist within this context.
*/
class ExecContext : public DeclContext {

};

class FuncDecl : public Decl, public ExecContext {
protected:
    FuncDecl(SourceLocation loc, Type *type, Symbol *symbol)
        : Decl(stmtkind::function, loc, type, symbol), ExecContext() { }
};

class TranslationUnit : public DeclContext {

};



}

#endif
