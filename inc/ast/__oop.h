#ifndef AST___OOP_H
#define AST___OOP_H

// #include <vector>

// #include "analyzer/type.h"
// #include "utils/source.h"

// enum class stmtkind {

//     ret,
//     brk,
//     cont,
//     stmt_block,
//     loop,
//     ifthen,

//     // typed statements
//     _typed_stmt_start,

//         // expressions
//         _expr_start,

//             ref,
//             call,
//             subscript,
//             paren,
//             cast,
//             unop,
//             binop,

//             // literals
//             _literal_start,

//                 integer,
//                 character,
//                 string,

//             _literal_end,

//         _expr_end,

//         // declarations
//         _decl_start,

//             // variables
//             _var_start,

//                 localvar,
//                 globalvar,
//                 parameter,

//             _var_end,

//             type,
//             function,

//         _decl_end,

//     _typed_stmt_end,

//     // special
//     error
// };

// enum class valuekind {
//     lvalue,
//     rvalue
// };

// class DeclContext;
// class ExecContext;
// class Stmt;
// class RetStmt;
// class BreakStmt;
// class ContinueStmt;
// class StmtBlock;
// class LoopStmt;
// class IfThenStmt;
// class TypedStmt;
// class Expr;
// class RefExpr;
// class CallExpr;
// class SubscriptExpr;
// class ParenExpr;
// class CastExpr;
// class UnOpExpr;
// class BinOpExpr;
// class Literal;
// class IntLiteral;
// class CharLiteral;
// class StringLiteral;
// class Decl;
// class VarDecl;
// class LocalVarDecl;
// class GlobalVarDecl;
// class ParamDecl;
// class TypeDecl;
// class FuncDecl;

// /**
//  * A declaration context. Only declarations may exist within this context.
// */
// class DeclContext {

// };

// /**
//  * An execution context. Statements and declarations may exist within this context.
// */
// class ExecContext : public DeclContext {

// };

// /** ------------- Statements -------------- */

// class Stmt {
// private:
//     stmtkind kind;

// protected:
//     Stmt(stmtkind kind) : kind(kind) { }
    
// public:
//     stmtkind get_kind() { return kind; }
//     virtual SourceLocation start_loc() = 0;
//     virtual SourceLocation end_loc()   = 0;
// };

// class RetStmt : public Stmt {
// private:
//     Expr *expr;
//     SourceLocation loc;

// protected:
//     RetStmt(Expr *e, SourceLocation loc) : Stmt(stmtkind::ret), expr(e), loc(loc) { }

// public:
//     SourceLocation start_loc() override { return loc; }
//     SourceLocation end_loc()   override { return loc; }
// };

// class BreakStmt : public Stmt {
// private:
//     SourceLocation loc;

// protected:
//     BreakStmt(Expr *e, SourceLocation loc) : Stmt(stmtkind::ret), loc(loc) { }

// public:
//     SourceLocation start_loc() override { return loc; }
//     SourceLocation end_loc()   override { return loc; }
// };

// class ContinueStmt : public Stmt {
// private:
//     SourceLocation loc;

// protected:
//     ContinueStmt(Expr *e, SourceLocation loc) : Stmt(stmtkind::ret), loc(loc) { }

// public:
//     SourceLocation start_loc() override { return loc; }
//     SourceLocation end_loc()   override { return loc; }
// };

// class StmtBlock : public Stmt {
// private:
//     std::vector<Stmt *> stmts;
//     SourceLocation lbloc, rbloc;

// protected:
//     StmtBlock(std::vector<Stmt *> stmts, SourceLocation lloc, SourceLocation rloc)
//         : Stmt(stmtkind::stmt_block), stmts(stmts), lbloc(lloc), rbloc(rloc) { }

// public:
//     SourceLocation start_loc() override { return lbloc; }
//     SourceLocation end_loc()   override { return rbloc; }
// };

// class LoopStmt : public Stmt {
// private:
//     SourceLocation kwloc, lploc, rploc;

// protected:
//     LoopStmt(Expr *cond, StmtBlock *body, SourceLocation kwloc, SourceLocation lploc, SourceLocation rploc)
//         : Stmt(stmtkind::loop), kwloc(kwloc), lploc(lploc), rploc(rploc) { }

// public:
//     SourceLocation start_loc() override { return kwloc; }
//     SourceLocation end_loc()   override { return rploc; }
// };

// class IfThenStmt : public Stmt {
// private:
//     SourceLocation kwloc, lploc, rploc;

// protected:
//     IfThenStmt(Expr *cond, StmtBlock *body, SourceLocation kwloc, SourceLocation lploc, SourceLocation rploc)
//         : Stmt(stmtkind::loop), kwloc(kwloc), lploc(lploc), rploc(rploc) { }

// public:
//     SourceLocation start_loc() override { return kwloc; }
//     SourceLocation end_loc()   override { return rploc; }
// };

// /**
//  * This class only exists to factor out common type logic
//  * from Decl and Expr.
// */
// class TypedStmt : public Stmt {
// private:
//     Type *type;

// protected:
//     TypedStmt(stmtkind kind, Type *type)
//         : Stmt(kind), type(type) { }

// public:
//     Type *get_type() { return type; }
// };

// /** ------------- Expressions -------------- */

// /**
//  * A language expression.
//  * An expression is every language construct other than declarations.
// */
// class Expr : public TypedStmt {
// private:
//     valuekind vk;

// protected:
//     Expr(stmtkind kind, Type *type, valuekind valkind)
//         : TypedStmt(kind, type), vk(valkind) { }

// public:
//     valuekind get_valuekind() { return vk; }
// };

// class RefExpr : public Expr {
// private:
//     Decl *ref;
// };

// class CallExpr : public Expr { };

// class SubscriptExpr : public Expr { };

// class ParenExpr : public Expr { };

// class CastExpr : public Expr { };

// class UnOpExpr : public Expr { };

// class BinOpExpr : public Expr { };

// /** ------------- Literals -------------- */

// class Literal : public Expr { };

// class IntLiteral : public Literal { };

// class CharLiteral : public Literal { };

// class StringLiteral : public Literal { };

// /** ------------- Declarations -------------- */

// /**
//  * A language declaration.
//  * These can appear in any context.
// */
// class Decl : public TypedStmt {
// private:
//     Symbol *symbol;

// protected:
//     Decl(stmtkind kind, Type *type, Symbol *symbol)
//         : TypedStmt(kind, type), symbol(symbol) { }

// public:
//     Symbol *get_symbol() { return symbol; }
// };

// class VarDecl : public Decl {
// protected:
//     VarDecl(stmtkind kind, Type *type, Symbol *symbol)
//         : Decl(kind, type, symbol) { }

// };

// class LocalVarDecl : public VarDecl {
// protected:
//     LocalVarDecl(Type *type, Symbol *symbol)
//         : VarDecl(stmtkind::localvar, type, symbol) { }

// };

// class GlobalVarDecl : public VarDecl {
// protected:
//     GlobalVarDecl(Type *type, Symbol *symbol)
//         : VarDecl(stmtkind::globalvar, type, symbol) { }

// };

// class ParamDecl : public VarDecl {
// protected:
//     ParamDecl(Type *type, Symbol *symbol)
//         : VarDecl(stmtkind::parameter, type, symbol) { }
// };

// class TypeDecl : public Decl {
// protected:
//     TypeDecl(Type *type, Symbol *symbol)
//         : Decl(stmtkind::type, type, symbol) { }
// };

// class FuncDecl : public Decl, public ExecContext {
// protected:
//     FuncDecl(Type *type, Symbol *symbol)
//         : Decl(stmtkind::function, type, symbol), ExecContext() { }
// };

// class TranslationUnit : public DeclContext {

// };

#endif
