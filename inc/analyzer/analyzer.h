#ifndef ANALYZER_H
#define ANALYZER_H

#include <string>
#include "analyzer/symbol.h"
#include "parser/type.h"
#include "parser/ast.h"
#include "memory/allocator.h"
#include <string>

class SemanticAnalyzer {
private:

    SymbolTable &symtable;

public:

    SemanticAnalyzer(
        SymbolTable &symtable
    );

    void enter_scope();
    void exit_scope();

    bool used_in_current_scope(std::string const *ident);
    bool used_in_any_active_scope(std::string const *ident);

    bool declared_in_current_scope(std::string const *str);
    bool declared_in_any_scope(std::string const *str);
    Type const *get_type_by_string(std::string const *str);

    void add_type(
        std::string const *type_str,
        Type *type_ptr
    );

    void act_on_declaration(
        symbol::symbol_type symtype,
        std::string const *decl_name,
        Type *type_ptr,
        SourceLocation loc
    );

    void act_on_type_alias(
        std::string const *alias,
        Type *type_ptr,
        SourceLocation loc
    );

};

#endif