#ifndef ANALYZER_H
#define ANALYZER_H

#include <string>
#include "analyzer/symbol.h"
#include "analyzer/type.h"
#include "parser/ast.h"
#include "memory/allocator.h"
#include <string>
#include "analyzer/type.h"

class SemanticAnalyzer {
private:

    SymbolTable &symtable;

    Allocator<Type> &type_allocator;

public:

    SemanticAnalyzer(
        SymbolTable &symtable,
        Allocator<Type> &type_allocator,
        std::vector<std::string const *> const &primitives
    );

    void enter_scope();
    void exit_scope();

    bool declared_in_current_scope(std::string const *ident);
    bool declared_in_any_active_scope(std::string const *ident);
    
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