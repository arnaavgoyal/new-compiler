#include "analyzer/analyzer.h"
#include <iostream>
#include "memory/allocator.h"
#include "error/error.h"

bool SemanticAnalyzer::declared_in_current_scope(std::string const *ident) {
    Symbol const *sym = symtable.lookup_in_current_scope(*ident);
    return sym != nullptr;
}

bool SemanticAnalyzer::declared_in_any_active_scope(std::string const *ident) {
    Symbol const *sym = symtable.lookup(*ident);
    return sym != nullptr;
}

SemanticAnalyzer::SemanticAnalyzer(
    SymbolTable &symtable,
    Allocator<Type> &type_allocator,
    std::vector<std::string const *> const &primitives
) :
    symtable(symtable),
    type_allocator(type_allocator) {
    
    // global scope is entered by default
    // populate with primitive types
    Type *type;
    std::vector<std::string const *>::const_iterator start = primitives.begin();
    while (start != primitives.end()) {
        type = type_allocator.alloc();
        type->str = (std::string *)*start;
        type->type = type::primitive_type;
        symtable.insert(**start, symbol::type, type, nullptr);
        start++;
    }
}

void SemanticAnalyzer::enter_scope() {
    symtable.enter_scope();
}

void SemanticAnalyzer::exit_scope() {
    symtable.exit_scope();
}

Type const *SemanticAnalyzer::get_type_by_string(std::string const *str) {
    Symbol const *sym = symtable.lookup(*str);
    if (sym == nullptr) {
        return nullptr;
    }
    else if (sym->get_type() != symbol::type) {
        return nullptr;
    }
    return sym->get_type_ptr();
}

void SemanticAnalyzer::add_type(
    std::string const *type_str,
    Type *type_ptr
) {
    symtable.insert(*type_str, symbol::type, type_ptr, nullptr);
}

void SemanticAnalyzer::act_on_declaration(
    symbol::symbol_type symtype,
    std::string const *decl_name,
    Type *type_ptr,
    SourceLocation loc
) {
    // error - already declared in current scope
    if (declared_in_current_scope(decl_name)) {
        ErrorHandler::handle(error::redeclared, loc, decl_name->c_str());
        exit(EXIT_FAILURE);
    }
    symtable.insert(*decl_name, symtype, type_ptr, nullptr);
}

void SemanticAnalyzer::act_on_type_alias(
    std::string const *alias,
    Type *type_ptr,
    SourceLocation loc
) {
    // error - already declared in current scope
    if (declared_in_current_scope(alias)) {
        ErrorHandler::handle(error::redeclared, loc, alias->c_str());
        exit(EXIT_FAILURE);
    }
    symtable.insert(*alias, symbol::type, type_ptr, nullptr);
}