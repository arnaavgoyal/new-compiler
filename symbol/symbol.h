#ifndef SYMBOL_H
#define SYMBOL_H

#include <string>
#include <map>
#include <stack>
#include <vector>
#include "lexer/tokentypes.h"

/** LeBlanc-Cook style symbol table */

namespace symbol {

    typedef unsigned long int scope_id_t;

    enum symbol_type {
        var,
        func,
        type
    };

}

class Symbol {

    // SymbolTable writes to symbols directly as it creates them
    friend class SymbolTable;

private:

    /** symbol name */
    std::string name;

    /** type of symbol */
    symbol::symbol_type type;

    /** assigned scope id */
    symbol::scope_id_t scope_id;
    
    /**
     * pointer to the symbol that is the decl type of this symbol.
     * not applicable for symbols that are themselves types.
    */
    Symbol const *type_ptr;

    /**
     * extra data:
     * 1) var -- nullptr
     * 2) func -- pointer to params
     * 3) type -- nullptr
    */
    void *data;

    /**
     * Constructs a symbol with all fields as given.
    */
    Symbol(
        std::string name,
        symbol::symbol_type type,
        symbol::scope_id_t scope_id,
        Symbol const *type_ptr,
        void *data
    ) :
        name(name),
        type(type),
        scope_id(scope_id),
        type_ptr(type_ptr),
        data(data)
    { }

public:

    /**
     * Prevent copying to maintain uniqueness of symbols.
    */
    Symbol(Symbol const &other) = delete;
    Symbol &operator=(Symbol const &other) = delete;

    std::string const &get_name() const { return name; }

};

class SymbolTable {
private:

    enum scope_status { open, closed };
    using multimap = std::multimap<std::string, Symbol *>;
    using stack = std::stack<symbol::scope_id_t>;
    using vector = std::vector<std::pair<symbol::scope_id_t, scope_status>>;

    /** symbol hash table */
    multimap table;

    /** scope stack */
    stack scope_stack;

    /** list of all scopes and their status */
    vector scope_history;

    /** the current scope's id */
    symbol::scope_id_t curr_scope_id;

    /** the id generator for new scopes */
    symbol::scope_id_t scope_id_gen;

    /**
     * Gets the status of the scope with given id
     * 
     * @param id the id of the scope
     * @return the status
    */
    scope_status status(symbol::scope_id_t id) const;

public:

    SymbolTable();
    Symbol const *lookup (std::string const &name) const;
    Symbol const *insert(
        std::string name,
        symbol::symbol_type type,
        Symbol const *type_ptr,
        void *data
    );
    void enter_scope();
    void exit_scope();
    void clear();

    /**
     * Prevent copying to maintain uniqueness of symbol tables.
    */
    SymbolTable(SymbolTable const &other) = delete;
    SymbolTable &operator=(SymbolTable &other) = delete;

};

#endif