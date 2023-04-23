#ifndef SYMBOL_H
#define SYMBOL_H

#include <string>
#include <map>
#include <stack>
#include <vector>
#include "lexer/tokentypes.h"
#include "parser/type.h"

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
     * pointer to the type of this symbol
    */
    Type const *type_ptr;

    /**
     * Constructs a symbol with all fields as given.
    */
    Symbol(
        std::string const &name,
        symbol::symbol_type type,
        symbol::scope_id_t scope_id,
        Type const *type_ptr,
        void *data
    ) :
        name(name),
        type(type),
        scope_id(scope_id),
        type_ptr(type_ptr)
    { }

public:

    /**
     * Prevent copying to maintain uniqueness of symbols.
    */
    Symbol(Symbol const &other) = delete;
    Symbol &operator=(Symbol const &other) = delete;

    std::string const &get_name() const { return name; }
    symbol::symbol_type get_type() const { return type; }
    Type const *get_type_ptr() const { return type_ptr; }

};

class SymbolTable {
private:

    enum scope_status { open, closed };
    using sym_list = std::vector<Symbol *>;
    using hash_table = std::map<std::string, sym_list>;
    using stack = std::stack<symbol::scope_id_t>;
    using vector = std::vector<std::pair<symbol::scope_id_t, scope_status>>;

    /** symbol hash table */
    hash_table table;

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
    Symbol const *lookup (std::string const &name);
    Symbol const *lookup_in_current_scope(std::string const &name);
    Symbol const *insert(
        std::string const &name,
        symbol::symbol_type type,
        Type const *type_ptr,
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