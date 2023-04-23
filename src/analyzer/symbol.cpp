#include "analyzer/symbol.h"
#include "parser/type.h"
#include <iterator>
#include <iostream>

//#define DEBUG

SymbolTable::scope_status SymbolTable::status(symbol::scope_id_t id) const {
    if (id >= scope_history.size()) {
        std::cout << "id: " << id << ", size: " << scope_history.size() << std::endl;
        printf("SymbolTable::status() -- FATAL ERROR\n");
        exit(EXIT_FAILURE);
    }
    return scope_history[id].second;
}

SymbolTable::SymbolTable() {

    // initialize id trackers
    scope_id_gen = 0;
    curr_scope_id = 0;

    // enter initial (global) scope
    enter_scope();
}

Symbol const *SymbolTable::lookup(std::string const &name) {

    
    hash_table::iterator iter = table.find(name);
#ifdef DEBUG
    std::cout << "attempting to find: " << name << std::endl;
#endif
    if (iter != table.end()) {
        sym_list::reverse_iterator start = iter->second.rbegin();
        sym_list::reverse_iterator end = iter->second.rend();
        while (start != end) {
#ifdef DEBUG
            std::cout << "found: " << (*start)->get_name() << std::endl;
#endif
            if (status((*start)->scope_id) == open) {
                return (*start);
            }
            else {
                start++;
            }
        }
    }

    // not found
    return nullptr;
}

Symbol const *SymbolTable::lookup_in_current_scope(std::string const &name) {
    hash_table::iterator iter = table.find(name);
#ifdef DEBUG
    std::cout << "attempting to find: " << name << std::endl;
#endif
    if (iter == table.end()) {
        return nullptr;
    }

    sym_list::reverse_iterator start = iter->second.rbegin();
    sym_list::reverse_iterator end = iter->second.rend();

    if (start == end) {
        return nullptr;
    }

    if ((*start)->scope_id != curr_scope_id) {
        return nullptr;
    }
#ifdef DEBUG
    std::cout << "found: " << (*start)->get_name() << std::endl;
#endif
    return (*start);
}

Symbol const *SymbolTable::insert(
    std::string const &name,
    symbol::symbol_type type,
    Type const *type_ptr,
    void *data
) {
#ifdef DEBUG
    std::cout << "symtable::insert() -- " << name << std::endl;
#endif
    Symbol *sym = new Symbol(name, type, curr_scope_id, type_ptr, data);
    sym_list &list = table[name];
    list.push_back(sym);
    return sym;
}

void SymbolTable::enter_scope() {
    curr_scope_id = scope_id_gen;
    scope_id_gen++;
    scope_history.push_back(std::make_pair(curr_scope_id, open));
    scope_stack.push(curr_scope_id);
}

void SymbolTable::exit_scope() {
    scope_history[curr_scope_id].second = closed;
    scope_stack.pop();
    curr_scope_id = scope_stack.top();

}

void SymbolTable::clear() {
#ifdef DEBUG
    std::cout << "Attempting to free symbol table." << std::endl;
#endif
    hash_table::iterator end = table.end();
    for (hash_table::iterator i = table.begin(); i != end; i++) {
        for (sym_list::iterator j = i->second.begin(); j != i->second.end(); j++) {
            delete *j;
        }
    }
#ifdef DEBUG
    std::cout << "Symbol table freed." << std::endl;
#endif
}