#include "symbol/symbol.h"
#include <iterator>
#include <iostream>

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

    // DO NOT REMOVE THIS OR EVERYTHING WILL BREAK
    enter_scope();
}

Symbol const *SymbolTable::lookup(std::string const &name) const {

    std::pair<
        std::multimap<std::string, Symbol *>::const_iterator,
        std::multimap<std::string, Symbol *>::const_iterator
    > res = table.equal_range(name);

    std::multimap<std::string, Symbol *>::const_iterator begin = res.first;
    std::multimap<std::string, Symbol *>::const_iterator end = res.second;
    
    while (end != begin) {
        end--;
        if (status(end->second->scope_id) == open) {
            return end->second;
        }
    }

    // not found
    return nullptr;
}

Symbol const *SymbolTable::insert(
    std::string name,
    symbol::symbol_type type,
    Symbol const *type_ptr,
    void *data
) {
    Symbol *sym = new Symbol(name, type, curr_scope_id, type_ptr, data);
    return table.insert(std::make_pair(name, sym))->second;
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
    std::cout << "Attempting to free symbol table." << std::endl;
    std::multimap<std::string, Symbol *>::iterator begin;
    std::multimap<std::string, Symbol *>::iterator end = table.end();
    for (begin = table.begin(); begin != end; begin++) {
        delete begin->second;
    }
    std::cout << "Symbol table freed." << std::endl;
}