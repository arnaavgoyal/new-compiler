#ifndef SCOPE_H
#define SCOPE_H

#include "analyzer/hashtable.h"
#include "analyzer/symbol.h"
#include <map>
#include <string>

class Scope {
public:

    std::map<std::string, Symbol *> sym_table;
    std::map<std::string, Type *> type_table;
    Scope *parent;

    Scope() { parent = nullptr; }
};

#endif