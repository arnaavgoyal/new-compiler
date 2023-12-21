#ifndef SCOPE_H
#define SCOPE_H

#include "analyzer/hashtable.h"
#include "analyzer/symbol.h"
#include <map>
#include <string>
#include <iostream>

namespace fe {

class Scope {
public:

    std::map<std::string, Symbol *> sym_table;
    std::map<std::string, Type *> type_table;
    Scope *parent;

    Scope() { parent = nullptr; }

    int dump(int ind = 1) {
        std::string indstr;
        if (parent != nullptr) {
            ind = parent->dump(ind);
            indstr = std::string(ind, ' ');
        }
        for (auto p : sym_table) {
            std::cout
                << indstr << "sym "
                << p.second->get_type_ptr()->stringify()
                << " " << p.second->get_name()
                << std::endl;
        }
        for (auto p : type_table) {
            std::cout
                << indstr << "type "
                << p.second->stringify() << std::endl;
        }
        return ind + 2;
    }

    void dump_me() {
        for (auto p : sym_table) {
            std::cout
                << "sym "
                << p.second->get_type_ptr()->stringify()
                << " " << p.second->get_name()
                << std::endl;
        }
        for (auto p : type_table) {
            std::cout
                << "type "
                << p.second->stringify() << std::endl;
        }
    }
};

}

#endif