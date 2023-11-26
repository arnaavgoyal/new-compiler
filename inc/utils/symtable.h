#ifndef IR_SYMTABLE_H
#define IR_SYMTABLE_H

#include <map>
#include <string>
#include <cassert>

template <typename T>
class SymbolTable {
private:
    std::map<std::string, T *> map;

public:
    std::string insert(std::string key, T *val) {
        auto res = map.emplace(key, val);
        assert(res.second);
        return res.first->first;
    }
    T *remove(std::string key) {
        auto it = map.find(key);
        assert(it != map.end() && "key not in symbol table");
        T *val = std::move(it->second);
        map.erase(it);
        return val;
    }
    T *get(std::string key) {
        auto it = map.find(key);
        assert(it != map.end() && "key not in symbol table");
        return it->second;
    }
};

#endif