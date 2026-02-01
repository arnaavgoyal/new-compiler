#ifndef AST_SCOPE_H
#define AST_SCOPE_H

#include <iostream>
#include <string>
#include <unordered_map>

#include "analyzer/type.h"
#include "ast/xast.h"

namespace fe {

class Scope {
public:

    struct transparent_string_hash {
        using is_transparent = void;

        auto operator()(std::string_view sv) const noexcept {
            return std::hash<std::string_view>{}(sv);
        }

        auto operator()(std::string const& s) const noexcept {
            return std::hash<std::string>{}(s);
        }
    };

    template <typename T>
    using owning_sv_map = std::unordered_map<std::string, T,
        transparent_string_hash, std::equal_to<>>;

    owning_sv_map<xast::Node *> symbols;
    owning_sv_map<Type *> types;
    Scope *parent;


    Scope() { parent = nullptr; }

    int dump(int ind = 0) {
        std::string indstr;
        if (parent != nullptr) {
            ind = parent->dump(ind);
        }
        indstr = std::string(ind, ' ');
        for (auto [ident, node] : symbols) {
            std::cout
                << indstr << "sym " << ident
                << std::endl;
        }
        for (auto [ident, type] : types) {
            std::cout
                << indstr << "type " << ident
                << std::endl;
        }
        return ind + 2;
    }

    void dump_me() {
        for (auto [ident, node] : symbols) {
            std::cout
                << "sym " << ident
                << std::endl;
        }
        for (auto [ident, type] : types) {
            std::cout
                << "type " << ident
                << std::endl;
        }
    }
};

} // fe

#endif
