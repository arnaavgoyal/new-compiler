#include "analyzer/type.h"
#include <cassert>

std::map<token::token_type, Type const *> Type::prims;

Type const *Type::builtin(token::token_type tk) {
    assert(token::is_primitive_type(tk));

    // try find
    auto it = prims.find(tk);

    // found
    if (it != prims.end()) {
        return it.operator*().second;
    }

    // not found ... insert

    // TODO: fix mem leak (these pointers are never freed)
    Type *type_ptr = new Type;
    std::string *str = new std::string(token::get_keyword_string(tk));
    type_ptr->kind = type::primitive_type;
    type_ptr->str = str;
    type_ptr->canonical = type_ptr;
    type_ptr->contains_error = false;
    prims.emplace(tk, type_ptr);
    return type_ptr;
}

Type::Type() {
    str = nullptr;
    kind = type::error_type;
    points_to = nullptr;
    canonical = nullptr;
    is_integral = false;
}
