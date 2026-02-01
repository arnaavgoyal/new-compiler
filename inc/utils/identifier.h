#ifndef UTILS_IDENTIFIER_H
#define UTILS_IDENTIFIER_H

#include <cassert>
#include <string_view>

#include "utils/source.h"

namespace fe {
namespace xast {

struct Identifier {
    std::string_view str;
    SourceLocation sloc;

    Identifier() = default;
    Identifier(std::string_view str, SourceLocation sloc)
    : str(str), sloc(sloc) { }

    bool valid() { return str.data() != nullptr; }
    void clear() { str = {}; }

    operator bool() { return valid(); }
    std::string_view &operator *() { assert(valid()); return str; }
};

} // xast
} // fe

#endif
