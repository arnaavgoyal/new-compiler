#include "analyzer/type.h"
#include <cassert>

Type::Type() {
    str = nullptr;
    kind = type::error_type;
    points_to = nullptr;
    canonical = nullptr;
    is_integral = false;
}
