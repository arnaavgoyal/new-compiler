#include "parser/type.h"

Type::Type() {
    str = nullptr;
    type = type::unknown_type;
    points_to = nullptr;
    params = nullptr;
}
