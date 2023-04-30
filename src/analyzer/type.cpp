#include "analyzer/type.h"

Type::Type() {
    str = nullptr;
    type = type::error_type;
    points_to = nullptr;
}
