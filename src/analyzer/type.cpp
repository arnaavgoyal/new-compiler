#include <vector>

#include "analyzer/type.h"

namespace fe {

ErrorType *ErrorType::get() {
    static auto ptr = new ErrorType;
    return ptr;
}

MetaType *MetaType::get() {
    static auto ptr = new MetaType;
    return ptr;
}

#define PRIMTYPE(name, signed, sizebytes) \
    PrimitiveType *PrimitiveType::get_##name##_type() { \
        static auto ptr = new PrimitiveType(typekind::name##_t, #name, sizebytes); \
        return ptr; \
    }
#include "analyzer/primtypes"

std::vector<PrimitiveType *> const &PrimitiveType::get_all() {

    static std::vector<PrimitiveType *> m{

#define PRIMTYPE(name, signed, sizebytes) get_##name##_type(),
#include "analyzer/primtypes"

    };
    return m;
}

} // fe
