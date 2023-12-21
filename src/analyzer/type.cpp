#include "analyzer/type.h"
#include <cassert>
#include <memory>

namespace fe {

Type *Type::get_error_type() {
    static std::unique_ptr<Type> ptr(new Type());
    return ptr.get();
}

#define PRIMTYPE(which) \
PrimitiveType *PrimitiveType::get_##which##_type() { \
    static std::unique_ptr<PrimitiveType> ptr(new PrimitiveType(typekind::which##_t, token::kw_##which)); \
    return ptr.get(); \
}
#define TYPE(which) PRIMTYPE(which)
#include "lexer/tokendefs"

PrimitiveType *PrimitiveType::get_prim(token::token_type tk) {
    assert(token::is_primitive_type(tk) && "get_prim called with non-primitive token type");
    switch (tk) {

#define PRIM_TOK_CASE(TYTK) \
case token::kw_##TYTK: return get_##TYTK##_type();

        PRIM_TOK_CASE(u8)
        PRIM_TOK_CASE(i8)
        PRIM_TOK_CASE(u16)
        PRIM_TOK_CASE(i16)
        PRIM_TOK_CASE(u32)
        PRIM_TOK_CASE(i32)
        PRIM_TOK_CASE(u64)
        PRIM_TOK_CASE(i64)
        PRIM_TOK_CASE(f32)
        PRIM_TOK_CASE(f64)
        PRIM_TOK_CASE(void)

#undef PRIM_TOK_CASE

        default:
            assert(false && "should be unreachable");
    }

    return nullptr;
}

PointerType *PointerType::get_error_type() {
    static std::unique_ptr<PointerType> ptr(new PointerType(nullptr, Type::get_error_type()));
    return ptr.get();
}

ArrayType *ArrayType::get_error_type() {
    static std::unique_ptr<ArrayType> ptr(new ArrayType(nullptr, Type::get_error_type()));
    return ptr.get();
}

ASTNode *AliasType::error_node = nullptr;
AliasType *AliasType::get_error_type() {
    static std::unique_ptr<AliasType> ptr(new AliasType(Type::get_error_type(), "<error-alias>", Type::get_error_type(), error_node));
    return ptr.get();
}

FunctionType *FunctionType::get_error_type() {
    static std::unique_ptr<FunctionType> ptr(new FunctionType(nullptr, Type::get_error_type(), std::vector<Type *>()));
    return ptr.get();
}

}
