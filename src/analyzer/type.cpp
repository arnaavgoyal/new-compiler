#include "analyzer/type.h"
#include <cassert>
#include <memory>

Type::Type() {
    str = nullptr;
    kind = type::error_type;
    points_to = nullptr;
    canonical = nullptr;
    is_integral = false;
}

Type *Type::get_u8_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_u8));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_i8_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_i8));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_u16_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_u16));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_i16_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_i16));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_u32_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_u32));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_i32_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_i32));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_u64_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_u64));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_i64_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_i64));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_f32_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_f32));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_f64_type() {
    static std::string const *str = new std::string(token::get_keyword_string(token::kw_f64));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, true));
    return ty.get();
}
Type *Type::get_void_type() {
    static std::string *str = new std::string(token::get_keyword_string(token::kw_void));
    static std::unique_ptr<Type> ty(new Type(str, type::primitive_type, false));
    return ty.get();
}
Type *Type::get_prim(token::token_type tk) {
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
}
Type *Type::get_error_type() {
    static std::string *str = new std::string("<error-type>");
    static std::unique_ptr<Type> ty(new Type(str, type::error_type, true));
    return ty.get();
}
