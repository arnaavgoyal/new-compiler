#include "ir/type.h"
#include <memory>

namespace ir {

Type *get_u8() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::u8));
    return ty.get();
}
Type *get_i8() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::i8));
    return ty.get();
}
Type *get_u16() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::u16));
    return ty.get();
}
Type *get_i16() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::i16));
    return ty.get();
}
Type *get_u32() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::u32));
    return ty.get();
}
Type *get_i32() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::i32));
    return ty.get();
}
Type *get_u64() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::u64));
    return ty.get();
}
Type *get_i64() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::i64));
    return ty.get();
}
Type *get_f32() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::f32));
    return ty.get();
}
Type *get_f64() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::f64));
    return ty.get();
}
Type *get_label() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::label));
    return ty.get();
}
Type *get_void() {
    static std::unique_ptr<Type> ty(new Type(ir::typekind::void_ty));
    return ty.get();
}

}
