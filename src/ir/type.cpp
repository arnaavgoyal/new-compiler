#include <memory>
#include <iostream>
#include "ir/type.h"

namespace ir {

std::string PrimitiveType::stringify() {
    return str;
}
PrimitiveType *PrimitiveType::get_i1_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i1, 1, "i1"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_u8_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::u8, 8, "u8"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_i8_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i8, 8, "i8"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_u16_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::u16, 16, "u16"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_i16_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i16, 16, "i16"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_u32_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::u32, 32, "u32"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_i32_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i32, 32, "i32"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_u64_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::u64, 64, "u64"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_i64_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i64, 64, "i64"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_f32_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::f32, 32, "f32"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_f64_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::f64, 64, "f64"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_block_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::label, POINTER_SIZE_IN_BITS, "block"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_void_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::void_ty, 0, "void"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_ptr_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::ptr, POINTER_SIZE_IN_BITS, "ptr"));
    return ty.get();
}

std::string FunctionType::stringify() {
    std::string str("(");
    for (Type *p : params) {
        str.append(p->stringify());
    }
    str.append(")");
    str.append(returns->stringify());
    return str;
}

}
