#include <memory>
#include <iostream>
#include "ir/type.h"

namespace ir {

std::string PrimitiveType::stringify() {
    return str;
}
PrimitiveType *PrimitiveType::get_i1_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i1, "i1"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_u8_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::u8, "u8"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_i8_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i8, "i8"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_u16_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::u16, "u16"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_i16_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i16, "i16"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_u32_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::u32, "u32"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_i32_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i32, "i32"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_u64_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::u64, "u64"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_i64_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::i64, "i64"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_f32_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::f32, "f32"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_f64_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::f64, "f64"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_label_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::label, "label"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_void_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::void_ty, "void"));
    return ty.get();
}
PrimitiveType *PrimitiveType::get_ptr_type() {
    static std::unique_ptr<PrimitiveType> ty(new PrimitiveType(ir::typekind::ptr, "ptr"));
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
