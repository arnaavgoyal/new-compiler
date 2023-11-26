#ifndef IR_TYPE_H
#define IR_TYPE_H

#include <vector>

namespace ir {
    enum class typekind {

    // primitives
    
        // integral
        u8,  i8,
        u16, i16,
        u32, i32,
        u64, i64,

        // floating point
        f32, f64,

        // other
        label,
        void_ty,

    // constructible

        pointer,
        array,
        function,
    };


class Type {
private:
    typekind kind;

protected:
    Type(Type &) = default;
    Type(Type &&) = default;
    Type(typekind kind) : kind(kind) { }

public:
    typekind get_kind() { return kind; }

private:
    friend Type *get_u8();
    friend Type *get_i8();
    friend Type *get_u16();
    friend Type *get_i16();
    friend Type *get_u32();
    friend Type *get_i32();
    friend Type *get_u64();
    friend Type *get_i64();
    friend Type *get_f32();
    friend Type *get_f64();
    friend Type *get_label();
    friend Type *get_void();
};

class PointerType : public Type {
private:
    Type *points_to;

protected:
    PointerType(PointerType &) = default;
    PointerType(PointerType &&) = default;
    PointerType(Type *pointee_ty)
        : Type(typekind::pointer), points_to(pointee_ty) { }

public:
    Type *pointee_ty() { return points_to; }
};

class ArrayType : public Type {
private:
    Type *array_of;

protected:
    ArrayType(ArrayType &) = default;
    ArrayType(ArrayType &&) = default;
    ArrayType(Type *element_ty)
        : Type(typekind::array), array_of(element_ty) { }

public:
    Type *element_ty() { return array_of; }
};

class FunctionType : public Type {
private:
    Type *returns;
    std::vector<Type *> params;

public:
    FunctionType(FunctionType &) = default;
    FunctionType(FunctionType &&) = default;
    FunctionType(Type *return_ty, std::vector<Type *> param_tys)
        : Type(typekind::function), returns(return_ty), params(param_tys) { }

public:
    Type *return_ty() { return returns; }
    std::vector<Type *> &param_tys() { return params; }
};

Type *get_u8();
Type *get_i8();
Type *get_u16();
Type *get_i16();
Type *get_u32();
Type *get_i32();
Type *get_u64();
Type *get_i64();
Type *get_f32();
Type *get_f64();
Type *get_label();
Type *get_void();

}

#endif