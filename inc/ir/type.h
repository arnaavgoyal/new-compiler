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
    typekind get_kind() const { return kind; }

private:
    friend Type const *get_u8();
    friend Type const *get_i8();
    friend Type const *get_u16();
    friend Type const *get_i16();
    friend Type const *get_u32();
    friend Type const *get_i32();
    friend Type const *get_u64();
    friend Type const *get_i64();
    friend Type const *get_f32();
    friend Type const *get_f64();
    friend Type const *get_label();
    friend Type const *get_void();
};

class PointerType : public Type {
private:
    Type const *points_to;

protected:
    PointerType(PointerType &) = default;
    PointerType(PointerType &&) = default;
    PointerType(Type const *pointee_ty)
        : Type(typekind::pointer), points_to(pointee_ty) { }

public:
    Type const *pointee_ty() const { return points_to; }
};

class ArrayType : public Type {
private:
    Type const *array_of;

protected:
    ArrayType(ArrayType &) = default;
    ArrayType(ArrayType &&) = default;
    ArrayType(Type const *element_ty)
        : Type(typekind::array), array_of(element_ty) { }

public:
    Type const *element_ty() const { return array_of; }
};

class FunctionType : public Type {
private:
    Type const *returns;
    std::vector<Type const *> params;

public:
    FunctionType(FunctionType &) = default;
    FunctionType(FunctionType &&) = default;
    FunctionType(Type const *return_ty, std::vector<Type const *> param_tys)
        : Type(typekind::function), returns(return_ty), params(param_tys) { }

public:
    Type const *return_ty() const { return returns; }
    std::vector<Type const *> const &param_tys() const { return params; }
};

Type const *get_u8();
Type const *get_i8();
Type const *get_u16();
Type const *get_i16();
Type const *get_u32();
Type const *get_i32();
Type const *get_u64();
Type const *get_i64();
Type const *get_f32();
Type const *get_f64();
Type const *get_label();
Type const *get_void();

}

#endif