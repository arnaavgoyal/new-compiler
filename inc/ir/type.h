#ifndef IR_TYPE_H
#define IR_TYPE_H

#include <vector>
#include <string>

namespace ir {
    enum class typekind {

    // primitives
    
        // integral
        _integral_start,
        u8,  i8,
        u16, i16,
        u32, i32,
        u64, i64,
        _integral_end,

        // floating point
        _fp_start,
        f32, f64,
        _fp_end,

        // other
        label,
        void_ty,
        ptr,

    // constructible

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
    bool is_integral_type() { return kind > typekind::_integral_start && kind < typekind::_integral_end; }
    bool is_fp_type() { return kind > typekind::_fp_start && kind < typekind::_fp_end; }
    virtual std::string stringify() = 0;
};

class PrimitiveType final : public Type {
private:
    std::string str;

    PrimitiveType(typekind kind, std::string str)
        : Type(kind), str(str) { }

public:
    PrimitiveType() = delete;
    PrimitiveType(PrimitiveType &) = delete;
    PrimitiveType(PrimitiveType &&) = delete;

    static PrimitiveType *get_u8_type();
    static PrimitiveType *get_i8_type();
    static PrimitiveType *get_u16_type();
    static PrimitiveType *get_i16_type();
    static PrimitiveType *get_u32_type();
    static PrimitiveType *get_i32_type();
    static PrimitiveType *get_u64_type();
    static PrimitiveType *get_i64_type();
    static PrimitiveType *get_f32_type();
    static PrimitiveType *get_f64_type();
    static PrimitiveType *get_label_type();
    static PrimitiveType *get_void_type();
    static PrimitiveType *get_ptr_type();

    std::string stringify();
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
    std::string stringify();
};

}

#endif