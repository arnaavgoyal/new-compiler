#ifndef TYPE_H
#define TYPE_H

#include <string>
#include <vector>
#include "lexer/tokentypes.h"
#include <map>
#include <cassert>

namespace type {
    enum kind {

        primitive_type,

        pointer_type,
        array_type,
        alias_type,
        function_type,

        error_type
    };
}

class Type {

    friend class SemanticAnalyzer;

private:
    // primitive constructor
    Type(std::string const *str, type::kind kind, bool is_integral)
        : str(str), kind(kind), canonical(this),
        is_integral(is_integral), points_to(nullptr),
        contains_error(false) { }

public:

    /** string representation (as-written) of this type */
    std::string const *str;

    /** the kind of this type */
    type::kind kind;

    /** the equivalent canonical type */
    Type const *canonical;

    /** whether this type is integral or not */
    bool is_integral;

    union {

        /** the type that this pointer type points to */
        Type const *points_to;

        /** the type that this array type is an array of */
        Type const *array_of;

        /** the type that this alias type is an alias of */
        Type const *alias_of;

        /** the type that this func type returns */
        Type const *returns;
    };

    union {

        /** if type is function, this points to the list of param types */
        std::vector<Type const *> params;
    };

    bool contains_error : 1;

    void set(
        std::string *str,
        type::kind kind,
        Type const *inner_type
    ) {
        this->str = str;
        this->kind = kind;
        this->points_to = inner_type;
        this->contains_error = this->contains_error || inner_type->contains_error;
    }

    void set_inner_type(Type const *inner) {
        points_to = inner;
        contains_error = this->contains_error || inner->contains_error;
    }

    void add_param(Type const *param) {
        params.push_back(param);
        contains_error = this->contains_error || param->contains_error;
    }

    std::string const *get_str() const { return str; }

    Type();
    ~Type() { }

    static Type *get_u8_type();
    static Type *get_i8_type();
    static Type *get_u16_type();
    static Type *get_i16_type();
    static Type *get_u32_type();
    static Type *get_i32_type();
    static Type *get_u64_type();
    static Type *get_i64_type();
    static Type *get_f32_type();
    static Type *get_f64_type();
    static Type *get_void_type();
    static Type *get_prim(token::token_type);
    static Type *get_error_type();
};

class NType {
private:
    type::kind kind;
    NType *canonical;

protected:
    NType(type::kind kind, NType *canonical) : kind(kind), canonical(canonical) {
        if (!canonical) {
            this->canonical = this;
        }
    }

public:
    type::kind get_kind() { return kind; }
    NType *get_canonical() { return canonical; }
};

class PointerType : public NType {
private:
    NType *pointee;

protected:
    PointerType(NType *pointee, PointerType *canonical)
        : NType(type::pointer_type, canonical), pointee(pointee) { }

public:
    NType *get_pointee() { return pointee; }
};

class ArrayType : public NType {
private:
    NType *element;

protected:
    ArrayType(NType *element_ty, ArrayType *canonical)
        : NType(type::array_type, canonical), element(element_ty) { }

public:
    NType *get_element_ty() { return element; }
};

class AliasType : public NType {
private:
    std::string str;
    NType *aliasee;

protected:
    AliasType(std::string str, NType *aliasee, NType *canonical)
        : NType(type::alias_type, canonical), str(str), aliasee(aliasee) { }

public:
    NType *get_aliasee() { return aliasee; }
};

class FunctionType : public NType {
private:
    NType *return_ty;
    std::vector<NType *> params;

protected:
    FunctionType(NType *return_ty, std::vector<NType *> params, NType *canonical)
        : NType(type::function_type, canonical), return_ty(return_ty), params(params) { }

public:
    NType *get_return_ty() { return return_ty; }
    std::vector<NType *> get_params() { return params; }
};

#endif
