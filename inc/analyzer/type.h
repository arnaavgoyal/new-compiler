#ifndef TYPE_H
#define TYPE_H

#include <string>
#include <vector>
#include "lexer/tokentypes.h"
#include <map>
#include <cassert>

// namespace type {
//     enum kind {

//         primitive_type,

//         pointer_type,
//         array_type,
//         alias_type,
//         function_type,

//         error_type
//     };
// }

// class ASTNode;

// class Type {

//     friend class SemanticAnalyzer;

// private:
//     // primitive constructor
//     Type(std::string const *str, type::kind kind, bool is_integral)
//         : str(str), kind(kind), canonical(this),
//         is_integral(is_integral), points_to(nullptr),
//         contains_error(false) { }

// public:

//     /** string representation (as-written) of this type */
//     std::string const *str;

//     /** the kind of this type */
//     type::kind kind;

//     /** the equivalent canonical type */
//     Type const *canonical;

//     /** whether this type is integral or not */
//     bool is_integral;

//     union {

//         /** the type that this pointer type points to */
//         Type const *points_to;

//         /** the type that this array type is an array of */
//         Type const *array_of;

//         /** the type that this alias type is an alias of */
//         Type const *alias_of;

//         /** the type that this func type returns */
//         Type const *returns;
//     };

//     union {

//         /** if type is function, this points to the list of param types */
//         std::vector<Type const *> params;
//     };

//     ASTNode *decl;

//     bool contains_error : 1;

//     void set(
//         std::string *str,
//         type::kind kind,
//         Type const *inner_type
//     ) {
//         this->str = str;
//         this->kind = kind;
//         this->points_to = inner_type;
//         this->contains_error = this->contains_error || inner_type->contains_error;
//     }

//     void set_inner_type(Type const *inner) {
//         points_to = inner;
//         contains_error = this->contains_error || inner->contains_error;
//     }

//     void add_param(Type const *param) {
//         params.push_back(param);
//         contains_error = this->contains_error || param->contains_error;
//     }

//     std::string const *get_str() const { return str; }

//     Type();
//     ~Type() { }

//     static Type *get_u8_type();
//     static Type *get_i8_type();
//     static Type *get_u16_type();
//     static Type *get_i16_type();
//     static Type *get_u32_type();
//     static Type *get_i32_type();
//     static Type *get_u64_type();
//     static Type *get_i64_type();
//     static Type *get_f32_type();
//     static Type *get_f64_type();
//     static Type *get_void_type();
//     static Type *get_prim(token::token_type);
//     static Type *get_error_type();
// };

namespace fe {

class ASTNode;
class SemanticAnalyzer;

enum class typekind {

    // error
    error_t,

    // primitives
    _primitive_start,

        // numeric
        _numeric_start,

            // integral
            _integral_start,
            u8_t,  i8_t,
            u16_t, i16_t,
            u32_t, i32_t,
            u64_t, i64_t,
            _integral_end,

            // floating point
            _fp_start,
            f32_t, f64_t,
            _fp_end,
        
        _numeric_end,

        // other
        void_t,

    _primitive_end,

// constructible
    pointer_t,
    array_t,
    alias_t,
    function_t,
};

class Type {
private:
    typekind kind;
    Type *canonical;
    bool error;

protected:
    Type(typekind kind, Type *canonical, bool error)
        : kind(kind), canonical(canonical), error(error) {
        if (!canonical) {
            this->canonical = this;
        }
    }
    void set_error() { error = true; }

private:
    // error type constructor
    Type() : Type(typekind::error_t, this, true) { }

public:
    typekind get_kind() { return kind; }
    Type *get_canonical() { return canonical; }
    bool has_error() { return error; }
    bool is_primitive() { return kind > typekind::_primitive_start && kind < typekind::_primitive_end; }
    bool is_numeric() { return kind > typekind::_numeric_start && kind < typekind::_numeric_end; }
    bool is_integral() { return kind > typekind::_integral_start && kind < typekind::_integral_end; }
    bool is_fp() { return kind > typekind::_fp_start && kind < typekind::_fp_end; }
    virtual std::string stringify() { return "<error-type>"; };

    static Type *get_error_type();
};

class PrimitiveType : public Type {
private:
    token::token_type tok;

protected:
    PrimitiveType(typekind kind, token::token_type tok)
        : Type(kind, this, false), tok(tok) { assert(token::is_keyword(tok)); }
    std::string stringify() override { return token::get_keyword_string(tok); }

public:
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
    static PrimitiveType *get_void_type();
    static PrimitiveType *get_prim(token::token_type);
};

class PointerType : public Type {
private:
    friend SemanticAnalyzer;
    Type *pointee;

protected:
    PointerType(PointerType *canonical, Type *pointee)
        : Type(typekind::pointer_t, canonical, pointee->has_error()), pointee(pointee) { }

public:
    Type *get_pointee() { return pointee; }
    std::string stringify() override { return std::string("*") + pointee->stringify(); }

    static PointerType *get_error_type();
};

class ArrayType : public Type {
private:
    friend SemanticAnalyzer;
    Type *element;

protected:
    ArrayType(ArrayType *canonical, Type *element_ty)
        : Type(typekind::array_t, canonical, element_ty->has_error()), element(element_ty) { }

public:
    Type *get_element_ty() { return element; }
    std::string stringify() override { return std::string("[]") + element->stringify(); }

    static ArrayType *get_error_type();
};

class AliasType : public Type {
private:
    friend SemanticAnalyzer;
    std::string str;
    Type *aliasee;
    ASTNode *decl;

    static ASTNode *error_node;

protected:
    AliasType(Type *canonical, std::string str, Type *aliasee, ASTNode *decl)
        : Type(typekind::alias_t, canonical, aliasee->has_error()), str(str), aliasee(aliasee), decl(decl) { }

public:
    Type *get_aliasee() { return aliasee; }
    ASTNode *get_decl() { return decl; }
    std::string stringify() override { return str; }

    static void set_error_node (ASTNode *en) { error_node = en; }
    static AliasType *get_error_type();
};

class FunctionType : public Type {
private:
    friend SemanticAnalyzer;
    Type *return_ty;
    std::vector<Type *> params;

protected:
    FunctionType(Type *canonical, Type *return_ty, std::vector<Type *> params)
        : Type(typekind::function_t, canonical, false), return_ty(return_ty), params(std::move(params)) {
        if (return_ty->has_error()) {
            set_error();
            return;
        }
        for (auto &ty : params) {
            if (ty->has_error()) {
                set_error();
                break;
            }
        }
    }

public:
    Type *get_return_ty() { return return_ty; }
    std::vector<Type *> get_params() { return params; }
    std::string stringify() override {
        std::string str("(");
        bool hp = false;
        for (auto &param : params) {
            str.append(param->stringify());
            str.append(", ");
            hp = true;
        }
        if (hp) {
            str.pop_back();
            str.pop_back();
        }
        str.append(")");
        str.append(return_ty->stringify());
        return str;
    }

    static FunctionType *get_error_type();
};

}

#endif
