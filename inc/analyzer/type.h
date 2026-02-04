#ifndef ANALYZER_TYPE_H
#define ANALYZER_TYPE_H

#include <cassert>
#include <map>
#include <string>
#include <vector>

#include "ast/xast.h"

#define POINTER_SIZE_IN_BYTES 8

namespace fe {

enum class typekind {

    // error
    error_t,

    // for compile-time expressions that return a type
    meta_t,

    // primitives
    _primitive_start,

        // numeric
        _numeric_start,

            // integral
            _integral_start,

                // unsigned
                _unsigned_start,

                    u8_t,
                    u16_t,
                    u32_t,
                    u64_t,

                _unsigned_end,

                // signed
                _signed_start,

                    i8_t,
                    i16_t,
                    i32_t,
                    i64_t,

                _signed_end,

            _integral_end,

            // floating point
            _fp_start,

                f32_t,
                f64_t,

            _fp_end,
        
        _numeric_end,

        // other
        void_t,

    _primitive_end,

    // constructible
    pointer_t,
    array_t,
    function_t,

    // declarable
    __decl_start,
        alias_t,
        struct_t,
        union_t,
        templated_t,
        placeholder_t,
        instantiated_t,
    __decl_end,
};

class Type {
private:
    typekind kind;
    unsigned size; // size in bytes
    Type *canonical;

protected:
    Type(typekind kind, unsigned sz, Type *canonical)
    : kind(kind), size(sz), canonical(canonical) {
        
        if (!canonical) {
            // if canonical is nullptr then this type is canonical
            //   and therefore is its own canonical type
            this->canonical = this;
        }

        assert(this->size == this->canonical->size && "a type and its canonical type must have the same size");
    }

public:
    typekind get_kind() { return kind; }
    unsigned get_size() { return size; }
    Type *get_canonical() { return canonical; }
    virtual std::string stringify() = 0;

    static bool is_primitive(typekind kind) { return kind > typekind::_primitive_start && kind < typekind::_primitive_end; }
    static bool is_numeric(typekind kind) { return kind > typekind::_numeric_start && kind < typekind::_numeric_end; }
    static bool is_integral(typekind kind) { return kind > typekind::_integral_start && kind < typekind::_integral_end; }
    static bool is_unsigned(typekind kind) { return kind > typekind::_unsigned_start && kind < typekind::_unsigned_end; }
    static bool is_signed(typekind kind) { return kind > typekind::_signed_start && kind < typekind::_signed_end; }
    static bool is_fp(typekind kind) { return kind > typekind::_fp_start && kind < typekind::_fp_end; }
    static bool is_decl(typekind kind) { return kind > typekind::__decl_start && kind < typekind::__decl_end; }

    bool is_primitive() { return Type::is_primitive(kind); }
    bool is_numeric() { return Type::is_numeric(kind); }
    bool is_integral() { return Type::is_integral(kind); }
    bool is_unsigned() { return Type::is_unsigned(kind); }
    bool is_signed() { return Type::is_signed(kind); }
    bool is_fp() { return Type::is_fp(kind); }
    bool is_decl() { return Type::is_decl(kind); }
};

struct MetaType : public Type {
private:
    MetaType() : Type(typekind::meta_t, 0, this) { }

public:
    std::string stringify() override { return "<type>"; }
    static MetaType *get();
};

struct ErrorType : public Type {
private: 
    ErrorType() : Type(typekind::error_t, 0, this) { }

public:
    std::string stringify() override { return "<err>"; }
    static ErrorType *get();
};

class PrimitiveType : public Type {
private:
    std::string name;

protected:
    PrimitiveType(typekind kind, std::string name, unsigned size)
        : Type(kind, size, this), name(name) { }

public:
    std::string stringify() override { return name; }

#define PRIMTYPE(name, signed, sizebytes) \
    static PrimitiveType *get_##name##_type();
#include "analyzer/primtypes"

    static std::vector<PrimitiveType *> const &get_all();
};

class PointerType : public Type {
private:
    Type *pointee;

public:
    PointerType(PointerType *canonical, Type *pointee)
    : Type(typekind::pointer_t, POINTER_SIZE_IN_BYTES, canonical)
    , pointee(pointee) { }

    Type *get_pointee() { return pointee; }
    std::string stringify() override { return std::string("*") + pointee->stringify(); }
};

class ArrayType : public Type {
private:
    Type *element;
    unsigned num;

public:
    ArrayType(ArrayType *canonical, Type *element_ty, unsigned num_elements)
    : Type(typekind::array_t, element_ty->get_size() * num, canonical)
    , element(element_ty) { }

    Type *get_element_ty() { return element; }
    std::string stringify() override { return std::string("[]") + element->stringify(); }
};

class FunctionType : public Type {
private:
    Type *return_ty;
    std::vector<Type *> params;

public:
    FunctionType(Type *canonical, Type *return_ty, std::vector<Type *> params)
    : Type(typekind::function_t, POINTER_SIZE_IN_BYTES, canonical)
    , return_ty(return_ty)
    , params(std::move(params)) {
        assert(return_ty);
        for (auto &ty : params) {
            assert(ty);
        }
    }

    Type *get_return_ty() { return return_ty; }
    std::vector<Type *> get_params() { return params; }
    std::string stringify() override {
        std::string str("(");
        bool hp = false;
        for (auto &param : params) {
            str.append(param->stringify());
            str.append(",");
            hp = true;
        }
        if (hp) {
            str.pop_back();
        }
        str.append(")");
        str.append(return_ty->stringify());
        return str;
    }
};

struct DeclType : public Type {
    xast::Node *decl;
    std::string ident;  // local name
    std::string scoped_ident;  // fully scoped name (e.g., "Outer::Inner")

    DeclType(typekind tk, unsigned sz, Type *canonical, xast::Node *decl, std::string scoped_ident = "")
    : Type(tk, sz, canonical), decl(decl) {
        if (decl && decl->data.is_ident()) {
            ident = decl->data.ident;
            this->scoped_ident = scoped_ident.empty() ? ident : scoped_ident;
        } else {
            ident = "";
            this->scoped_ident = scoped_ident;
        }
    }
    std::string stringify() override { 
        return scoped_ident; 
    }
};

struct AliasType : public DeclType {
    Type *aliasee;

    AliasType(xast::Node *decl, Type *aliasee, std::string scoped_ident = "")
    : DeclType(typekind::alias_t, (assert(aliasee), aliasee->get_size()),
        aliasee->get_canonical(), decl, scoped_ident)
    , aliasee(aliasee) { }
};

struct PlaceholderType : public DeclType {
    PlaceholderType(xast::Node *decl, std::string scoped_ident = "")
    : DeclType(typekind::placeholder_t, 0, nullptr, decl, scoped_ident) { }
};

// StructType and UnionType are "leaf" composite types.
// They should generally not appear as user-facing types directly.
// Instead, they should be wrapped in:
//   - InstantiatedType for template instantiations (Vector!(i32))
//   - AliasType for named composites (let! Foo = type { ... })
// Field access goes through the AST node (decl), not through these types.
// Type equality for composites is NOMINAL (identity), not structural.

struct StructType : public DeclType {
    StructType(xast::Node *decl, std::string scoped_ident = "")
    : DeclType(typekind::struct_t, 0, nullptr, decl, scoped_ident) { }
    
    std::string stringify() override {
        return scoped_ident.empty() ? "<struct>" : scoped_ident;
    }
};

struct UnionType : public DeclType {
    UnionType(xast::Node *decl, std::string scoped_ident = "")
    : DeclType(typekind::union_t, 0, nullptr, decl, scoped_ident) { }
    
    std::string stringify() override {
        return scoped_ident.empty() ? "<union>" : scoped_ident;
    }
};

// InstantiatedType represents a template instantiation like Vector!(i32).
// 
// Key properties:
// - template_name: The template's name ("Vector")
// - template_args: Type* pointers to the arguments (should be canonical for comparisons)
// - instantiation_node: The AST composite node for field lookup
//
// Canonicalization:
// - Two InstantiatedTypes are equal if they have the same template_name
//   AND all their canonical template_args match.
// - Should be interned so Vector!(i32) always yields the same Type* pointer.
//
// For AliasType wrapping:
// - IntOptVec = Vector!(Option!(i32)) creates:
//   AliasType("IntOptVec", canonical=InstantiatedType("Vector", [canonical Option!(i32)]))

struct InstantiatedType : public Type {
    std::string template_name;
    std::vector<Type *> template_args;  // Should hold canonical Type* for comparison
    xast::Node *instantiation_node;     // AST node for field lookup
    
    InstantiatedType(std::string template_name, std::vector<Type *> template_args, xast::Node *inst_node = nullptr)
    : Type(typekind::instantiated_t, 0, nullptr)
    , template_name(template_name)
    , template_args(template_args)
    , instantiation_node(inst_node) { }
    
    // For interning: create with canonical as self, then intern
    InstantiatedType(std::string template_name, std::vector<Type *> template_args, 
                     xast::Node *inst_node, Type *canonical)
    : Type(typekind::instantiated_t, 0, canonical)
    , template_name(template_name)
    , template_args(template_args)
    , instantiation_node(inst_node) { }
    
    std::string stringify() override {
        std::string str = template_name;
        str += "!(";
        for (size_t i = 0; i < template_args.size(); ++i) {
            if (i > 0) str += ", ";
            // Non-type args are stored as nullptr, represent as "<cexpr>"
            if (template_args[i])
                str += template_args[i]->stringify();
            else
                str += "<cexpr>";
        }
        str += ")";
        return str;
    }
    
    // Get canonical form of this instantiation (with canonical args)
    std::string canonical_key() const {
        std::string str = template_name;
        str += "!(";
        for (size_t i = 0; i < template_args.size(); ++i) {
            if (i > 0) str += ", ";
            // Non-type args are stored as nullptr, represent as "<cexpr>"
            if (template_args[i])
                str += template_args[i]->get_canonical()->stringify();
            else
                str += "<cexpr>";
        }
        str += ")";
        return str;
    }
};

}

#endif
