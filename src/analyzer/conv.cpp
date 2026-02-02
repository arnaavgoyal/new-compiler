#include "analyzer/conv.h"

namespace fe {

unsigned get_integral_rank(typekind kind) {
    // Rank determines conversion ordering for integral types
    // Higher rank = larger capacity
    switch (kind) {
        case typekind::i8_t:
        case typekind::u8_t:
            return 1;
        case typekind::i16_t:
        case typekind::u16_t:
            return 2;
        case typekind::i32_t:
        case typekind::u32_t:
            return 3;
        case typekind::i64_t:
        case typekind::u64_t:
            return 4;
        default:
            return 0;
    }
}

bool is_signedness_change(typekind from, typekind to) {
    bool from_signed = Type::is_signed(from);
    bool to_signed = Type::is_signed(to);
    
    // Both are integral
    if (Type::is_integral(from) && Type::is_integral(to)) {
        return from_signed != to_signed;
    }
    
    return false;
}

ConversionResult can_convert(Type *from, Type *to, bool is_explicit) {
    // Null check
    if (!from || !to) {
        return {convkind::invalid};
    }

    // Identity: same type
    if (from == to) {
        return {convkind::identity};
    }

    // Canonical type comparison
    Type *from_canonical = from->get_canonical();
    Type *to_canonical = to->get_canonical();

    if (from_canonical == to_canonical) {
        return {convkind::identity};
    }

    // --- VOID TYPE CONVERSIONS ---
    // void cannot be converted from/to anything except itself
    if (from_canonical->get_kind() == typekind::void_t ||
        to_canonical->get_kind() == typekind::void_t) {
        return {convkind::invalid};
    }

    // --- POINTER TYPE CONVERSIONS ---
    if (from_canonical->get_kind() == typekind::pointer_t &&
        to_canonical->get_kind() == typekind::pointer_t) {
        auto *from_ptr = static_cast<PointerType *>(from_canonical);
        auto *to_ptr = static_cast<PointerType *>(to_canonical);
        
        Type *from_pointee = from_ptr->get_pointee();
        Type *to_pointee = to_ptr->get_pointee();

        // Same pointee type
        if (from_pointee == to_pointee) {
            return {convkind::pointer_compat};
        }

        // nullptr -> any pointer (if from is nullptr literal)
        // TODO: Add diagnostic point for implicit pointer conversion
        if (!is_explicit) {
            // DIAGNOSTIC POINT: implicit pointer conversion
            // Warn about potential unsafe pointer conversion
        }

        return {convkind::pointer_compat};
    }

    // Cannot convert pointer to non-pointer (unless explicit and numeric)
    if (from_canonical->get_kind() == typekind::pointer_t) {
        if (is_explicit && Type::is_numeric(to_canonical->get_kind())) {
            // TODO: Add diagnostic point for pointer-to-int cast
            return {convkind::lossless};
        }
        return {convkind::invalid};
    }

    if (to_canonical->get_kind() == typekind::pointer_t) {
        if (is_explicit && Type::is_numeric(from_canonical->get_kind())) {
            // TODO: Add diagnostic point for int-to-pointer cast
            return {convkind::lossless};
        }
        return {convkind::invalid};
    }

    // --- NUMERIC TYPE CONVERSIONS ---
    typekind from_kind = from_canonical->get_kind();
    typekind to_kind = to_canonical->get_kind();

    bool from_is_numeric = Type::is_numeric(from_kind);
    bool to_is_numeric = Type::is_numeric(to_kind);

    if (from_is_numeric && to_is_numeric) {
        // Both floating point
        if (Type::is_fp(from_kind) && Type::is_fp(to_kind)) {
            unsigned from_size = from_canonical->get_size();
            unsigned to_size = to_canonical->get_size();

            if (from_size < to_size) {
                // f32 -> f64: lossless
                return {convkind::lossless};
            } else if (from_size > to_size) {
                // f64 -> f32: narrowing
                if (!is_explicit) {
                    // TODO: Add diagnostic point for implicit narrowing float conversion
                    // warn: implicit narrowing conversion from f64 to f32
                }
                return {convkind::narrowing};
            }
            return {convkind::identity};
        }

        // Integral to integral
        if (Type::is_integral(from_kind) && Type::is_integral(to_kind)) {
            unsigned from_rank = get_integral_rank(from_kind);
            unsigned to_rank = get_integral_rank(to_kind);
            bool sign_change = is_signedness_change(from_kind, to_kind);

            if (from_rank < to_rank) {
                // Widening conversion: i32 -> i64
                if (sign_change && !is_explicit) {
                    // TODO: Add diagnostic point for implicit signedness change on widening
                    // note: implicit signedness change in widening conversion
                }
                return {convkind::lossless};
            } else if (from_rank > to_rank) {
                // Narrowing conversion: i64 -> i32
                if (!is_explicit) {
                    // TODO: Add diagnostic point for implicit narrowing integral conversion
                    // warn: implicit narrowing conversion from i64 to i32
                }
                if (sign_change) {
                    // TODO: Add diagnostic point for narrowing with signedness change
                    // warn: narrowing conversion also changes signedness
                }
                return {convkind::narrowing};
            } else {
                // Same rank but different signedness
                if (sign_change) {
                    if (!is_explicit) {
                        // TODO: Add diagnostic point for implicit signedness change
                        // warn: implicit signedness change in integral conversion
                    }
                    return {convkind::narrowing};
                }
                return {convkind::identity};
            }
        }

        // Integral to floating point
        if (Type::is_integral(from_kind) && Type::is_fp(to_kind)) {
            // i32 -> f64: always lossless for practical purposes
            return {convkind::lossless};
        }

        // Floating point to integral
        if (Type::is_fp(from_kind) && Type::is_integral(to_kind)) {
            if (!is_explicit) {
                // TODO: Add diagnostic point for implicit float-to-integral conversion
                // error: cannot implicitly convert floating-point to integral type
                return {convkind::invalid};
            }
            // TODO: Add diagnostic point for explicit float-to-integral conversion
            // note: explicit float-to-integral conversion may lose precision
            return {convkind::narrowing};
        }
    }

    // --- ARRAY TYPE CONVERSIONS ---
    // Arrays can decay to pointers (in some contexts)
    if (from_canonical->get_kind() == typekind::array_t &&
        to_canonical->get_kind() == typekind::pointer_t) {
        auto *from_arr = static_cast<ArrayType *>(from_canonical);
        auto *to_ptr = static_cast<PointerType *>(to_canonical);

        Type *arr_element = from_arr->get_element_ty();
        Type *ptr_pointee = to_ptr->get_pointee();

        if (arr_element == ptr_pointee) {
            // TODO: Add diagnostic point for array-to-pointer decay
            // note: array decays to pointer to first element
            return {convkind::lossless};
        }
    }

    // --- FUNCTION TYPE CONVERSIONS ---
    // Function types are typically not convertible except for pointers
    if (from_canonical->get_kind() == typekind::function_t ||
        to_canonical->get_kind() == typekind::function_t) {
        return {convkind::invalid};
    }

    // --- DECLARABLE TYPE CONVERSIONS ---
    // Alias types should use their aliasee for conversion checking
    if (from_canonical->get_kind() == typekind::alias_t) {
        auto *alias_from = static_cast<AliasType *>(from_canonical);
        return can_convert(alias_from->aliasee, to, is_explicit);
    }
    if (to_canonical->get_kind() == typekind::alias_t) {
        auto *alias_to = static_cast<AliasType *>(to_canonical);
        return can_convert(from, alias_to->aliasee, is_explicit);
    }

    // Struct/union types are not implicitly convertible
    if (from_canonical->get_kind() == typekind::struct_t ||
        from_canonical->get_kind() == typekind::union_t ||
        to_canonical->get_kind() == typekind::struct_t ||
        to_canonical->get_kind() == typekind::union_t) {
        return {convkind::invalid};
    }

    // Default: no conversion
    return {convkind::invalid};
}

}  // namespace fe
