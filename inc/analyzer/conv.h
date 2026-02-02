#ifndef ANALYZER_CONV_H
#define ANALYZER_CONV_H

#include "type.h"

namespace fe {

enum class convkind {
    invalid,           // conversion not allowed
    identity,          // same type
    lossless,          // safe conversion (e.g., i32 -> i64)
    narrowing,         // potentially lossy (e.g., i64 -> i32)
    pointer_compat,    // pointer type compatible
};

struct ConversionResult {
    convkind kind;
    bool is_safe() const { return kind != convkind::invalid; }
    bool is_lossless() const { return kind == convkind::lossless || kind == convkind::identity; }
};

/**
 * @brief Check if a type can be converted to another type.
 * 
 * This function evaluates type compatibility and conversion safety. It provides
 * diagnostic points for:
 * - Narrowing conversions (e.g., i64 -> i32)
 * - Implicit conversions when explicit cast is not used
 * - Signedness changes in integral conversions
 * 
 * @param from Source type
 * @param to Target type
 * @param is_explicit Whether this is an explicit cast/conversion
 * @return ConversionResult describing the conversion compatibility and kind
 */
ConversionResult can_convert(Type *from, Type *to, bool is_explicit = false);

/**
 * @brief Get the rank of an integral type (for conversion ordering).
 * 
 * Used to determine if a conversion is narrowing or lossless.
 * Higher rank = larger type capacity.
 */
unsigned get_integral_rank(typekind kind);

/**
 * @brief Check if a conversion is a sign change (signed <-> unsigned).
 */
bool is_signedness_change(typekind from, typekind to);

}

#endif
