#ifndef TYPE_H
#define TYPE_H

#include <string>
#include <vector>

namespace type {
    enum type {

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

public:

    /** string representation of this type */
    std::string *str;

    /** the type of this type */
    type::type type;

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

public:

    Type();
    ~Type() { };

    /**
     * Prevent copying.
    */
    Type(Type const &other) = delete;
    Type const &operator=(Type const &other) = delete;

};

#endif