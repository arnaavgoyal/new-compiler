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

        unknown_type
    };
}

class Type {

    friend class Parser;

public:

    /** string representation of this type */
    std::string *str;

    /** the type of this type */
    type::type type;

    union {

        /** the type that this pointer type points to */
        Type *points_to;

        /** the type that this array type is an array of */
        Type *array_of;

        /** the type that this alias type is an alias of */
        Type *alias_of;

        /** the type that this func type returns */
        Type *returns;
    };

    /** if type is function, this points to the list of param types */
    std::vector<Type *> *params;

public:

    Type();

    /**
     * Prevent copying.
    */
    Type(Type const &other) = delete;
    Type const &operator=(Type const &other) = delete;

};

#endif