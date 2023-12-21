#ifndef TOKENTYPES_H
#define TOKENTYPES_H

#include <string>
#include <vector>

namespace token {

    enum token_type {

#define TOKEN(A) A,
#define KEYWORD(A) kw_##A,
#define OPERATOR(A,B) op_##A,
#include "tokendefs"

        NUM_TOKENS

    };

    /**
     * Gets a given token type's token type name as string.
     * 
     * @param t the token type
     * @return name as string
    */
    char const *get_token_string(token_type t);

    /**
     * Gets a given token type's keyword as string.
     * 
     * @param t the token type
     * @return if keyword, keyword as string;
     *         if not, nullptr
    */
    char const *get_keyword_string(token_type t);

    /**
     * Gets a given token type's operator as string.
     * 
     * @param t the token type
     * @return if operator, operator as string;
     *         if not, nullptr
    */
    char const *get_operator_string(token_type t);

    char const *get_print_string(token_type t);

    /**
     * Determines if the given token type is a literal token.
    */
    bool is_literal(token_type t);

    /**
     * Determines if the given token type is a primitive type.
    */
    bool is_primitive_type(token_type t);

    /**
     * Determines if the given token type is a keyword,
     * which includes both lang keywords and primitive types.
    */
    bool is_keyword(token_type t);

    /**
     * Determines if the given token type is an operator.
    */
    bool is_operator(token_type t);

    /**
     * 
    */
    std::vector<token::token_type> get_all_keywords();

    /**
     * 
    */
    std::vector<token::token_type> get_types_list();

}

#endif