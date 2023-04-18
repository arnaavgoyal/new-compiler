#ifndef TOKENTYPES_H
#define TOKENTYPES_H

namespace token {

    enum token_type {
        #define TOKEN(A) A,
        #define KEYWORD(A) kw_##A,
        #define OPERATOR(A,B) op_##A,
        #include "tokendefs"
        NUM_TOKENS
    };

    char const *get_token_string(token_type t);

    char const *get_keyword_string(token_type t);

    char const *get_operator_string(token_type t);

}

#endif