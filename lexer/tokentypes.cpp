#include "lexer/tokentypes.h"

char const *token::get_token_string(token_type t) {
    switch (t) {
    #define TOKEN(A) case A: return #A;
    #define KEYWORD(A) case kw_##A: return #A;
    #define OPERATOR(A,B) case op_##A: return #A;
    #include "tokendefs"
    default: break;
    }
    return "";
}

char const *token::get_keyword_string(token_type t) {
    switch (t) {
    #define KEYWORD(A) case kw_##A: return #A;
    #include "tokendefs"
    default: break;
    }
    return "";
}

char const *token::get_operator_string(token_type t) {
    switch (t) {
    #define OPERATOR(A,B) case op_##A: return B;
    #include "tokendefs"
    default: break;
    }
    return "";
}