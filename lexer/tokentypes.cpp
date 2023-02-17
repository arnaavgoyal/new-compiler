#include "tokentypes.h"
#include <cstdio>
#include <stdlib.h>

char const *token::get_keyword_string(token_type t) {
    switch (t) {
    #define KEYWORD(A) case kw_##A: return #A;
    #include "tokendefs"
    default: break;
    }
    return nullptr;
}

char const *token::get_operator_string(token_type t) {
    switch (t) {
    #define OPERATOR(A,B) case op_##A: return B;
    #include "tokendefs"
    default: break;
    }
    exit(1);
    return nullptr;
}