#include "tokentypes.h"
#include <string>
#include <vector>

char const *token::get_token_string(token_type t) {
    switch (t) {

#define TOKEN(A) case A: return #A;
#define KEYWORD(A) case kw_##A: return #A;
#define OPERATOR(A,B) case op_##A: return #A;
#include "tokendefs"

        default: return "";

    }
}

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
    return nullptr;
}

bool token::is_literal(token_type t) {
    return (t == character_literal || t == numeric_literal || t == string_literal);
}

bool token::is_primitive_type(token_type t) {
    switch (t) {

#define TYPE(A) case kw_##A:
#include "tokendefs"

        return true;
    
    default:
        return false;
    }
}

bool token::is_keyword(token_type t) {
    return (get_keyword_string(t) != nullptr);
}

bool token::is_operator(token_type t) {
    return (get_operator_string(t) != nullptr);
}

std::vector<std::string const *> token::get_types_list () {
    std::vector<std::string const *> prims {

#define TYPE(A) new std::string(#A),
#include "tokendefs"

    };
    return prims;
}