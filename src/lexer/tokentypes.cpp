#include "lexer/tokentypes.h"
#include <string>
#include <vector>

char const *token::get_token_string(token_type t) {
    switch (t) {

#define TOKEN(A) case A: return #A;
#define KEYWORD(A) case kw_##A: return #A;
#define OPERATOR(A,B) case op_##A: return #A;
#include "lexer/tokendefs"

        default: return "";

    }
}

char const *token::get_keyword_string(token_type t) {
    switch (t) {

#define KEYWORD(A) case kw_##A: return #A;
#include "lexer/tokendefs"

        default: break;

    }
    return nullptr;
}

char const *token::get_operator_string(token_type t) {
    switch (t) {

#define OPERATOR(A,B) case op_##A: return B;
#include "lexer/tokendefs"

        default: break;

    }
    return nullptr;
}

char const *token::get_print_string(token_type t) {
    if (is_keyword(t)) {
        return get_keyword_string(t);
    }
    if (is_operator(t)) {
        return get_operator_string(t);
    }
    return get_token_string(t);
}

bool token::is_literal(token_type t) {
    return (t == character_literal || t == numeric_literal || t == string_literal);
}

bool token::is_primitive_type(token_type t) {
    switch (t) {

#define TYPE(A) case kw_##A:
#include "lexer/tokendefs"

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

std::vector<token::token_type> token::get_all_keywords() {
    std::vector<token::token_type> kws {

#define KEYWORD(A) kw_##A,
#include "lexer/tokendefs"

    };
    return kws;
}

std::vector<token::token_type> token::get_types_list () {
    std::vector<token::token_type> prims {

#define TYPE(A) kw_##A,
#include "lexer/tokendefs"

    };
    return prims;
}