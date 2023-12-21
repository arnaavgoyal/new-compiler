#include <string.h>

#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "source/source.h"

Token::Token() :
    type(token::unknown),
    loc(),
    str(nullptr)
{ }

void Token::clear() {
    type = token::unknown;
    loc = SourceLocation();
    str = nullptr;
}

char const *Token::get_token_str() {
    return token::get_token_string(type);
}