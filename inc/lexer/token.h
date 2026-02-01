#ifndef LEXER_TOKEN_H
#define LEXER_TOKEN_H

#include "lexer/tokentypes.h"
#include "utils/source.h"

namespace fe {

struct Token {

    token::token_type type = token::unknown;
    SourceLocation sloc;
    union {
        std::string_view str;
        uint64_t ival;
    };

    Token() : ival{} { }
    Token(token::token_type type, SourceLocation sloc, std::string_view str)
    :  type(type), sloc(sloc), str(str) { }
    Token(token::token_type type, SourceLocation sloc, uint64_t ival)
    :  type(type), sloc(sloc), ival(ival) { }


    std::string_view pstr() {
        if (type == token::character_literal || type == token::unknown 
            || type == token::numeric_literal || type == token::eof) {
            return token::get_token_string(type);
        }
        else {
            return str;
        }
    }
};

} // fe

#endif
