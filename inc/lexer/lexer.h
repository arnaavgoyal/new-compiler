#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H

#include <fstream>
#include <vector>

#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "utils/identifier.h"
#include "utils/memory.h"
#include "utils/source.h"

namespace fe {

class Lexer {
private:

    SourceFile *src;
    char const *pos, *startpos;
    StringPool &strings;
    bool save_comments;
    
    char peek() { return *pos; }
    char get() { return *(pos++); }
    void unget() { pos--; }

    SourceLocation sloc() {
        return {
            (uint32_t)(src->base + (startpos - src->content.data()))
            , (uint32_t)(pos - startpos)
        };
    }

    Token lex_identifier();

    Token lex_numeric_literal();
    Token lex_char_literal();
    Token lex_string_literal();

    Token lex_line_comment();
    Token lex_block_comment();

    Token lex_token();

public:

    Lexer
    ( SourceFile *src
    , StringPool &strings
    , bool save_comments) 
    : src(src)
    , strings(strings)
    , save_comments(save_comments) {
        pos = src->content.data();
    }

    Token next() { return lex_token(); }

};

} // fe

#endif
