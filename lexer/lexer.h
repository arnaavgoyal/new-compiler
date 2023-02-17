#ifndef LEXER_H
#define LEXER_H

#include "source.h"
#include "token.h"
#include "tokentypes.h"
#include <fstream>
#include <vector>

class Lexer {
private:
    SourceFileManager src_manager;
    SourceFileID src_id;
    std::ifstream *src;

    int line;
    int col;

    bool save_comments;

    void lex_numeric_literal(Token &tk);
    void lex_identifier(Token &tk);
    void lex_char_literal(Token &tk);
    void lex_string_literal(Token &tk);
    bool lex_line_comment(Token &tk);
    bool lex_block_comment(Token &tk);
    void rectify_token(Token &tk, token::token_type type, int c);

public:
    Lexer(SourceFileManager &manager, SourceFileID src_id, bool sc);
    void lex(Token &tk);
};

#endif