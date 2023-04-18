#include <string.h>

#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "source/source.h"

Token::Token() { }

Token::Token(token::token_type type, SourceFileID src, int r_line, int r_col, void *ptr) {
    set(type, src, r_line, r_col, ptr);
}

Token::~Token() { }

void Token::set(token::token_type type, SourceFileID src, int r_line, int r_col, void *ptr) {
    this->type = type;
    this->src_id = src;
    this->raw_line = r_line;
    this->raw_col = r_col;
    this->ptr = ptr;
}

Token &Token::operator=(Token &other) {
    this->type = other.type;
    this->src_id = other.src_id;
    this->raw_line = other.raw_line;
    this->raw_col = other.raw_col;
    this->ptr = other.ptr;
    return other;
}

void Token::clear() {
    type = token::unknown;
    src_id = SourceFileID();
    raw_line = 0;
    raw_col = 0;
    ptr = nullptr;
}