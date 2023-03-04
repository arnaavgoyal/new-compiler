#include "token.h"
#include "tokentypes.h"
#include "source.h"

void Token::set(token::token_type type, SourceFileID src, int r_line, int r_col, void *ptr, bool nd) {
    this->type = type;
    this->src_id = src;
    this->raw_line = r_line;
    this->raw_col = r_col;
    this->ptr = ptr;
    this->dealloc = nd;
}

Token &Token::operator=(Token &other) {
    this->type = other.type;
    this->src_id = other.src_id;
    this->raw_line = other.raw_line;
    this->raw_col = other.raw_col;
    this->ptr = other.ptr;
    return other;
}