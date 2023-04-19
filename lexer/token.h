#ifndef TOKEN_H
#define TOKEN_H

#include "lexer/tokentypes.h"
#include "source/source.h"

class Token {
private:
    SourceFileID src_id;
    int raw_line;
    int raw_col;
    void *ptr;

public:
    token::token_type type;

    Token();
    Token(token::token_type type, SourceFileID src, int r_line, int r_col, void *ptr);
    ~Token();
    void set(token::token_type type, SourceFileID src, int r_line, int r_col, void *ptr);
    void clear();
    Token &operator=(Token &other);

    token::token_type get_type() { return type; }
    void set_type(token::token_type type) { this->type = type; }
    SourceFileID get_source_id() { return src_id; }
    void set_source_id(SourceFileID id) { src_id = id; }
    int get_raw_line() { return raw_line; }
    void set_raw_line(int line) { raw_line = line; }
    int get_raw_col() { return raw_col; }
    void set_raw_col(int col) { raw_col = col; }
    void *get_ptr() { return ptr; }
    void set_ptr(void *ptr) { this->ptr = ptr; }
};

#endif