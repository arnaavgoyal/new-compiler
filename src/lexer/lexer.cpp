#include <vector>
#include <iostream>
#include <fstream>
#include "lexer/lexer.h"
#include "source/source.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"

//#define DEBUG

void Lexer::lex_numeric_literal(Token &tk) {
    int c;
    std::string *str = str_allocator.alloc();
    tk.str = str;
    bool end = false;
    while (!end) {
        c = src->peek();
        switch (c) {
            case '0': case '1':
            case '2': case '3':
            case '4': case '5':
            case '6': case '7':
            case '8': case '9':
                str->push_back(c);
                src->get();
                col++;
                break;
            default:
                end = true;
                break;
        }
    }
    col--;
}

void Lexer::lex_identifier(Token &tk) {
    int c;
    std::string *ident = str_allocator.alloc();
    tk.str = ident;
    bool end = false;
    while (!end) {
        c = src->peek();
        switch (c) {
            case '0': case '1':
            case '2': case '3':
            case '4': case '5':
            case '6': case '7':
            case '8': case '9':
            case 'A': case 'B': case 'C': case 'D':
            case 'E': case 'F': case 'G': case 'H':
            case 'I': case 'J': case 'K': case 'L':
            case 'M': case 'N': case 'O': case 'P':
            case 'Q': case 'R': case 'S': case 'T':
            case 'U': case 'V': case 'W': case 'X':
            case 'Y': case 'Z':
            case 'a': case 'b': case 'c': case 'd':
            case 'e': case 'f': case 'g': case 'h':
            case 'i': case 'j': case 'k': case 'l':
            case 'm': case 'n': case 'o': case 'p':
            case 'q': case 'r': case 's': case 't':
            case 'u': case 'v': case 'w': case 'x':
            case 'y': case 'z':
            case '_':
                ident->push_back(c);
                src->get();
                col++;
                break;
            default:
                end = true;
                break;
        }
    }
    col--;
}

void Lexer::lex_char_literal(Token &tk) {
    std::string *s = str_allocator.alloc();
    tk.str = s;
    int c;
    while ((c = src->get()) != '\'') {
        col++;
        s->push_back(c);
    }
}

void Lexer::lex_string_literal(Token &tk) {
    std::string *s = str_allocator.alloc();
    tk.str = s;
    int c;
    while ((c = src->get()) != '"') {
        col++;
        s->push_back(c);
    }
}

bool Lexer::lex_line_comment(Token &tk) {
    if (this->save_comments) {
        std::string *s = str_allocator.alloc();
        tk.str = s;
        int c = src->get();
        while (c != '\n' && c != EOF) {
            col++;
            s->push_back(c);
            c = src->get();
        }
        src->unget();
        col--;
        return true;
    }
    else {
        int c = src->get();
        while (c != '\n' && c != EOF) {
            col++;
            c = src->get();
        }
        src->unget();
        col--;
        return false;
    }
}

bool Lexer::lex_block_comment(Token &tk) {
    std::cout << "NYI\n";
    exit(EXIT_FAILURE);
    return false;
}

static inline token::token_type check_keywords(
    void *str,
    unsigned len,
    token::token_type *expecteds
) {
    for (int i = 0; i < len; i++) {
        if (((std::string *)str)->compare(token::get_keyword_string(expecteds[i])) == 0) {
            return expecteds[i];
        }
    }
    return token::identifier;
}

void Lexer::lex_token(Token &tk) {
    int c;
    token::token_type type;
    tk.clear();

lex_start:

    start_offset = src->tellg();
    start_col = col;
    c = src->get();
    switch (c){

        case '\0':
        case EOF:
            type = token::eof;
            break;

        case '\r':
            if (src->peek() == '\n')
                c = src->get();
            [[fallthrough]];
        case '\n':
            line++;
            col = 1;
            goto lex_start;

        case ' ':
            col++;
            [[fallthrough]];
        case '\t':
        case '\f':
        case '\v':
            goto lex_start;

        case '0': case '1':
        case '2': case '3':
        case '4': case '5':
        case '6': case '7':
        case '8': case '9':
            src->unget();
            lex_numeric_literal(tk);
            type = token::numeric_literal;
            break;

        // currently, these call lex_identifier, which allocs a std::string and
        // reads the identifier into it. However, if it is classified as a keyword
        // afterwards, the alloced string will never be used.
        // TODO: fix this error
#define KEYWORD_CHECK(L, C, ...) \
case C: \
    src->unget(); \
    lex_identifier(tk); \
    static token::token_type L##_kws[] = {__VA_ARGS__}; \
    type = check_keywords(tk.str, sizeof(L##_kws) / sizeof(L##_kws[0]), L##_kws); \
    break;

        KEYWORD_CHECK(a, 'a', token::kw_as)
        KEYWORD_CHECK(b, 'b', token::kw_break)
        KEYWORD_CHECK(c, 'c', token::kw_continue)
        KEYWORD_CHECK(d, 'd', token::kw_def)
        KEYWORD_CHECK(e, 'e', token::kw_else)
        KEYWORD_CHECK(f, 'f', token::kw_f32, token::kw_f64, token::kw_false)
        KEYWORD_CHECK(i, 'i', token::kw_i8, token::kw_i16, token::kw_i32, token::kw_i64, token::kw_if)
        KEYWORD_CHECK(r, 'r', token::kw_return)
        KEYWORD_CHECK(t, 't', token::kw_true, token::kw_type)
        KEYWORD_CHECK(u, 'u', token::kw_u8, token::kw_u16, token::kw_u32, token::kw_u64)
        KEYWORD_CHECK(v, 'v', token::kw_var, token::kw_void)
        KEYWORD_CHECK(w, 'w', token::kw_while)

#undef KEYWORD_CHECK
        
        // chars that no keyword starts with
        case 'A': case 'B': case 'C': case 'D':
        case 'E': case 'F': case 'G': case 'H':
        case 'I': case 'J': case 'K': case 'L':
        case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X':
        case 'Y': case 'Z':
        case 'g': case 'h':
        case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q':
        case 's': case 'x':
        case 'y': case 'z': case '_':
            src->unget();
            lex_identifier(tk);
            type = token::identifier;
            break;

        case '\'':
            col++;
            lex_char_literal(tk);
            type = token::character_literal;
            break;
        case '"':
            col++;
            lex_string_literal(tk);
            type = token::string_literal;
            break;

        case '(':
            type = token::op_leftparen;
            break;
        case ')':
            type = token::op_rightparen;
            break;
        case '[':
            type = token::op_leftbracket;
            break;
        case ']':
            type = token::op_rightbracket;
            break;
        case '{':
            type = token::op_leftbrace;
            break;
        case '}':
            type = token::op_rightbrace;
            break;
        
        case '+':
            if (src->peek() == '+') {
                type = token::op_plusplus;
                c = src->get();
                col++;
            }
            else
                type = token::op_plus;
            break;
        case '-':
            if (src->peek() == '-') {
                type = token::op_minusminus;
                c = src->get();
                col++;
            }
            else
                type = token::op_minus;
            break;
        case '*':
            type = token::op_asterisk;
            break;
        case '/':
            c = src->peek();
            if (c == '/') {
                src->get();
                col += 2;
                if (lex_line_comment(tk)) {
                    type = token::comment;
                    break;
                }
                goto lex_start;
            }
            else if (c == '*') {
                src->get();
                col += 2;
                if (lex_block_comment(tk)) {
                    type = token::comment;
                    break;
                }
                goto lex_start;
            }
            type = token::op_slash;
            break;
        case '%':
            type = token::op_percent;
            break;
        
        case '!':
            if (src->peek() == '=') {
                col++;
                type = token::op_exclamationequal;
                c = src->get();
            }
            else
                type = token::op_exclamation;
            break;
        case '&':
            if (src->peek() == '&') {
                col++;
                type = token::op_ampamp;
                c = src->get();
            }
            else
                type = token::op_amp;
            break;
        case '|':
            if (src->peek() == '|') {
                col++;
                type = token::op_pipepipe;
                c = src->get();
            }
            else
                type = token::unknown;
            break;
        
        case '>':
            if (src->peek() == '=') {
                col++;
                type = token::op_greaterequal;
                c = src->get();
            }
            else
                type = token::op_greater;
            break;
        case '<':
            if (src->peek() == '=') {
                col++;
                type = token::op_lessequal;
                c = src->get();
            }
            else
                type = token::op_less;
            break;
        case '=':
            if (src->peek() == '=') {
                col++;
                type = token::op_equalequal;
                c = src->get();
            }
            else if (src->peek() == '>') {
                col++;
                type=token::op_equalgreater;
                c = src->get();
            }
            else {
                type = token::op_equal;
            }
            break;
        case ',':
            type = token::op_comma;
            break;
        case ';':
            type = token::op_semicolon;
            break;

        default:
            type = token::unknown;
            break;

    }

    tk.set(
        type,
        SourceLocation(
            src_id,
            start_offset, (SourceLocation::byte_offset_t)src->tellg() - 1,
            line, start_col, line, col
        )
    );

    col++;
    return;
}

Lexer::Lexer(
    SourceID src_id,
    Allocator<std::string> &str_allocator,
    bool save_comments
) : str_allocator(str_allocator) {
    this->src_id = src_id;
    this->src = SourceManager::open_source(src_id);
    this->save_comments = save_comments;
    this->line = 1;
    this->col = 1;
}

Lexer::~Lexer() {
    src->close();
    delete src;
    src = nullptr;
#ifdef DEBUG
    std::cout << "Lexer destroyed." << std::endl;
#endif
}

void Lexer::lex(Token &tk) {
    lex_token(tk);
}
