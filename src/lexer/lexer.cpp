#include <fstream>
#include <iostream>
#include <vector>

#include "utils/identifier.h"
#include "utils/source.h"

#include "lexer/lexer.h"

//#define DEBUG

namespace fe {

Token Lexer::lex_identifier() {
    int c;
    std::string s;
    bool end = false;
    while (!end) {
        c = peek();
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
                s.push_back(c);
                get();
                break;
            default:
                end = true;
                break;
        }
    }
    return { token::identifier, sloc(), strings.get(s) };
}

Token Lexer::lex_numeric_literal() {
    int c;
    std::string str;
    bool end = false;
    while (!end) {
        c = peek();
        switch (c) {
            case '0': case '1':
            case '2': case '3':
            case '4': case '5':
            case '6': case '7':
            case '8': case '9':
                str.push_back(c);
                get();
                break;
            default:
                end = true;
                break;
        }
    }
    return { token::numeric_literal, sloc(), std::stoull(str) };
}

Token Lexer::lex_char_literal() {
    assert(false && "nyi");
}

Token Lexer::lex_string_literal() {
    std::string s;
    int c;
    while ((c = get()) != '"') {
        s.push_back(c);
    }
    return { token::string_literal, sloc(), strings.get(s) };
}

Token Lexer::lex_line_comment() {
    std::string s;
    int c = peek();
    while (c != '\n' && c != SourceManager::END_OF_FILE) {
        s.push_back(c);
        get();
        c = peek();
    }
    if (save_comments)
        return { token::comment, sloc(), strings.get(s) };
    else
        return {};
}

Token Lexer::lex_block_comment() {
    std::string s;
    int c = peek();
    while (c != SourceManager::END_OF_FILE) {
        if (c == token::op_asterisk) {
            get();
            if (peek() == token::op_slash) {
                get();
                break;
            }
            unget();
        }
        s.push_back(c);
        get();
        c = peek();
    }
    if (save_comments)
        return { token::comment, sloc(), strings.get(s) };
    else
        return {};
}

static inline Token check_keywords(
    Token identtk,
    unsigned len,
    token::token_type *expecteds
) {
    for (int i = 0; i < len; i++) {
        if (identtk.str.compare(token::get_keyword_string(expecteds[i])) == 0) {
            return { expecteds[i], identtk.sloc, identtk.str };
        }
    }
    return identtk;
}

Token Lexer::lex_token() {
    char c;
    token::token_type type;

lex_start:

    startpos = pos;
    c = get();
    switch (c){

        case '\r':
            if (peek() == '\n')
                c = get();
            [[fallthrough]];
        case '\n':
            goto lex_start;

        case ' ':
        case '\t':
        case '\f':
        case '\v':
            goto lex_start;

        case '0': case '1':
        case '2': case '3':
        case '4': case '5':
        case '6': case '7':
        case '8': case '9':
            unget();
            return lex_numeric_literal();

        // currently, these call lex_identifier, which allocs a std::string and
        // reads the identifier into it. However, if it is classified as a keyword
        // afterwards, the alloced string will never be used.
        // TODO: fix this error
#define KEYWORD_CHECK(L, C, ...)                            \
    case C: {                                               \
        unget();                                            \
        auto tk = lex_identifier();                         \
        static token::token_type L##_kws[] = {__VA_ARGS__}; \
        return check_keywords(tk,                           \
            sizeof(L##_kws) / sizeof(L##_kws[0]), L##_kws); \
    }

        KEYWORD_CHECK(a, 'a', token::kw_as)
        KEYWORD_CHECK(b, 'b', token::kw_break)
        KEYWORD_CHECK(c, 'c', token::kw_continue)
        KEYWORD_CHECK(e, 'e', token::kw_else)
        KEYWORD_CHECK(f, 'f', token::kw_false)
        KEYWORD_CHECK(i, 'i', token::kw_if)
        KEYWORD_CHECK(l, 'l', token::kw_let)
        KEYWORD_CHECK(r, 'r', token::kw_return)
        KEYWORD_CHECK(t, 't', token::kw_true, token::kw_type)
        KEYWORD_CHECK(w, 'w', token::kw_while, token::kw_where)

#undef KEYWORD_CHECK
        
        // chars that no keyword starts with
        case 'A': case 'B': case 'C': case 'D':
        case 'E': case 'F': case 'G': case 'H':
        case 'I': case 'J': case 'K': case 'L':
        case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X':
        case 'Y': case 'Z':
        case 'd': case 'g': case 'h':
        case 'j': case 'k': case 'm':
        case 'n': case 'o': case 'p': case 'q':
        case 's': case 'u': case 'v': case 'x':
        case 'y': case 'z': case '_':
            unget();
            return lex_identifier();

        case '\\':
            type = token::op_backslash;
            break;
        case '\'':
            return lex_char_literal();
        case '"':
            return lex_string_literal();

        case '(':
            type = token::op_leftparen;
            break;
        case ')':
            if (peek() == '>') {
                type = token::op_rightangleparen;
                c = get();
            }
            else
                type = token::op_rightparen;
            break;
        case '[':
            if (peek() == ']') {
                type = token::op_lrbracket;
                c = get();
            }
            else
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
            if (peek() == '+') {
                type = token::op_plusplus;
                c = get();
            }
            else
                type = token::op_plus;
            break;
        case '-':
            if (peek() == '-') {
                type = token::op_minusminus;
                c = get();
            }
            else if (peek() == '>') {
                type = token::op_minusgreater;
                c = get();
            }
            else
                type = token::op_minus;
            break;
        case '*':
            type = token::op_asterisk;
            break;
        case '/': {
            c = peek();
            if (c == '/') {
                get();
                auto tk = lex_line_comment();
                if (save_comments) return tk;
                else goto lex_start;
            }
            else if (c == '*') {
                get();
                auto tk = lex_block_comment();
                if (save_comments) return tk;
                else goto lex_start;
            }
            type = token::op_slash;
            break;
        }
        case '%':
            type = token::op_percent;
            break;
        
        case '!':
            if (peek() == '=') {
                type = token::op_exclamationequal;
                c = get();
            }
            else
                type = token::op_exclamation;
            break;
        case '&':
            if (peek() == '&') {
                type = token::op_ampamp;
                c = get();
            }
            else
                type = token::op_amp;
            break;
        case '|':
            if (peek() == '|') {
                type = token::op_pipepipe;
                c = get();
            }
            else
                type = token::op_pipe;
            break;
        
        case '>':
            if (peek() == '=') {
                type = token::op_greaterequal;
                c = get();
            }
            else
                type = token::op_greater;
            break;
        case '<':
            if (peek() == '=') {
                type = token::op_lessequal;
                c = get();
            }
            else if (peek() == '(') {
                type = token::op_leftangleparen;
                c = get();
            }
            else
                type = token::op_less;
            break;
        case '=':
            if (peek() == '=') {
                type = token::op_equalequal;
                c = get();
            }
            else if (peek() == '>') {
                type = token::op_equalgreater;
                c = get();
            }
            else {
                type = token::op_equal;
            }
            break;
        case '.':
            type = token::op_dot;
            break;
        case ',':
            type = token::op_comma;
            break;
        case ';':
            type = token::op_semicolon;
            break;
        case ':':
            if (peek() == ':') {
                type = token::op_coloncolon;
                c = get();
            }
            else
                type = token::op_colon;
            break;

        case SourceManager::END_OF_FILE:
            type = token::eof;
            break;
        default:
            type = token::unknown;
            break;

    }

    assert(type == token::unknown || type == token::eof || token::is_operator(type));
    
    return { type, sloc(),
        std::string_view(
            (type == token::unknown || type == token::eof)
            ? token::get_token_string(type)
            : token::get_operator_string(type)
        )
    };
}

} // fe
