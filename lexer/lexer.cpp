#include <vector>
#include <iostream>
#include <fstream>

#include "lexer.h"
#include "source.h"
#include "token.h"
#include "tokentypes.h"

Lexer::Lexer(SourceFileManager &manager, SourceFileID src_id, bool sc = false) {
    this->src_manager = manager;
    this->src_id = src_id;
    this->src = manager.get_source(src_id);
    this->save_comments = sc;
    this->line = 1;
    this->col = 1;
}

void Lexer::lex_numeric_literal(Token &tk) {
    int c;
    std::string *str = new std::string;
    tk.set(token::numeric_literal, src_id, line, col, str, true);
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
}

void Lexer::lex_identifier(Token &tk) {
    int c;
    std::string *ident = new std::string;
    tk.set(token::identifier, src_id, line, col, ident, true);
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
}

void Lexer::lex_char_literal(Token &tk) {
    std::cout << "char\n";
}

void Lexer::lex_string_literal(Token &tk) {
    std::cout << "str\n";
}

bool Lexer::lex_line_comment(Token &tk) {
    std::cout << "l com\n";
}

bool Lexer::lex_block_comment(Token &tk) {
    std::cout << "b com\n";
}

void Lexer::rectify_token(Token &tk, token::token_type type, int c) {
    std::string *str;
    if (type == token::unknown) {
        str = new std::string;
        str->push_back((char)c);
    }
    else {
        str = new std::string(token::get_operator_string(type));
    }
    tk.set(type, src_id, line, col, (void *)str, true);
}

void Lexer::lex(Token &tk) {
    int c;
    bool at_line_start = true;
    token::token_type type;
    tk.clear();

lex_start:
    c = src->get();
    switch (c){

        case '\0':
        case EOF:
            tk.set_type(token::eof);
            return;

        case '\r':
            if (src->peek() == '\n')
                c = src->get();
            [[fallthrough]];
        case '\n':
            line++;
            at_line_start = true;
            col = 1;
            goto lex_start;

        case ' ':
            col++;
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
            return;

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
            src->unget();
            lex_identifier(tk);
            return;

        case '\'':
            lex_char_literal(tk);
            return;
        case '"':
            lex_string_literal(tk);
            return;

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
                col++;
                if (lex_line_comment(tk)) {
                    return;
                }
                goto lex_start;
            }
            else if (c == '*') {
                src->get();
                col++;
                if (lex_block_comment(tk)) {
                    return;
                }
                goto lex_start;
            }
            type = token::op_slash;
            break;
        
        case '!':
            type = token::op_exclamation;
            break;
        case '&':
            if (src->peek() == '&') {
                col++;
                type = token::op_ampamp;
                c = src->get();
            }
            else
                type = token::unknown;
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

    rectify_token(tk, type, c);
    col++;
    return;
}