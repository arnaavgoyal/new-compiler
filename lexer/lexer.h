#ifndef LEXER_H
#define LEXER_H

#include "source/source.h"
#include "token.h"
#include "tokentypes.h"
#include "memory/allocator.h"
#include <vector>
#include <fstream>

class Lexer {
private:

    /** ------------------- FIELDS ------------------- */

    /** src file to lex from */
    std::ifstream *src;

    /** source id */
    SourceID src_id;

    /** used to cache start byte offset of token */
    SourceLocation::byte_offset_t start_offset;

    /** used to cache start line of token */
    SourceLocation::rowcol_offset_t start_line;

    /** used to cache start col of token */
    SourceLocation::rowcol_offset_t start_col;

    /** current line */
    SourceLocation::rowcol_offset_t line;

    /** current col */
    SourceLocation::rowcol_offset_t col;

    /** whether to save comments as tokens or skip them */
    bool save_comments;

    /** string allocator */
    Allocator<std::string> &str_allocator;

    /** ------------------- TOKEN LEXING ------------------- */

    /**
     * Lexes a numeric literal given that the next character is the first
     * character of the numeric literal.
     * 
     * @param tk the token to write into
    */
    void lex_numeric_literal(Token &tk);

    /**
     * Lexes an identifier given that the next character is the first
     * character of the identifier.
     * 
     * @param tk the token to write into
    */
    void lex_identifier(Token &tk);

    /**
     * Lexes a character literal given that the next character is the first
     * character of the character literal AFTER the single quotes. For example,
     * if lexing 'c', the next character when calling this function should be c.
     * 
     * @param tk the token to write into
    */
    void lex_char_literal(Token &tk);

    /**
     * Lexes a string literal given that the next character is the first
     * character of the string literal AFTER the double quotes. For example,
     * if lexing "str", the next character when calling this function should be 's'.
     * 
     * @param tk the token to write into
    */
    void lex_string_literal(Token &tk);

    /**
     * Lexes a line comment given that the next character is the first
     * character of the comment AFTER the two slashes. For example, if
     * lexing "//comment", the next character when calling this function
     * should be 'c'.
     * 
     * @param tk the token to write into
    */
    bool lex_line_comment(Token &tk);

    /**
     * Lexes a block comment given that the next character is the first
     * character of the comment AFTER the slash and asterisk. For example,
     * if lexing "/ *comment...", the next character when calling this function
     * should be 'c'.
     * 
     * @param tk the token to write into
    */
    bool lex_block_comment(Token &tk);

    /**
     * Lexes a token into the given token object.
     * 
     * On function return, the next character in the source stream is the first
     * character after the return token.
     * 
     * @param tk the token to write into
    */
    void lex_token(Token &tk);

public:

    /**
     * Constructs a Lexer.
     * 
     * @param src_id the source id of the source to lex from
     * @param str_allocator the allocator to use for identifier and literal strings
     * @param save_comments if true, the lexer will save comments as tokens;
     *                      if false, the lexer will skip comments
    */    
    Lexer(SourceID src_id, Allocator<std::string> &str_allocator, bool save_comments);

    /**
     * Destructor. Closes and frees src.
    */
    ~Lexer();

    /**
     * Client-facing lex function. Lexes a token from source.
     * @param tk the token to write into
    */
    void lex(Token &tk);

};

#endif