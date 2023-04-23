#ifndef TOKEN_H
#define TOKEN_H

#include "lexer/tokentypes.h"
#include "source/source.h"

class Token {

    // Lexer writes directly into str when creating tokens
    friend class Lexer;
    
private:

    /** token type */
    token::token_type type;
    
    /** source location of token */
    SourceLocation loc;

    /**
     * data is different based on type:
     * a) identifier     --  (std::string *)     owned by Allocator
     * b) literal        --  (std::string *)     owned by Allocator
     * c) operator       --  nullptr (use token::get_operator_string())
     * c) keyword        --  nullptr (use token::get_keyword_string())
    */
    std::string *str;

    /**
     * Constructs a token with given fields.
     * 
     * @param type the token type
     * @param src_loc the source location of the token
    */
    Token(
        token::token_type type,
        SourceLocation src_loc
    ) { set(type, src_loc); }

    /**
     * Set all token fields to the given fields.
     * 
     * @param type the token type
     * @param src_loc the source location of the token
    */
    void set(
        token::token_type type,
        SourceLocation src_loc
    ) { this->type = type; loc = src_loc; }

    /**
     * Clears token to default unknown token.
    */
    void clear();

public:

    /**
     * Constructs an unknown token with invalid source location.
    */
    Token();

    /**
     * Forbid copy to preserve uniqueness of tokens.
    */
    Token(Token const &other) = delete;
    Token &operator=(Token const &other) = delete;

    token::token_type get_type() { return type; }
    SourceLocation get_src_loc() { return loc; }
    std::string const *get_identifier_str() { return str; }
    std::string const *get_literal_str() { return str; }
    char const *get_operator_str() { return token::get_operator_string(type); }
    char const *get_keyword_str() { return token::get_keyword_string(type); }

    /**
     * Gets token name as string for debugging purposes.
    */
    char const *get_print_str();

};

#endif