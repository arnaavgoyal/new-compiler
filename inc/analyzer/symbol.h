#ifndef SYMBOL_H
#define SYMBOL_H

#include <string>
#include "lexer/tokentypes.h"
#include "analyzer/type.h"

class Symbol {

    friend class SemanticAnalyzer;

private:

    /** symbol name */
    std::string name;
    
    /**
     * pointer to the type of this symbol
    */
    Type const *type_ptr;

    /**
     * Constructs a symbol with all fields as given.
    */
    Symbol(
        std::string const &name,
        Type const *type_ptr
    ) :
        name(name),
        type_ptr(type_ptr)
    { }

public:

    /** Default constructor */
    Symbol() { }

    /**
     * Prevent copying to maintain uniqueness of symbols.
    */
    Symbol(Symbol const &other) = delete;
    Symbol &operator=(Symbol const &other) = delete;

    std::string const &get_name() const { return name; }
    Type const *get_type_ptr() const { return type_ptr; }

};

#endif