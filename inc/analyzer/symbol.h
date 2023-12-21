#ifndef SYMBOL_H
#define SYMBOL_H

#include <string>
#include "lexer/tokentypes.h"
#include "analyzer/type.h"

namespace fe {

class Scope;
class ASTNode;

class Symbol {

    friend class SemanticAnalyzer;

public:

    /** symbol name */
    std::string name;
    
    /**
     * pointer to the type of this symbol
    */
    Type *type_ptr;

    Scope *scope;

    ASTNode *decl;

    /**
     * Constructs a symbol with all fields as given.
    */
    Symbol(
        std::string &name,
        Type *type_ptr
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
    Symbol(Symbol &other) = delete;
    Symbol &operator=(Symbol &other) = delete;

    std::string const &get_name() { return name; }
    Type *get_type_ptr() { return type_ptr; }

};

}

#endif