#include <iostream>
#include <string>
#include <iomanip>
#include "lexer/lexer.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "source/source.h"
#include "parser/ast.h"
#include "parser/parser.h"
#include "memory/allocator.h"
#include "error/error.h"
#include "analyzer/analyzer.h"
#include "parser/type.h"

//#define DEBUG

int main() {
    
    // Make all necessary allocators
    Allocator<std::string> str_alloc;
    Allocator<ASTNode> node_alloc;
    Allocator<Type> type_alloc;
    Allocator<std::vector<Type *>> vec_alloc;

    // Add source to source manager
    SourceID src_id = SourceManager::add_source("input.c");

    // Make lexer
    Lexer lexer(src_id, str_alloc, false);

#ifdef DEBUG
    std::cout << "Lexer made\n";
#endif

    // Make symbol table
    SymbolTable symtable;

#ifdef DEBUG
    std::cout << "Symbol table made\n";
#endif

    // Get list of primitive types
    std::vector<token::token_type> primitives = token::get_types_list();

#ifdef DEBUG
    std::cout << "Primitives made\n";
#endif

    // Make semantic analyzer
    SemanticAnalyzer analyzer(symtable);

#ifdef DEBUG
    std::cout << "Analyzer made\n";
#endif

    // Make parser
    Parser parser(
        lexer,
        analyzer,
        node_alloc,
        type_alloc,
        str_alloc,
        vec_alloc,
        primitives
    );

#ifdef DEBUG
    std::cout << "Parser made\n";
#endif

    // Get ast
    ASTNode *tree = parser.parse();

    // print ast
    tree->print();

    // dump errors
    ErrorHandler::dump();

#ifdef DEBUG
    std::cout << "Done testing." << std::endl;
#endif

    return 0;
}