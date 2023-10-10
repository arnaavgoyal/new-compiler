#include <iostream>
#include <string>
#include <iomanip>
#include "lexer/lexer.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "source/source.h"
#include "ast/ast.h"
#include "parser/parser.h"
#include "memory/allocator.h"
#include "error/error.h"
#include "analyzer/analyzer.h"
#include "analyzer/type.h"

#define DEBUG

int main() {
    
    // Make all necessary allocators
    Allocator<std::string> str_alloc;
    Allocator<ASTNode> node_alloc;
    Allocator<Type> type_alloc;
    Allocator<std::vector<Type *>> vec_alloc;

    // Add source to source manager
    SourceID src_id = SourceManager::add_source("input.a");

    // Make lexer
    Lexer lexer(src_id, str_alloc, /** save comments = */ false);

#ifdef DEBUG
    std::cout << "Lexer made\n";
#endif

    // Get list of primitive types
    std::vector<token::token_type> primitives = token::get_types_list();

#ifdef DEBUG
    std::cout << "Primitives made\n";
#endif

    // Make semantic analyzer
    SemanticAnalyzer analyzer(
        str_alloc,
        primitives
    );

#ifdef DEBUG
    std::cout << "Analyzer made\n";
#endif

    // Make parser
    Parser parser(
        lexer,
        analyzer
    );

#ifdef DEBUG
    std::cout << "Parser made\n";
#endif

    // parse
    ASTNode *ast = nullptr;
    bool parse_success = parser.parse(&ast);
    ast->print();
    std::cout << parse_success << std::endl;

    // dump errors
    int num_errors = ErrorHandler::dump();

    // fail if errors
    if (num_errors) {
        return EXIT_FAILURE;
    }

    // codegen

    return 0;
}