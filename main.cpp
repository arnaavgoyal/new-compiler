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
#include "ast/translate.h"
#include "ir/ir.h"

#define DEBUG

// class haha;

// class hehe : public STPPIListNode<hehe, haha> {
// public:
//     hehe() { set_parent(nullptr); }
// };

// class haha {
//     STPPIList<hehe, haha> list;
// };

int main(int argc, char **argv) {

    // ---------------- FRONTEND ------------------
    
    // Make all necessary allocators
    Allocator<std::string> str_alloc;
    Allocator<ASTNode> node_alloc;
    Allocator<Type> type_alloc;
    Allocator<std::vector<Type *>> vec_alloc;

    // Add source to source manager
    SourceID src_id = SourceManager::add_source(argv[1]);

    // Make lexer
    Lexer lexer(src_id, str_alloc, /** save comments = */ false);

    // Get list of primitive types
    std::vector<token::token_type> primitives = token::get_types_list();

    // Make semantic analyzer
    SemanticAnalyzer analyzer(
        str_alloc,
        primitives
    );

    // Make parser
    Parser parser(
        lexer,
        analyzer
    );

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

    // ---------------- MIDEND ------------------

    ir::Program *translated_ast = ASTTranslator().translate(ast);
    std::cout << std::endl;
    translated_ast->dump();

    // ---------------- BACKEND ------------------

    // codegen
    std::ofstream outfile(argv[2]);
    outfile << "x\n";
    outfile.close();


    return 0;
}