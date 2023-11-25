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
#include "ir/ir.h"

#define DEBUG

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

    ir::Program p;
    std::cout << "made p" << std::endl;
    ir::Function f(
        new ir::FunctionType(
            ir::get_void(),
            std::vector<ir::Type const *>()
        ),
        ir::linkage::external, &p
    );
    std::cout << "made f" << std::endl;
    ir::Block b(&f);
    std::cout << "made b" << std::endl;

    ir::Program binop = ast2ir(ast);
    //reinterpret_cast<BinaryOpInstr *>(binop)->dump();
    //binop->dump();

    // ---------------- BACKEND ------------------

    // codegen
    std::ofstream outfile(argv[2]);
    outfile << "x\n";
    outfile.close();


    return 0;
}