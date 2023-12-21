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
#include "codegen/codegen.h"
#include "ir/cfg.h"
#include "ir/pass.h"
#include "utils/ioformat.h"

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
    Allocator<fe::ASTNode> node_alloc;
    Allocator<fe::Type> type_alloc;
    Allocator<std::vector<fe::Type *>> vec_alloc;

    // Add source to source manager
    SourceID src_id = SourceManager::add_source(argv[1]);

    // Make lexer
    Lexer lexer(src_id, str_alloc, /** save comments = */ false);

    // Get list of primitive types
    std::vector<token::token_type> primitives = token::get_types_list();

    // Make semantic analyzer
    fe::SemanticAnalyzer analyzer(
        str_alloc,
        primitives
    );

    // Make parser
    fe::Parser parser(
        lexer,
        analyzer
    );

    // parse
    fe::ASTNode *ast = nullptr;
    bool parse_success = parser.parse(&ast);
    ast->print();
    std::cout << "\nparse status: ";
    if (parse_success) {
        std::cout << ioformat::GREEN << "success" << ioformat::RESET;
    }
    else {
        std::cout << ioformat::RED << "failure" << ioformat::RESET;
    }
    std::cout << "\n\n";

    // dump errors
    // std::cout << "\n-------- ErrorHandler --------\n";
    // int num_errors = ErrorHandler::dump();
    // std::cout << "\n-------- DiagnosticHandler --------\n";
    int num_errors = DiagnosticHandler::dump();
    std::cout << num_errors << " errors\n\n";

    // fail if errors
    if (num_errors) {
        exit(EXIT_FAILURE);
    }

    if (!parse_success) {
        exit(EXIT_FAILURE);
    }

    // ---------------- MIDEND ------------------

    ir::Program *prog = fe::ASTTranslator().translate(ast);
    std::cout << std::endl;
    //prog->dump();

    std::ofstream cfgfile("cfg.dot");
    dump_cfg(prog->get_function("main"), cfgfile);
    cfgfile.close();

    run_stackpromotion(prog->get_function("foo"));
    //prog->dump();

    run_stackpromotion(prog->get_function("main"));
    prog->dump();

    // ---------------- BACKEND ------------------

    // codegen
    std::ofstream outfile(argv[2]);
    outfile << "x\n";
    outfile.close();


    return 0;
}