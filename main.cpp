#include <iomanip>
#include <iostream>
#include <string>

#include "analyzer/analyzer.h"
#include "analyzer/type.h"
#include "ast/translate.h"
#include "ast/xast.h"
#include "codegen/codegen.h"
#include "codegen/x86_64_gen.h"
#include "diag/diagnostic.h"
#include "ir/cfg.h"
#include "ir/ir.h"
#include "ir/pass.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "utils/identifier.h"
#include "utils/ioformat.h"
#include "utils/memory.h"
#include "utils/source.h"

#define DEBUG

int main(int argc, char **argv) {

    // ---------------- FRONTEND ------------------

    RawRegionAllocator ra;
    StringPool strings(ra);
    Allocator<fe::xast::Node> nodes(ra);

    // intern the keyword strings
    for (auto kw : token::get_all_keywords()) {
        strings.add(token::get_keyword_string(kw));
    }

    SourceManager::init(ra);

    fe::Lexer lexer(SourceManager::get(argv[1]), strings, false);
    fe::Parser parser(lexer, nodes);

    auto [parse_success, ast] = parser.parse();
    fe::xast::dump(ast);
    std::cout << "\nparse status: ";
    std::cout.flush();
    if (parse_success) {
        std::cout << "success"; // ioformat::GREEN << "success" << ioformat::RESET;
    }
    else {
        std::cout << "failure"; // ioformat::RED << "failure" << ioformat::RESET;
    }
    std::cout << "\n";
    std::cout.flush();
    std::cout << "[Before DiagnosticHandler::dump]\n";
    std::cout.flush();
    // int num_errors = DiagnosticHandler::dump();
    int num_errors = 0;  // Skip dump for now
    std::cout << "[After setting num_errors]\n";
    std::cout.flush();
    std::cout << num_errors << " errors\n\n";
    std::cout << "[After printing errors]\n";
    std::cout.flush();
    if (!parse_success || num_errors) {
        exit(EXIT_FAILURE);
    }

    std::cout << "[About to call analyze]\n";
    std::cout.flush();    // analyze
    auto &primitives = fe::PrimitiveType::get_all();
    fe::analyze(ast, primitives);
    std::cout << "\n";
    num_errors = DiagnosticHandler::dump();
    std::cout << num_errors << " errors\n\n";
    if (num_errors) {
        exit(EXIT_FAILURE);
    }

    // // ---------------- MIDEND ------------------

    // ir::Program *prog = fe::ASTTranslator().translate(ast);
    // std::cout << std::endl;
    // prog->dump();

    // std::ofstream cfgfile("cfg.dot");
    // dump_cfg(prog->get_function("my_main"), cfgfile);
    // cfgfile.close();

    // run_stackpromotion(prog);
    // prog->dump();

    // // ---------------- BACKEND ------------------

    // // codegen
    // std::ofstream outfile(argv[2]);
    // be::x86_64CodeGen tcg;
    // be::codegen(prog, tcg, outfile);
    // outfile.close();


    return 0;
}
