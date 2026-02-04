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
    // fe::xast::dump(ast);
    std::cout << "\nparse status: ";
    std::cout.flush();
    if (parse_success) {
        std::cout << ioformat::GREEN << "success" << ioformat::RESET;
    }
    else {
        std::cout << ioformat::RED << "failure" << ioformat::RESET;
    }
    std::cout << "\n";
    // fe::xast::dump(ast);
    int num_errors = DiagnosticHandler::dump();
    if (!parse_success || num_errors) {
        exit(EXIT_FAILURE);
    }

    auto &primitives = fe::PrimitiveType::get_all();
    fe::analyze(ast, primitives, strings);
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
