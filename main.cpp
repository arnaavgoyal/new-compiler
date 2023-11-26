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

    ir::Program p;

    std::string fname = "myfunc";
    ir::Function f(
        new ir::FunctionType(
            ir::get_void(),
            std::vector<ir::Type *>()
        ),
        ir::linkage::external,
        &p,
        fname
    );
    assert(f.get_parent() == &p);
    assert(f.get_name() == fname);
    assert(p.get_function(fname) == &f);

    std::string bname = "myblock";
    ir::Block b(&f, bname);
    assert(b.get_parent() == &f);
    assert(b.get_name() == bname);
    assert(&b == f.get_block(bname));

    ir::IntegralConstant *ic = ir::IntegralConstant::get(ir::get_i32(), 1);

    std::string riname = "myretinstr";
    ir::ReturnInstr ri(ic, &b, riname);
    assert(ri.get_parent() == &b);
    assert(ri.get_name() == riname);
    assert(b.get_instr(riname) == &ri);

    //reinterpret_cast<BinaryOpInstr *>(binop)->dump();
    //binop->dump();

    // ---------------- BACKEND ------------------

    // codegen
    std::ofstream outfile(argv[2]);
    outfile << "x\n";
    outfile.close();


    return 0;
}