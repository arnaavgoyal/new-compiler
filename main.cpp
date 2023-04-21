#include <iostream>
#include <string>
#include <iomanip>
#include "lexer/lexer.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "source/source.h"
#include "parser/ast.h"
#include "parser/parser.h"
#include "symbol/symbol.h"
#include "memory/allocator.h"
#include "error/error.h"

void test_lexer() {
#define LOC_NUM_WIDTH 2
    SourceID src_id = SourceManager::add_source("input.c");
    Allocator<std::string> *str_allocator = new Allocator<std::string>;
    Lexer lexer(src_id, *str_allocator, true);
    Token tk;
    SourceLocation loc;
    lexer.lex(tk);
    while (tk.get_type() != token::eof) {
        loc = tk.get_src_loc();
        std::cout
            << std::setfill('0')
            << SourceManager::get_source_path(loc.src_id) << " @ "
            << std::setw(LOC_NUM_WIDTH) << loc.start_row  << ":"
            << std::setw(LOC_NUM_WIDTH) << loc.start_col  << "::"
            << std::setw(LOC_NUM_WIDTH) << loc.end_row    << ":"
            << std::setw(LOC_NUM_WIDTH) << loc.end_col    << ": "
            << tk.get_print_str() << std::endl;
        lexer.lex(tk);
    }
    delete str_allocator;
    return;
#undef LOC_NUM_WIDTH
}

void test_parser() {
    SourceID src_id = SourceManager::add_source("input.c");
    Allocator<std::string> str_allocator;
    Lexer lexer(src_id, str_allocator, false);
    Allocator<ASTNode> node_allocator;
    Parser parser(lexer, node_allocator);
    ASTNode *tree = parser.parse();
    tree->print();
    return;
}

int main() {
    
    //test_lexer();

    test_parser();

    std::cout << "Done testing." << std::endl;

    return 0;
}