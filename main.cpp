#include <iostream>
#include "lexer/lexer.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "source/source.h"
#include "parser/ast.h"
#include "parser/parser.h"

int main() {
    SourceFileManager file_manager;
    SourceFileID src_id = file_manager.add_source("input.txt");
    Lexer lexer(file_manager, src_id, true);
    Parser parser(lexer);
    ASTNode *tree;
    while ((tree = parser.parse()) != nullptr) {
        print_ast(tree);
    }
    return 0;
}