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
    ASTNode *n;
    ASTNode *tree = new ASTNode;
    tree->data = new std::string("Translation Unit");
    tree->type = ast::unknown;
    while ((n = parser.parse_stmt()) != nullptr) {
        tree->list.push_back(n);
        //std::cout << token::get_token_string(parser.tk.get_type()) << std::endl;
    }
    print_ast(tree);
    return 0;
}