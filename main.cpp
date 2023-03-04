#include <iostream>
#include "lexer/lexer.h"
#include "lexer/token.h"
#include "lexer/tokentypes.h"
#include "lexer/source.h"

int main() {
    SourceFileManager file_manager = SourceFileManager();
    SourceFileID src_id = file_manager.add_source("input.txt");
    Lexer lexer = Lexer(file_manager, src_id, true);
    Token tk = Token();
    lexer.lex(tk);
    while (tk.get_type() != token::eof) {
        std::cout << tk.get_raw_line() << ":" << tk.get_raw_col() << ": " << *(std::string *)(tk.get_ptr()) << std::endl;
        if (tk.needs_dealloc()) {
            delete tk.get_ptr();
            tk.clear();
        }
        lexer.lex(tk);
    }
    return 0;
}