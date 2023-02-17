#include <cstdio>
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
        printf("%s\n", (char const *)tk.get_ptr());
        lexer.lex(tk);
    }
}