#ifndef ERROR_H
#define ERROR_H

#include "source/source.h"

#define BLK "\e[0;90m"
#define RED "\e[0;91m"
#define GRN "\e[0;92m"
#define YLW "\e[0;93m"
#define BLU "\e[0;94m"
#define PRP "\e[0;95m"
#define CYA "\e[0;96m"
#define WHT "\e[0;97m"
#define RST "\e[0m"

namespace error {

    enum error_type {

        // syntax errors
        missing_expr,
        missing_token,

    };

}

class ErrorHandler {
private:

    using color = char const *;



    static void print_error_preamble(SourceLocation loc);
    static void set_stream_color(color col);

    /**
     * Sets the given stream to the start of the current line (i.e. the next
     * character in the stream is the first char in the line).
     * 
     * @param stream
     * @param loc
    */
    static void set_stream_to_line_start(std::ifstream *stream, SourceLocation loc);

    static void print_loc_highlight(SourceLocation loc);

public:

    /**
     * Handles a missing expr error.
     * 
     * @param type the type of error
     * @param loc the location of the error
    */
    static void handle_missing_expr(SourceLocation loc);

    /**
     * Handles a missing token error.
     * 
     * @param type the type of error
     * @param loc the location of the error
     * @param tok string to print for token
    */
    static void handle_missing_token(SourceLocation loc, char const *tok);

};

#endif