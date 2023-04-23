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

        missing,
        redeclared,
        unknown,
        nyi,
        deprecated
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
     * Handles a missing (expected __) error.
     * 
     * @param type the type of error
     * @param loc the location of the error
     * @param missing string to print for what is missing
    */
    static void handle_missing(SourceLocation loc, char const *missing);

    static void handle_redeclared(SourceLocation loc, char const *redeclared);

    static void handle(error::error_type type, SourceLocation loc, char const *str);

};

#endif