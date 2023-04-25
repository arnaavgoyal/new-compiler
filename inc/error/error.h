#ifndef ERROR_H
#define ERROR_H

#include "source/source.h"
#include <vector>

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
        undeclared,
        unknown,
        nyi,
        deprecated
    };

}

class ErrorHandler {
private:

    struct Error {
        error::error_type type;
        SourceLocation loc;
        char const *str;
    };

    /** list of errors */
    static std::vector<Error> list;

    /**
     * Prints the error preamble.
     * @param loc
    */
    static void print_error_preamble(SourceLocation &loc);

    /**
     * Sets the given stream to the start of the current line (i.e. the next
     * character in the stream is the first char in the line).
     * 
     * @param stream
     * @param loc
    */
    static void set_stream_to_line_start(std::ifstream *stream, SourceLocation &loc);

    /**
     * Prints the pretty location highlight of error at location within
     * the source code.
     * @param loc
    */
    static void print_loc_highlight(SourceLocation &loc);

    /**
     * Prints an error.
    */
   static void print_error(Error &err);

public:

    /**
     * Handles an error.
     * 
     * @param type the type of error
     * @param loc the location of the error
     * @param missing string to print for what is missing
    */
    static void handle(error::error_type type, SourceLocation loc, char const *str);

    /**
     * Dumps all queued errors.
    */
   static void dump();

};

#endif