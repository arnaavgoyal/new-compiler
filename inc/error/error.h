#ifndef ERROR_H
#define ERROR_H

#include "stdlib.h"
#include "source/source.h"
#include <vector>

namespace error {

    enum error_type {

        // syntax errors
        missing,

        // semantic errors
        redeclaration_of_symbol_in_same_scope,
        ident_is_not_a_typename,
        mismatch_between_func_type_and_param_list,

        // dev errors
        deprecated,
        nyi
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

    static void prog_exit() { dump(); exit(EXIT_FAILURE); }

};

#endif