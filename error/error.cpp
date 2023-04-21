#include "error.h"
#include "source/source.h"
#include <iostream>
#include <iomanip>

void ErrorHandler::print_error_preamble(SourceLocation loc) {
    std::cout 
        << WHT
        << SourceManager::get_source_path(loc.src_id)
        << "::" << loc.start_row  << ":" << loc.start_col  << ": "
        << RED
        << "error: "
        << RST;
}

void ErrorHandler::set_stream_color(color col) {
    std::cout << col;
}

void ErrorHandler::set_stream_to_line_start(std::ifstream *stream, SourceLocation loc) {
    if (loc.start_row == 1) {
        stream->seekg(0, std::ios::beg);
        while (isspace(stream->peek())) {
            stream->get();
        }
    }
    else {
        int c = stream->peek();
        stream->seekg(loc.start_offset, std::ios::beg);
        while (c != '\n') {
            stream->seekg(-1, std::ios::cur);
            c = stream->peek();
        }
        stream->get();
    }
}

void ErrorHandler::print_loc_highlight(SourceLocation loc) {

    // get source file
    std::ifstream *src = SourceManager::open_source(loc.src_id);

    // set stream to line start
    set_stream_to_line_start(src, loc);

    // print code highlight
    int pre_offset = 8;
    int len = 0;
    int i = 0;
    std::cout << std::setfill(' ') << std::setw(5) << loc.start_row << " | ";
    std::streampos pos;
    int c = src->peek();
    while (c != '\n' && c != EOF) {
        pos = src->tellg();
        if (pos == loc.start_offset) {
            std::cout << RED;
            pre_offset += i;
        }
        else if (pos == loc.end_offset + 1) {
            std::cout << RST;
            len = i - pre_offset;
        }
        i++;
        std::cout << (char)src->get();
        c = src->peek();
    }
    std::cout << std::endl;

    // print underline
    for (int j = 0; j < pre_offset; j++) {
        putchar(' ');
    }
    std::cout << RED;
    putchar('^');
    for (int j = 0; j < len; j++) {
        putchar('~');
    }
    std::cout << RST << std::endl;

    // close and dealloc file
    src->close();
    delete src;
}

void ErrorHandler::handle_missing_expr(SourceLocation loc) {

    // print preamble
    print_error_preamble(loc);

    // print error
    std::cout << "expected expression" << std::endl;

    // print loc highlight
    print_loc_highlight(loc);
}

void ErrorHandler::handle_missing_token(SourceLocation loc, char const *tok) {

    // print preamble
    print_error_preamble(loc);

    // print error
    std::cout << "Expected " << tok << std::endl;

    // print loc highlight
    print_loc_highlight(loc);
}