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
    int pos;
    bool flag = false;
    //::cout << loc.end_offset - loc.start_offset << std::endl;
    int c = src->peek();
    while (c != '\n' && c != EOF) {
        pos = src->tellg();
        if (pos == loc.start_offset) {
            std::cout << RED;
            pre_offset += i;
            flag = true;
        }
        else if (pos == loc.end_offset + 1) {
            std::cout << RST;
            flag = false;
        }
        if (flag) {
            len++;
        }
        i++;
        std::cout << (char)src->get();
        c = src->peek();
    }
    len--;
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

void ErrorHandler::handle_missing(SourceLocation loc, char const *missing) {

    // print preamble
    print_error_preamble(loc);

    // print error
    std::cout << "expected " << missing << std::endl;

    // print loc highlight
    print_loc_highlight(loc);
}

void ErrorHandler::handle_redeclared(SourceLocation loc, char const *redeclared) {

    // print preamble
    print_error_preamble(loc);

    // print error
    std::cout << "redeclared " << redeclared << std::endl;

    // print loc highlight
    print_loc_highlight(loc);
}

void ErrorHandler::handle(error::error_type type, SourceLocation loc, char const *str) {

    // print preamble
    print_error_preamble(loc);

    // print error
    switch (type) {
        case error::missing:
            std::cout << "expected " << GRN << str << RST;
            break;
        case error::redeclared:
            std::cout << "redeclared " << YLW << str << RST;
            break;
        case error::unknown:
            std::cout << "unknown " << WHT << str << RST;
            break;
        case error::deprecated:
            std::cout << PRP << str << RST << " is deprecated";
            break;
        case error::nyi:
            std::cout << PRP << str << RST << " is not yet implemented";
            break;
        default:
            std::cout << WHT << str << RST;
            break;
    }
    
    std::cout << std::endl;

    // print loc highlight
    print_loc_highlight(loc);
}