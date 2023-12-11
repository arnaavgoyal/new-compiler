#include "error/error.h"
#include "source/source.h"
#include "utils/ioformat.h"
#include <iostream>
#include <iomanip>

std::vector<ErrorHandler::Error> ErrorHandler::list = std::vector<Error>();

void ErrorHandler::print_error_preamble(SourceLocation &loc) {
    std::cout 
        << ioformat::WHITE
        << SourceManager::get_source_path(loc.src_id)
        << "::" << loc.start_row  << ":" << loc.start_col  << ": "
        << ioformat::RED
        << "error: "
        << ioformat::RESET;
}

void ErrorHandler::set_stream_to_line_start(std::ifstream *stream, SourceLocation &loc) {
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

void ErrorHandler::print_loc_highlight(SourceLocation &loc) {

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
            std::cout << ioformat::RED;
            pre_offset += i;
            flag = true;
        }
        else if (pos == loc.end_offset + 1) {
            std::cout << ioformat::RESET;
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
    std::cout << ioformat::RED;
    putchar('^');
    for (int j = 0; j < len; j++) {
        putchar('~');
    }
    std::cout << ioformat::RESET << std::endl;

    // close and dealloc file
    src->close();
    delete src;
}

void ErrorHandler::print_error(Error &err) {
    // print preamble
    print_error_preamble(err.loc);

    // print error
    switch (err.type) {

        case error::missing:
            std::cout << "expected " << ioformat::GREEN << err.str << ioformat::RESET;
            break;

        case error::ident_is_not_a_typename:
            std::cout << ioformat::GREEN << err.str << ioformat::RESET << " is not a typename";
            break;

        case error::deprecated:
            std::cout << ioformat::CYAN << err.str << ioformat::RESET << " is deprecated";
            break;
        case error::nyi:
            std::cout << ioformat::BLUE << err.str << ioformat::RESET << " are not yet implemented";
            break;

        default:
            std::cout << ioformat::WHITE << err.str << ioformat::RESET;
            break;
    }
    
    std::cout << std::endl;

    // print loc highlight
    print_loc_highlight(err.loc);
}

void ErrorHandler::handle(error::error_type type, SourceLocation loc, char const *str) {

    // add error to list
    Error err = {type, loc, str};
    list.push_back(err);

    //print_error(err);
}

int ErrorHandler::dump() {

    // print all errors
    for (std::vector<Error>::iterator i = list.begin(); i != list.end(); i++) {
        print_error(*i);
    }

    return list.size();
}