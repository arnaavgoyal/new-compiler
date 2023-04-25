#include "error/error.h"
#include "source/source.h"
#include <iostream>
#include <iomanip>

std::vector<ErrorHandler::Error> ErrorHandler::list = std::vector<Error>();

void ErrorHandler::print_error_preamble(SourceLocation &loc) {
    std::cout 
        << WHT
        << SourceManager::get_source_path(loc.src_id)
        << "::" << loc.start_row  << ":" << loc.start_col  << ": "
        << RED
        << "error: "
        << RST;
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

void ErrorHandler::print_error(Error &err) {
    // print preamble
    print_error_preamble(err.loc);

    // print error
    switch (err.type) {
        case error::missing:
            std::cout << "expected " << GRN << err.str << RST;
            break;
        case error::redeclared:
            std::cout << "redeclared " << YLW << err.str << RST;
            break;
        case error::undeclared:
            std::cout << CYA << err.str << RST << " is not declared";
            break;
        case error::unknown:
            std::cout << "unknown " << WHT << err.str << RST;
            break;
        case error::deprecated:
            std::cout << PRP << err.str << RST << " is deprecated";
            break;
        case error::nyi:
            std::cout << PRP << err.str << RST << " is not yet implemented";
            break;
        default:
            std::cout << WHT << err.str << RST;
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

void ErrorHandler::dump() {

    // print all errors
    for (std::vector<Error>::iterator i = list.begin(); i != list.end(); i++) {
        print_error(*i);
    }
}