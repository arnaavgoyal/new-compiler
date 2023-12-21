#include "error/error.h"
#include "source/source.h"
#include "utils/ioformat.h"
#include <iostream>
#include <iomanip>
#include <ctype.h>
#include <utility>

static void print_loc_prefix(SourceLocation &loc) {
    std::cout 
        << ioformat::WHITE
        << SourceManager::get_source_path(loc.src_id)
        << "::" << loc.start_row  << ":" << loc.start_col  << ": "
        << ioformat::RESET;
}

static void set_filestream_to_line_start(std::ifstream *stream, SourceLocation &loc) {
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

static void print_loc_highlight(SourceLocation &loc, char const *hc) {

    // get source file
    std::ifstream *src = SourceManager::open_source(loc.src_id);

    // set stream to line start
    set_filestream_to_line_start(src, loc);

    // print code highlight
    unsigned pre_offset = 8;
    unsigned len = 0;
    unsigned i = 0;
    std::cout << std::setfill(' ') << std::setw(5) << loc.start_row << " | ";
    unsigned pos;
    bool flag = false;
    //::cout << loc.end_offset - loc.start_offset << std::endl;
    int c = src->peek();
    while (c != '\n' && c != EOF) {
        pos = src->tellg();
        if (pos == loc.start_offset) {
            std::cout << hc;
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
    for (unsigned j = 0; j < pre_offset; j++) {
        putchar(' ');
    }
    std::cout << hc;
    putchar('^');
    for (unsigned j = 0; j < len; j++) {
        putchar('~');
    }
    std::cout << ioformat::RESET << std::endl;

    // close and dealloc file
    src->close();
    delete src;
}

void DiagnosticHandler::print_diag(Diagnostic &diag) {

    // print location prefix
    print_loc_prefix(diag.locs[0]);

    char const *highlight_col = ioformat::GREEN;

    // print the diag severity and choose highlight color
    switch (diag.sev) {
    case diag::severity::fatal:
    case diag::severity::error:
        highlight_col = ioformat::RED;
        std::cout << highlight_col
            << "error: "
            << ioformat::RESET;
        break;
    case diag::severity::note:
        highlight_col = ioformat::CYAN;
        std::cout << highlight_col
            << "note: "
            << ioformat::RESET;
        break;
    }

    // print error
    std::cout << ioformat::WHITE << diag.finalstr << ioformat::RESET << std::endl;

    // print loc highlight
    print_loc_highlight(diag.locs[0], highlight_col);
}

static void fmt(std::string &finalstr, char const *formatstr, char const *formatend, std::vector<std::string> &args) {
start:
    auto formatstart = formatstr;
    while (*formatstr != '%' && formatstr != formatend) {
        formatstr++;
    }
    finalstr.append(formatstart, formatstr - formatstart);
    if (formatstr == formatend) {
        return;
    }

    assert(*formatstr == '%' && "not at end of format string but also not at a '%'?");

    formatstr++;

    assert(isdigit(*formatstr) && "char after '%' is not a digit");

    unsigned char idx = *formatstr - '0';
    finalstr.append(args[idx]);

    formatstr++;

    if (formatstr != formatend) {
        goto start;
    }
}

namespace diag {

RawDiagnostic get(id diag_id) {
    switch (diag_id) {

#define DIAGNOSTIC(name, sev, str) case id::name: return RawDiagnostic{severity::sev, str};
#include "error/diagdefs"

        case id::__end:
        default:
            break;
    }
    assert(false && "unreachable");
    return RawDiagnostic();
}

}

void Diagnostic::fmt() {
    ::fmt(finalstr, formatstr, formatstr + strlen(formatstr), args);
}

void DiagnosticBuilder::finish() {
    // std::cout << "finishing diag\n";
    // std::cout << "  args:\n";
    // for (auto &str : d.args) {
    //     std::cout << "    " << str << std::endl;
    // }
    // std::cout << "  locs:\n";
    // for (auto &loc : d.locs) {
    //     std::cout << "    ";
    //     loc.print();
    //     std::cout << std::endl;
    // }
    valid = false;
    // std::cout << "  formatting\n";
    d.fmt();
    // std::cout << "    done\n";
    // std::cout << "  final: " << d.finalstr << std::endl;
    // std::cout << "  handling\n";
    DiagnosticHandler::handle(std::move(d));
    // std::cout << "    done\n";
}

std::vector<Diagnostic> DiagnosticHandler::diags;

int DiagnosticHandler::dump() {
    for (Diagnostic &diag : diags) {
        print_diag(diag);
    }
    return diags.size();
}

void DiagnosticHandler::prog_exit() {
    dump();
    exit(EXIT_FAILURE);
}
