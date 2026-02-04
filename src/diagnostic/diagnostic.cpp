#include <ctype.h>
#include <iomanip>
#include <iostream>
#include <map>
#include <utility>

#include "diag/diagnostic.h"
#include "utils/ioformat.h"
#include "utils/source.h"


static void print_loc_prefix(ExpandedSourceLocation &loc) {
    std::cout 
        << ioformat::WHITE
        << loc.src->path
        << "::" << loc.start_row  << ":" << loc.start_col  << ": "
        << ioformat::RESET;
}

static void print_loc_highlight(std::vector<ExpandedSourceLocation> &locs, char const *hc) {

    if (locs.empty()) return;

    // All locations should be on the same line
    unsigned line_num = locs[0].start_row;
    
    // print code highlight
    std::cout << std::setfill(' ') << std::setw(5) << line_num << " | ";
    unsigned pos = locs[0].src->lines[line_num-1];
    int c = locs[0].src->content[pos];
    
    while (c != '\n' && c != EOF) {
        bool in_range = false;
        for (auto &loc : locs) {
            if (pos >= loc.start_offset && pos <= loc.end_offset) {
                in_range = true;
                break;
            }
        }
        
        if (in_range) {
            std::cout << hc;
        }
        
        std::cout << (char)c;
        
        if (in_range) {
            std::cout << ioformat::RESET;
        }
        
        c = locs[0].src->content[++pos];
    }
    std::cout << std::endl;

    // Build underline with all carets/tildes on one line
    std::cout << "        "; // 8 spaces to align with line number column
    
    unsigned line_start = locs[0].src->lines[line_num-1];
    pos = line_start;
    c = locs[0].src->content[pos];
    
    while (c != '\n' && c != EOF) {
        bool is_caret = false;
        bool is_tilde = false;
        
        // Check if this is the start of any location (caret)
        for (auto &loc : locs) {
            if (pos == loc.start_offset) {
                is_caret = true;
                std::cout << hc << "^";
                break;
            }
        }
        
        // If not a caret, check if it's in a range (tilde)
        if (!is_caret) {
            for (auto &loc : locs) {
                if (pos > loc.start_offset && pos <= loc.end_offset) {
                    is_tilde = true;
                    std::cout << hc << "~";
                    break;
                }
            }
        }
        
        // If neither, print space
        if (!is_caret && !is_tilde) {
            putchar(' ');
        }
        
        if (is_caret || is_tilde) {
            std::cout << ioformat::RESET;
        }
        
        c = locs[0].src->content[++pos];
    }
    
    std::cout << std::endl;
}

void DiagnosticHandler::print_diag(Diagnostic &diag) {

    char const *highlight_col = ioformat::GREEN;
    char const *diag_ty_str = nullptr;

    // print the diag severity and choose highlight color
    switch (diag.sev) {
    case diag::severity::fatal:
    case diag::severity::error:
        highlight_col = ioformat::RED;
        diag_ty_str = "error";
        break;
    case diag::severity::warning:
        highlight_col = ioformat::PURPLE;
        diag_ty_str = "warning";
        break;
    case diag::severity::note:
        highlight_col = ioformat::CYAN;
        diag_ty_str = "note";
        break;
    }

    // group locations by line number
    std::map<unsigned, std::vector<ExpandedSourceLocation>> lines_map;
    std::vector<unsigned> line_order; // track order of first appearance
    
    for (auto &loc : diag.locs) {
        auto esl = SourceManager::expand(loc);
        unsigned line = esl.start_row;
        if (lines_map.find(line) == lines_map.end()) {
            line_order.push_back(line);
        }
        lines_map[line].push_back(esl);
    }

    // print each line with its locations
    for (size_t i = 0; i < line_order.size(); i++) {
        unsigned line = line_order[i];
        auto &locs_on_line = lines_map[line];
        
        // print location prefix for the first location on this line
        print_loc_prefix(locs_on_line[0]);

        // print error message only on the first line
        if (i == 0) {
            std::cout << highlight_col << diag_ty_str << ": "
                    << ioformat::WHITE << diag.finalstr
                    << ioformat::RESET << std::endl;
        } else {
            std::cout << std::endl;
        }

        // print all highlights for this line
        print_loc_highlight(locs_on_line, highlight_col);
    }
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
#include "diag/diagdefs"

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

unsigned DiagnosticHandler::dump() {
    unsigned num_errors = 0;
    for (Diagnostic &diag : diags) {
        print_diag(diag);
        if (diag.sev < diag::severity::_uncompilable) {
            num_errors++;
        }
    }
    return num_errors;
}

void DiagnosticHandler::prog_exit() {
    dump();
    exit(EXIT_FAILURE);
}
