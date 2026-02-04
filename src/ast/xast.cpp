#include <cassert>
#include <iomanip>
#include <iostream>

#include "analyzer/type.h"
#include "utils/ioformat.h"
#include "utils/source.h"

#include "ast/xast.h"

static void print_err_loc_preamble(SourceLocation loc) {
    auto els = SourceManager::expand(loc);
#define LOC_NUM_WIDTH 2
    std::cout
        << std::setfill('0')
        << std::setw(LOC_NUM_WIDTH) << els.start_row  << ":"
        << std::setw(LOC_NUM_WIDTH) << els.start_col  << "::"
        << std::setw(LOC_NUM_WIDTH) << els.end_row    << ":"
        << std::setw(LOC_NUM_WIDTH) << els.end_col
        ;
#undef LOC_NUM_WIDTH
}

namespace fe {
namespace xast {

void Use::set(Node *n) {
    if (_used) _used->uses.remove(this);
    _used = n;
    if (_used) _used->uses.append(this);
}

bool is_decl(nk k) { return k > nk::__decl_start && k < nk::__decl_end; }

void dump(Node *node, std::string stem) {

    if (node != nullptr) {

        std::string kind_str;
        switch (node->kind) {

#define NK(kind, n, c) \
    case nk::kind: kind_str = #kind; break;
#include "ast/nodekinds"

        default:
            kind_str = "?";
            break;

#undef KC

        }

        std::cout << stem << "`" << kind_str;

        if (node->meta) {
            std::cout << ioformat::RED << "!";
        }

        std::cout << ioformat::YELLOW;
        if (node->data.is_ident()) {
            std::cout << " '" << *node->data.ident << "'";
        }
        else if (node->data.is_ival()) {
            std::cout << " " << node->data.ival;
        }
        else if (node->data.is_op()) {
            std::cout << " " << token::get_operator_string(node->data.op.tok);
        }

        if (node->type != nullptr) {
            std::cout << " "
                << ioformat::GREEN
                << node->type->stringify()
                << ioformat::BLUE << " "
                << node->type->get_canonical()->stringify();
        }
        // if (node->is_lvalue) {
        //     std::cout << ioformat::CYAN << " lval";
        // }
        std::cout << ioformat::PURPLE << " <";
        print_err_loc_preamble(node->sloc);
        std::cout << ">";
        // if (node->has_error) {
        //     std::cout << ioformat::RED << " {contains errors}";
        // }
        std::cout << ioformat::RESET << std::endl;
        stem.push_back(' ');
        stem.push_back('|');
        int size = node->opedges.size();
        for (int i = 0; i < size; i++) {
            if (i == size - 1) {
                stem.pop_back();
                stem.push_back(' ');
            }
            dump(node->opedges[i]->used(), stem);
        }
    }

    else {
        std::cout << stem << "`" << "null" << std::endl;
    }
}

} // xast
} // fe
