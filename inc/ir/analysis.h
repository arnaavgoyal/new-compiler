#ifndef IR_ANALYSIS_H
#define IR_ANALYSIS_H

#include <vector>
#include <map>
#include <deque>
#include "ir/ir.h"

std::vector<ir::Block *> predecessors(ir::Block *b) {

    // every predecessor to a block b has to use
    // a branch instr to b. Thus, the list of
    // predecessors to b is just the parents
    // of each user in its use list
    std::vector<ir::Block *> preds;
    for (Use *u : b->uses_iterable()) {
        preds.push_back(static_cast<Instr *>(u->get_user())->get_parent());
    }
}

std::vector<ir::Block *> successors(ir::Block *b) {

    // every successor to a block b is reached
    // by b's terminator branch instr. Thus, the list
    // of successors to b is the block operands
    // of the branch instruction.
    Instr *term = b->get_terminator();
    assert(term && "block is ill-formed -- no terminator instruction");
    std::vector<ir::Block *> succs;
    if (term->get_instr_kind() == ir::instr::branch) {
        ir::BranchInstr *bi = static_cast<ir::BranchInstr *>(term);
        succs.push_back(bi->get_jmp_true());
        if (bi->is_conditional()) {
            succs.push_back(bi->get_jmp_false());
        }
    }
    return succs;
}

// dominance tree and IDF calculation based on:
// https://dl.acm.org/doi/pdf/10.1145/199448.199464

template <typename Inner>
struct DominatorGraphVertex {
    using adjlist_ty = std::vector<DominatorGraphVertex *>;
    adjlist_ty in;
    adjlist_ty out;
    Inner val;
};

template <typename Inner>
class DominatorGraph {
public:
    using vertex_ty = DominatorGraphVertex<Inner>;
    using vertex_map_ty = std::map<Inner, vertex_ty *>;
    vertex_map_ty vertices;
    vertex_ty *root;
};

DominatorGraph<ir::Block *> make_dom_graph(ir::Function *f) {
    for (ir::Block *b : f) {
        for (ir::Block *succ : successors(b)) {
            
        }
    }
}

#endif
