#include "ir/analysis.h"
#include "utils/iterator.h"

std::vector<ir::Block *> predecessors(ir::Block *b) {

    // every predecessor to a block b has to use
    // a branch instr to b. Thus, the list of
    // predecessors to b is just the parents
    // of each user in its use list
    std::vector<ir::Block *> preds;
    //std::cout << "preds for " << b->get_name() << std::endl;
    for (ir::Use *u : b->uses_iterable()) {
        ir::Block *pred = static_cast<ir::Instr *>(u->get_user())->get_parent();
        preds.push_back(pred);
        //std::cout << "  found pred: " << pred->get_name() << "\n";
        //u->get_user()->dump(4);
    }
    //std::cout << "  done\n";
    return preds;
}

std::vector<ir::Block *> successors(ir::Block *b) {

    // every successor to a block b is reached
    // by b's terminator branch instr. Thus, the list
    // of successors to b is the block operands
    // of the branch instruction.
    ir::Instr *term = b->get_last_instr();
    assert(term->is_terminator() && "block is ill-formed -- no terminator instruction");
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

void CFG::dump(std::ostream &out) {
    out << "digraph {\n";
    for (edge_ty *e : edges) {
        out << "\t" << e->from->val->get_name()
            << " -> " << e->to->val->get_name() << std::endl;
    }
    out << "}" << std::endl;
}

CFG make_cfg(ir::Function *f) {
    CFG cfg(f->get_first_block());
    CFG::vertex_ty *v = nullptr;
    for (ir::Block *b : make_iterator_range(++f->begin(), f->end())) {
        if (!(v = cfg.get_vertex(b))) {
            v = cfg.add_vertex(b);
            cfg.add_edge(cfg.get_root(), v);
        }
        for (ir::Block *succ : successors(b)) {
            CFG::vertex_ty *u = cfg.add_vertex(succ);
            cfg.add_edge(v, u);
        }
    }
    return cfg;
}
