#include "ir/cfg.h"
#include "utils/iterator.h"

std::vector<ir::Block *> predecessors(ir::Block *b) {

    // every predecessor to a block b has to use
    // a branch instr to b. Thus, the list of
    // predecessors to b is just the parents
    // of each user in its use list
    std::vector<ir::Block *> preds;
    //std::cout << "preds for " << b->get_name() << std::endl;
    for (ir::Use *u : b->uses_iterable()) {
        auto instr = static_cast<ir::Instr *>(u->get_user());
        if (instr->get_kind() == ir::defkind::branch) {
            preds.push_back(instr->get_parent());
            //std::cout << "  found pred: " << instr->get_parent()->get_name() << "\n";
            //u->get_user()->dump(4);
        }
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
    if (term->get_kind() == ir::defkind::branch) {
        ir::BranchInstr *bi = static_cast<ir::BranchInstr *>(term);
        succs.push_back(bi->get_jmp_true());
        if (bi->is_conditional()) {
            succs.push_back(bi->get_jmp_false());
        }
    }
    return succs;
}

void dump_cfg(ir::Function *cfg, std::ostream &out) {
    out << "digraph {\n";
    for (auto b : *cfg) {
        for (auto succ : successors(b))
        out << "\t" << b->get_name()
            << " -> " << succ->get_name() << std::endl;
    }
    out << "}" << std::endl;
}
