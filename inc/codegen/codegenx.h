#ifndef CODEGEN_CODEGENX_H
#define CODEGEN_CODEGENX_H

#include <limits>
#include <queue>

#include "codegen/codegen.h"
#include "utils/ioformat.h"
#include "ir/cfg.h"

namespace be {

enum reg : unsigned {

    _reg_start = _preg_start,

#define REG(val, bits) val,
#define SUBREG(val, bits, parent)
#include "codegen/x86_64_reg"

    _reg_end,

#define REG(val, bits)
#define SUBREG(val, bits, parent) val,
#include "codegen/x86_64_reg"

    _subreg_end
};
static_assert(_subreg_end <= std::numeric_limits<std::underlying_type<reg>::type>::max());

static constexpr unsigned num_pregs = reg::_reg_end - _reg_start - 1;

enum opcode : unsigned {

    _ir_opc_end = irdk2opc(ir::defkind::_instr_end),

#define INSTR(val, str) val,
#include "codegen/x86_64_instr"

    _num_ops
};

bool is_preg(unsigned reg) {
    return reg >= regbound::_preg_start;
}

std::string instrstr(unsigned opc) {
    assert(opc > opcode::_ir_opc_end);
    switch (opc) {

#define INSTR(val, str) case opcode::val: return str;
#include "codegen/x86_64_instr"

        default: break;
    }
    assert(false);
    __builtin_unreachable();
}

std::string pregstr(unsigned preg) {
    //assert(preg < reg::_subreg_end);
    switch (preg) {
    default: return std::string("%v:") + std::to_string(preg);

#define REG(val, bits) case reg::val: return #val;
#include "codegen/x86_64_reg"

    }
    assert(false);
    __builtin_unreachable();
}

struct cgxRegData {
    unsigned reg;
};

struct cgxStackData {
    unsigned offset;
    bool param;
};

struct cgxGlobData {
    ir::GlobalVar *glob;
};

struct cgxImmData {
    uint64_t val;
};

struct cgxLabelData {
    ir::Block *label;
};

struct cgxValue;
struct cgxInstr;

struct cgxUseEdge {
    cgxValue *val = nullptr;
    cgxInstr *instr = nullptr;
    bool implicit = false;

    cgxUseEdge(cgxValue *v, cgxInstr *i, bool imp)
        : val(v), instr(i), implicit(imp) { }
    
    cgxUseEdge(cgxValue *v, cgxInstr *i)
        : cgxUseEdge(v, i, false) { }
};

struct cgxValue : public IListNode<cgxValue> {
    typekind ty;
    storagekind loc;
    union {
        cgxRegData regdata;
        cgxStackData stackdata;
        cgxGlobData globdata;
        cgxImmData immdata;
        cgxLabelData labeldata;
    };
    IList<cgxUseEdge> uses;
};

struct cgxInstr : public IListNode<cgxInstr> {
    unsigned opcode;
    ir::Instr *orig;
    std::vector<cgxUseEdge> uses;
    IList<cgxValue> defs;

    cgxInstr(unsigned opcode, ir::Instr *orig = nullptr)
        : opcode(opcode), orig(orig) { }
};

struct cgxBlock {
    std::string label;
    std::vector<cgxInstr> instrs;
};

struct cgxFunc {
    std::string name;
    std::vector<cgxInstr *> graphs;
};

struct cgxProg {
    std::map<ir::GlobalVar *, cgxValue> globs;
    std::vector<cgxFunc> funcs;
};

cgxInstr *make_graph(ir::Block *b, cgxProg &cgp) {
    std::map<ir::Def *, bool> done;
    std::queue<ir::Def *> waiting;

    ir::Instr *term = b->get_last_instr();
    assert(term->is_terminator());

    waiting.push(term);

    while (waiting.size() != 0) {
        ir::Def *d = waiting.front();
        waiting.pop();
        if (d->is_instr()) {

        }
        else if (d->get_kind() == ir::defkind::globalvar) {

        }
        else if (d->get_kind() == ir::defkind::)
    }

}

void codegenx(ir::Program *p, std::ostream &os) {

    cgxProg cgp;

    // 1. populate globals

    for (ir::GlobalVar *gv : make_iterator_range(p->gvar_begin(), p->gvar_end())) {
        cgxValue cggv;
        cggv.loc = storagekind::glob;
        cggv.globdata = { gv };
        cgp.globs[gv] = cggv;
    }

    // 2. create graphs

    for (ir::Function *f : *p) {
        cgxFunc &cgf = cgp.funcs.emplace_back();
        cgf.name = f->get_name();

        std::queue<ir::Block *> waiting;
        std::map<ir::Block *, bool> done;
        assert(f->get_first_block());
        waiting.push(f->get_first_block());
        while (waiting.size() != 0) {
            ir::Block *curr = waiting.front();
            waiting.pop();
            done[curr] = true;
            cgf.graphs.push_back(make_graph(curr, cgp));
            for (auto succ : successors(curr)) {
                if (!done[succ]) {
                    waiting.push(succ);
                }
            }
        }
    }
    
    std::cout << std::endl;
    dbgprint(tp, std::cout);
    std::cout << std::endl;

    // lower calls

}

}

#endif
