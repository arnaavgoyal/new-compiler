#include "codegen/codegen.h"
#include "codegen/x86_64_gen.h"
#include "utils/ioformat.h"
#include <queue>
#include <ostream>
#include <string>
#include <limits>
#include <cmath>
#include <cstring>

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

void x86_64CodeGen::ccvl(TargetFunction &tf, ir::Function *f) {
    // params passed on the stack in order
    unsigned curr_offset = 16; // old base ptr + return ptr
    for (auto p : f->params_iterable()) {
        auto ty = irty2tk(p->get_type()->get_kind());
        tf.params.push_back(TargetValue::stack(ty, StackData{ curr_offset, true }));
        curr_offset += tysizebytes(ty);
    }
}

bool x86_64CodeGen::isel(TargetFunction &tf, TargetInstr *ti) {

#define IRDKCASE(irdk) case irdk2opc(ir::defkind::irdk)
    switch (ti->opcode) {
    default:
        assert(false);
        break;
    IRDKCASE(ret): {
        // update opcode
        ti->opcode = opcode::ret;

        UseEdge &ue = ti->uses[0];
        assert(ue.val);

        // set use to implicit
        ue.implicit = true;

        // check if ret val is in rax
        if (ue.val->loc == storagekind::reg && ue.val->regdata.reg == reg::rax) {
            // already in rax, so done
            return true;
        }

        // generate a copy to rax and update the use-defs accordingly
        auto oldval = ue.val;
        assert(oldval->uses.size() > 0);
        auto copyinstr = new TargetInstr(opcode::mov);
        auto newval = TargetValue::reg(oldval->ty, RegData{ reg::rax });
        set_use(&ue, newval);
        add_use(copyinstr, oldval);
        add_def(copyinstr, newval);

        // add the new instr into the function
        tf.instrs.insert(ti, copyinstr);
        return true;
    }
    IRDKCASE(branch): {
        break;
    }
    IRDKCASE(salloc): {
        auto def = ti->defs.back();
        auto stackval = make_stack(def->ty, tf);
        for (auto ue : def->uses) {
            set_use(ue, stackval);
        }
        tf.instrs.remove(ti);
        return true;
    }
    IRDKCASE(read):
        ti->opcode = opcode::mov;
        return true;
    IRDKCASE(write):
        add_def(ti, ti->uses.back().val);
        rmv_use(ti, 1);
        ti->opcode = opcode::mov;
        return true;
    IRDKCASE(ptridx):
        break;
    IRDKCASE(typecast):
        break;
    IRDKCASE(iupcast):
        break;
    IRDKCASE(idowncast):
        break;
    IRDKCASE(iadd):
        ti->opcode = opcode::add;
        return true;
    IRDKCASE(isub):
        ti->opcode = opcode::sub;
        return true;
    IRDKCASE(imul):
        ti->opcode = opcode::imul;
        return true;
    IRDKCASE(idiv):
        break;
    IRDKCASE(icmp):
        break;
    IRDKCASE(call): {
        ti->opcode = opcode::call;
        auto olddef = ti->defs.back();
        auto newdef = TargetValue::reg(olddef->ty, RegData{ reg::rax });
        ti->defs.remove(olddef);
        add_def(ti, newdef);
        rauw(olddef, newdef);
        for (unsigned i = 1; i < ti->uses.size(); i++) {
            UseEdge &ue = ti->uses[i];
            auto arg = ue.val;
            auto mi = new TargetInstr(opcode::mov);
            std::cout << tf.str << ": " << tf.stacksize;
            auto argval = make_stack(arg->ty, tf);
            std::cout << " -> " << tf.stacksize << "\n";
            add_def(mi, argval);
            add_use(mi, ti->uses[i].val);
            set_use(&ue, argval, true);
            tf.instrs.insert(ti, mi);
        }
        return true;
    }
    IRDKCASE(phi):
        break;
    }
#undef IRDKCASE
    return false;
}

static constexpr unsigned reg2allocidx(reg r) {
    return r - reg::_reg_start - 1;
}

static constexpr reg allocidx2reg(unsigned idx) {
    return static_cast<reg>(idx + reg::_reg_start + 1);
}

static bool regalloced[num_pregs] = { false };

unsigned x86_64CodeGen::ralloc() {
    unsigned idx = 0;
    while (idx < num_pregs && regalloced[idx]) { idx++; }
    assert(idx < num_pregs && "stack spills nyi");
    regalloced[idx] = true;
    return allocidx2reg(idx);
}

void x86_64CodeGen::rfree(unsigned r) {
    assert(r > reg::_reg_start && r < reg::_reg_end && "not a valid reg");
    unsigned idx = reg2allocidx(static_cast<reg>(r));
    assert(regalloced[idx] && "freeing non-alloced reg?");
    regalloced[idx] = false;
}

void x86_64CodeGen::rfree_all() {
    std::memset(regalloced, false, num_pregs);
}

void x86_64CodeGen::parg(TargetValue *tv, std::ostream &os) {
    switch (tv->loc) {
    case storagekind::glob:
        os << "glob";
        break;
    case storagekind::imm:
        os << tv->immdata.val;
        break;
    case storagekind::label:
        os << tv->labeldata->str;
        break;
    case storagekind::reg:
        os << pregstr(tv->regdata.reg);
        break;
    case storagekind::stack:
        os << "[rbp " << (tv->stackdata.param ? "+ " : "- ") << tv->stackdata.offset << "]";
        break;
    case storagekind::inv:
    default:
        assert(false);
        break;
    }
}

void x86_64CodeGen::pasm(TargetProgram &tp, std::ostream &os) {

    // enable intel syntax (working with gcc)
    os << "\t.intel_syntax noprefix\n";
    
    // make funcs global
    for (auto &f : tp.funcs) {
        os << "\t.globl " << f.str << "\n";
    }

    // function bodies
    for (auto &f : tp.funcs) {

        // label
        os << f.str << ":\n";

        // prologue
        os << "\tpush\trbp\n"
            << "\tmov\trbp, rsp\n"
            << "\tsub\trsp, " << f.stacksize << "\n";

        // instructions
        for (auto i : f.instrs) {
            if (i->opcode == opcode::ret) break;
            os << "\t" << instrstr(i->opcode) << "\t";
            switch (i->opcode) {
            case opcode::add:
                assert(false);
                break;
            case opcode::call:
                parg(i->uses[0].val, os);
                break;
            case opcode::imul:
                parg(i->uses[0].val, os);
                os << ", ";
                parg(i->uses[1].val, os);
                break;
            case opcode::mov:
                parg(i->defs.front(), os);
                os << ", ";
                parg(i->uses[0].val, os);
                break;
            case opcode::ret:
                __builtin_unreachable();
            case opcode::sub:
                assert(false);
                break;
            default:
                assert(false);
                break;
            }
            os << "\n";
        }

        // epilogue
        os << "\tmov\trsp, rbp\n"
            << "\tpop\trbp\n";
        os << "\tret\n";
    }
}

std::string x86_64CodeGen::instrstr(unsigned opc) {
    assert(opc > opcode::_ir_opc_end);
    switch (opc) {

#define INSTR(val, str) case opcode::val: return str;
#include "codegen/x86_64_instr"

        default: break;
    }
    assert(false);
    __builtin_unreachable();
}

std::string x86_64CodeGen::pregstr(unsigned preg) {
    //assert(preg < reg::_subreg_end);
    switch (preg) {
    default: return std::string("%v:") + std::to_string(preg);

#define REG(val, bits) case reg::val: return #val;
#include "codegen/x86_64_reg"

    }
    assert(false);
    __builtin_unreachable();
}

bool x86_64CodeGen::is_preg(unsigned reg) {
    return reg >= regbound::_preg_start;
}

unsigned x86_64CodeGen::new_vreg() {
    static typename std::underlying_type<reg>::type next = regbound::_vreg_start;
    return next++;
}

bool match_impl(TargetInstr *ti, unsigned idx) {
    return true;
}

template <typename... OpLocs>
bool match_impl(TargetInstr *ti, unsigned idx, storagekind sk, OpLocs... oplocs) {
    if (idx >= ti->uses.size()) return false;
    if (ti->uses[idx].val->loc == sk) return match_impl(ti, idx + 1, oplocs...);
}

template <typename... OpLocs>
bool match(TargetInstr *ti, OpLocs... oplocs) {
    return match_impl(ti, 0, oplocs...);
}

}
