#ifndef CODEGEN_CODEGEN_H
#define CODEGEN_CODEGEN_H

#include <iostream>
#include "ir/ir.h"

namespace be {

/** enums */

enum class typekind {
    s8,
    s16,
    s32,
    s64,
    sfp,
    dfp
};

enum class storagekind {
    reg,
    stack,
    glob,
    imm,
    label,
    inv
};

enum regbound : unsigned {
    _vreg_start = 0,
    _vreg_end   = 1 << 24,
    _preg_start = _vreg_end + 1
};

/** ir -> target code utils */

constexpr unsigned irdk2opc(ir::defkind irdk) {
    assert(irdk <= ir::defkind::_instr_end && irdk >= ir::defkind::_instr_start);
    return static_cast<unsigned>(irdk) - static_cast<unsigned>(ir::defkind::_instr_start);
}

constexpr bool is_irop(unsigned opc) {
    return opc < irdk2opc(ir::defkind::_instr_end);
}

constexpr typekind irty2tk(ir::typekind irty) {
    switch (irty) {
    default:
        assert(false);
        break;
    case ir::typekind::void_ty:
        assert(false && "attempting to get type from void ty?");
        break;
    case ir::typekind::i1:
    case ir::typekind::u8:
    case ir::typekind::i8:
        return typekind::s8;
    case ir::typekind::u16:
    case ir::typekind::i16:
        return typekind::s16;
    case ir::typekind::u32:
    case ir::typekind::i32:
        return typekind::s32;
    case ir::typekind::u64:
    case ir::typekind::i64:
    case ir::typekind::label:
    case ir::typekind::ptr:
    case ir::typekind::array:
    case ir::typekind::function:
        return typekind::s64;
    case ir::typekind::f32:
        return typekind::sfp;
    case ir::typekind::f64:
        return typekind::dfp;
    }
    __builtin_unreachable();
}

constexpr unsigned tysizebits(typekind tk) {
    switch (tk) {
    case typekind::s8:
        return 8;
    case typekind::s16:
        return 16;
    case typekind::s32:
    case typekind::sfp:
        return 32;
    case typekind::s64:
    case typekind::dfp:
        return 64;
    }
    __builtin_unreachable();
}

constexpr unsigned tysizebytes(typekind tk) {
    return tysizebits(tk) / 8;
}

/** target data */

struct RegData {
    unsigned reg;
};

struct StackData {
    int offset;
};

struct GlobalData {
    std::string str;
};

struct ImmData {
    uint64_t val;
};

struct LabelData {
    std::string str;
};

struct TargetValue;
struct TargetInstr;

struct UseEdge : IListNode<UseEdge> {
    TargetValue *val = nullptr;
    TargetInstr *instr = nullptr;
    bool implicit = false;

    UseEdge(TargetValue *tv, TargetInstr *ti, bool imp)
        : val(tv), instr(ti), implicit(imp) { }
    
    UseEdge(TargetValue *tv, TargetInstr *ti)
        : UseEdge(tv, ti, false) { }
};

struct TargetValue : public IListNode<TargetValue> {
    typekind ty;
    storagekind loc;
    union {
        RegData regdata;
        StackData stackdata;
        GlobalData *globdata;
        ImmData immdata;
        LabelData *labeldata;
    };
    IList<UseEdge> uses;

    static TargetValue *reg(typekind tk, RegData data, TargetInstr *defr) {
        auto tv = new TargetValue();
        tv->ty = tk;
        tv->loc = storagekind::reg;
        tv->regdata = data;
        return tv;
    }
    static TargetValue *stack(typekind tk, StackData data) {
        auto tv = new TargetValue();
        tv->ty = tk;
        tv->loc = storagekind::stack;
        tv->stackdata = data;
        return tv;
    }
    static TargetValue *glob(typekind tk, GlobalData *data) {
        auto tv = new TargetValue();
        tv->ty = tk;
        tv->loc = storagekind::glob;
        tv->globdata = data;
        return tv;
    }
    static TargetValue *imm(typekind tk, ImmData data) {
        auto tv = new TargetValue();
        tv->ty = tk;
        tv->loc = storagekind::imm;
        tv->immdata = data;
        return tv;
    }
    static TargetValue *label(LabelData *data) {
        auto tv = new TargetValue();
        tv->ty = irty2tk(ir::typekind::label);
        tv->loc = storagekind::label;
        tv->labeldata = data;
        return tv;
    }
};

struct TargetInstr : public IListNode<TargetInstr> {
    unsigned opcode;
    ir::Instr *orig;
    std::vector<UseEdge> uses;
    IList<TargetValue> defs;

    TargetInstr(unsigned opcode, ir::Instr *orig = nullptr)
        : opcode(opcode), orig(orig) { }
};

struct TargetFunction {
    std::string str;
    unsigned stacksize = 0;
    IList<TargetValue> params;
    IList<TargetInstr> instrs;
};

struct TargetProgram {
    std::vector<TargetValue *> globs;
    std::vector<TargetFunction> funcs;
};

/** target modification utils */

inline void add_use(TargetInstr *ti, TargetValue *tv, bool imp = false) {
    assert(ti && tv);
    UseEdge &ue = ti->uses.emplace_back(tv, ti, imp);
    tv->uses.push_back(&ue);
}

inline void set_use(UseEdge *ue, TargetValue *def, bool imp) {
    assert(ue && def);
    ue->val->uses.remove(ue);
    ue->val = def;
    ue->implicit = imp;
    def->uses.push_back(ue);
}

inline void set_use(UseEdge *ue, TargetValue *def) {
    assert(ue && def);
    set_use(ue, def, ue->implicit);
}

inline void rmv_use(TargetInstr *ti, unsigned idx) {
    assert(ti && idx < ti->uses.size());
    ti->uses[idx].val->uses.remove(&ti->uses[idx]);
    ti->uses.erase(ti->uses.begin() + idx);
}

inline void add_def(TargetInstr *ti, TargetValue *tv) {
    assert(ti);
    ti->defs.push_back(tv);
}

inline void rauw(TargetValue *olddef, TargetValue *newdef) {
    assert(olddef && newdef);
    for (auto ue : olddef->uses) {
        ue->val = newdef;
    }
}

inline TargetValue *make_stack(typekind tk, TargetFunction &tf) {
    auto tv = TargetValue::stack(tk, StackData{ tf.stacksize } );
    tf.stacksize += tysizebytes(tk);
    return tv;
}

/** target implementation base */

struct TargetCodeGen {
    // 1. calling conv lowering (ccvl)
    virtual void ccvl(TargetFunction &tf, ir::Function *f) = 0;

    // 2. instruction selection (isel)
    virtual bool isel(TargetFunction &tf, TargetInstr *ti) = 0;

    // 3. print assembly (pasm)
    virtual void pasm(TargetProgram &tp, std::ostream &os) = 0;

    // utils
    virtual std::string targetstr() = 0;
    virtual std::string instrstr(unsigned opcode) = 0;
    virtual std::string pregstr(unsigned preg) = 0;
    virtual bool is_preg(unsigned reg) = 0;
    virtual unsigned new_vreg() = 0;
};

void codegen(ir::Program *p, TargetCodeGen &tcg, std::ostream &os);

}

#endif