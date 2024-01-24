#include "codegen/codegen.h"
#include "utils/iterator.h"
#include <utility>
#include <vector>
#include <map>
#include <cstring>
#include "utils/ioformat.h"


// static const unsigned num_main_regs = op::_reg_end;

// char const *regstr(unsigned r) {
//     switch (r) {
    
// #define REG(val, bits) case val: return #val;
// #include "codegen/x86_64_gen"

//     case stack: return "stack";
//     case glob: return "mem";
//     case imm: return "imm";
//     case inv: return "inv";
//     }
//     __builtin_unreachable();
// }

// unsigned reg2reg(unsigned r, unsigned sz) {
//     assert(r < _subreg_end && "must be reg op");

// #define SUBREG(val, bits, parent) if (r == parent && sz == bits) return val;
// #define REG(val, bits) SUBREG(val, bits, val)
// #include "codegen/x86_64_gen"

//     std::cout << r << " " << regstr(r) << " " << sz << "\n";
//     //assert(false && "incompatible bit size of (sub) reg");
//     return op::inv;
// }

// unsigned regsize(unsigned r) {
//     assert(r < _subreg_end && "must be reg op");
//     switch (r) {

// #define REG(val, bits) case val: return bits;
// #include "codegen/x86_64_gen"

//     default: __builtin_unreachable();
//     }
// }



// static std::string valstr(Value *v) {
//     if (v->reg()) {
//         return std::string(regstr(v->where));
//     }
//     else if (v->stack()) {
//         return std::string("[") + regstr(op::rbp) + " + " + std::to_string(v->stackdata) + "]";
//     }
//     else if (v->glob()) {
//         return std::string("[") + v->globdata->get_name() + "]";
//     }
//     else if (v->imm()) {
//         return std::to_string(v->immdata->get_value());
//     }
//     __builtin_unreachable();
// }

// std::map<ir::Def *, Value *> &def2op() {
//     static std::map<ir::Def *, Value *> m;
//     return m;
// }

// static Value *allocs[num_main_regs] = {nullptr};

// void ralloc(Value *to, unsigned sz) {
//     assert(to && "to must be non-null");
//     assert(to->where > _reg_end && "reg alloc on value with reg already alloced");

//     Value **curr = allocs;
//     while (*curr) {
//         if (curr - allocs >= num_main_regs) {
//             assert(false && "no more regs to alloc");
//         }
//         curr++;
//     }

//     *curr = to;
//     to->where = reg2reg(curr - allocs, sz);
// }

// void roverride(unsigned reg, Value *to, unsigned sz) {
//     assert(to && "to must be non-null");
//     assert(reg < _reg_end);
//     Value *old = allocs[reg];
//     unsigned oldsz = regsize(old->where);
//     old->where = op::inv;
//     ralloc(old, oldsz);
//     allocs[reg] = to;
//     to->where = reg2reg(reg, sz);
// }

// void rfree(Value *v) {
//     assert(v);
//     if (v->where < op::_subreg_end) {
//         std::cout << regstr(v->where) << "\n";
//         allocs[v->where] = nullptr;
//         v->where = op::inv;
//     }
// }

// void rfree_all() {
//     for (Value **i = allocs; i != allocs + op::_reg_end; i++) {
//         (*i)->where = op::inv;
//         i = nullptr;
//     }
// }

// void cg_gvars(ir::Program *p, std::ostream &os) {
//     os << "section .data\n";
//     for (auto gv : make_iterator_range(p->gvar_begin(), p->gvar_end())) {
//         os << gv->get_name() << ":"
//             << "\n\t";
//         switch (gv->get_type()->get_kind()) {
//         case ir::typekind::i1:
//         case ir::typekind::i8:
//         case ir::typekind::u8:
//             os << "db";
//             break;
//         case ir::typekind::i16:
//         case ir::typekind::u16:
//             os << "dw";
//             break;
//         case ir::typekind::i32:
//         case ir::typekind::u32:
//             os << "dd";
//             break;
//         case ir::typekind::i64:
//         case ir::typekind::u64:
//             os << "dq";
//             break;
//         default:
//             assert(false);
//             __builtin_unreachable();
//         }
//         // TODO: do init value if exists
//         os << " ?\n";
//         def2op().emplace(gv, new Value{gv->get_type(), op::glob});
//     }
// }

// Value *gen_copy(Value *v, unsigned reg, std::ostream &os) {
//     auto mv = new Value{};
//     roverride(op::rax, mv, v->ty->get_size());
//     os << "\tmov\t" << regstr(reg) << ", ";
//     os << valstr(v) << "\n";
//     return mv;
// }

// void gen_call(ir::CallInstr *ci, std::ostream &os) {
//     auto v = new Value{ci->get_type()};
//     // ...
// }

// void gen_instr(ir::Instr *i, std::ostream &os) {
//     i->dump();
//     auto thisv = new Value{i->get_type(), op::inv};
//     if (i->get_type() != ir::PrimitiveType::get_void_type()) {
//         unsigned sz = i->get_type()->get_size();
//         if (sz == 1) {
//             sz = 8;
//         }
//         ralloc(thisv, sz);
//     }
//     switch (i->get_kind()) {
//     case ir::defkind::ret: {
//         auto v = def2op()[i->get_operand(0)];
//         if (v->where != reg2reg(op::rax, v->ty->get_size())) {
//             // move value into rax
//             gen_copy(v, op::rax, os);
//             os << "\tret\n";
//         }
//         break;
//     }
//     case ir::defkind::call: {
//         gen_call(static_cast<ir::CallInstr *>(i), os);
//         break;
//     }
//     }
//     os << "\t" << regstr(thisv->where) << " = " << i->get_str_repr() << "\t";
//     for (auto d : make_iterator_range(i->def_operands_begin(), i->def_operands_end())) {
//         std::cout << "  ";
//         d->dump_as_operand();
//         std::cout << "\n";
//         os << " ";
//         if (d->get_kind() == ir::defkind::block) {
//             os << static_cast<ir::Block *>(d)->get_name();
//         }
//         else {
//             auto it = def2op().find(d);
//             if (it != def2op().end()) {
//                 os << regstr(it->second->where);
//                 rfree(it->second);
//             }
//             else {
//                 os << regstr(op::inv);
//             }
//         }
//     }
//     os << "\n";
//     def2op()[i] = thisv;
// }

// void gen_block(ir::Block *b, std::ostream &os) {
//     os << b->get_name() << ":\n";
//     for (auto i : *b) {
//         gen_instr(i, os);
//     }
// }

// namespace be {

// void codegen(ir::Program *p, std::ostream &os) {

//     // constants
//     for (auto &[e, c] : make_iterator_range(ir::IntegralConstant::begin(), ir::IntegralConstant::end())) {
//         def2op()[c] = new Value{c->get_type(), op::imm, {.immdata = c}};
//     }

//     // globals
//     cg_gvars(p, os);

//     // functions
//     for (auto f : *p) {

//         os << "\n" << f->get_name() << ":\n";
        
//         // params
//         for (auto param : f->params_iterable()) {
//             def2op().emplace(param, new Value{param->get_type(), op::stack, {.stackdata = 0}});
//         }

//         // blocks
//         for (auto b : *f) {
//             gen_block(b, os);
//         }
//     }
// }

// }

namespace be {

TargetInstr *visit_instr(ir::Instr *i, TargetCodeGen &tcg, TargetFunction &tf, std::map<ir::Def *, TargetValue *> &def2tv) {
    // if (i->get_kind() == ir::defkind::call) {
    //     auto ti = tcg.ccvl_call(tf, static_cast<ir::CallInstr *>(i));
    //     def2tv[i] = ti->defs.back();
    //     tf.instrs.push_back(ti);
    //     return ti;
    // }
    auto ti = new TargetInstr(irdk2opc(i->get_kind()), i);
    for (auto &u : make_iterator_range(i->operands_begin(), i->operands_end())) {
        auto d = u.get_def();
        TargetValue *tv;
        tv = def2tv[d];
        if (!tv) {
            //d->dump();
            switch (d->get_kind()) {
            case ir::defkind::block:
                tv = TargetValue::label(new LabelData{ static_cast<ir::Block *>(d)->get_name()});
                def2tv[d] = tv;
                break;
            case ir::defkind::integral_constant:
                std::cout << "int lit " << static_cast<ir::IntegralConstant *>(d)->get_value() << std::endl;
                tv = TargetValue::imm(irty2tk(d->get_type()->get_kind()), ImmData{ static_cast<ir::IntegralConstant *>(d)->get_value() });
                def2tv[d] = tv;
                break;    
            case ir::defkind::function:
                std::cout << "func " << static_cast<ir::Function *>(d)->get_name() << "\n";
                tv = TargetValue::label(new LabelData{ static_cast<ir::Function *>(d)->get_name() });
                def2tv[d] = tv;
                break;            
            default:
                assert(d->is_instr());
                tv = visit_instr(static_cast<ir::Instr *>(d), tcg, tf, def2tv)->defs.back();
                break;
            }
        }
        assert(tv);
        add_use(ti, tv);
    }
    if (i->get_type()->get_kind() != ir::typekind::void_ty) {
        // has a value that it defines
        add_def(ti, TargetValue::reg(irty2tk(i->get_type()->get_kind()), RegData{ tcg.new_vreg() }, ti));
        def2tv[i] = ti->defs.back();
    }
    tf.instrs.push_back(ti);
    return ti;
}

void translate(TargetProgram &tp, ir::Program *p, TargetCodeGen &tcg) {
    std::map<ir::Def *, TargetValue *> def2tv;

    for (auto gvar : make_iterator_range(p->gvar_begin(), p->gvar_end())) {
        tp.globs.push_back(TargetValue::glob(
            irty2tk(gvar->get_type()->get_kind()),
            new GlobalData{ gvar->get_name() }
        ));
        def2tv[gvar] = tp.globs.back();
    }
    for (auto &[def, tv] : def2tv) {
        std::cout << tv->globdata->str << "\n";
    }
    
    for (auto f : *p) {
        TargetFunction tf;
        tf.str = f->get_name();
        tcg.ccvl(tf, f);
        assert(tf.params.size() == f->num_params());
        auto fpit = f->params_begin();
        for (auto tvp : tf.params) {
            def2tv[*fpit] = tvp;
            fpit++;
        }
        for (auto b : *f) {
            for (auto i : *b) {
                if (!def2tv[i]) {
                    visit_instr(i, tcg, tf, def2tv);
                }
            }
        }
        tp.funcs.push_back(std::move(tf));
    }
}

void dbgprint(TargetProgram &tp, TargetCodeGen &tcg, std::ostream &os) {
    for (auto glob : tp.globs) {
        os << glob->globdata->str << ": " << tysizebytes(glob->ty) << "\n";
    }
    auto opdbgfunc = [&](TargetValue *val){
        os << "%";
        switch (val->loc) {
        case storagekind::reg:
            if (tcg.is_preg(val->regdata.reg)) {
                os << "p:" << tcg.pregstr(val->regdata.reg);
            }
            else {
                os << "v:" << val->regdata.reg;
            }
            break;
        case storagekind::stack:
            os << "s:" << val->stackdata.offset;
            break;
        case storagekind::glob:
            os << "g:" << val->globdata->str;
            break;
        case storagekind::imm:
            os << "i:" << val->immdata.val;
            break;
        case storagekind::label:
            os << "l:" << val->labeldata->str;
            break;
        default:
            break;
        }
    };
    for (auto &func : tp.funcs) {
        os << func.str << ": \n";
        for (auto instr : func.instrs) {
            os << "  ";
            if (instr->defs.size()) {
                print_internally_separated_list(
                    os,
                    instr->defs.begin(),
                    instr->defs.end(),
                    ", ",
                    opdbgfunc
                );
                os << " = ";
            }
            if (instr->opcode < irdk2opc(ir::defkind::_instr_end)) {
                // ir instr
                os << instr->orig->get_str_repr();
            }
            else {
                // target native instr
                os << tcg.targetstr() << ":" << tcg.instrstr(instr->opcode);
            }
            os << " ";
            print_internally_separated_list(
                os,
                instr->uses.begin(),
                instr->uses.end(),
                ", ",
                [&](UseEdge ue){
                    if (ue.implicit) os << "<imp ";
                    opdbgfunc(ue.val);
                    if (ue.implicit) os << ">";
                }
            );
            os << "\n";
        }
    }
}

void codegen(ir::Program *p, TargetCodeGen &tcg, std::ostream &os) {

    TargetProgram tp;
    translate(tp, p, tcg);

    dbgprint(tp, tcg, std::cout);
    std::cout << std::endl;

    for (auto tf : tp.funcs) {
        for (auto ti : tf.instrs) {
            if (is_irop(ti->opcode)) {
                bool selected = tcg.isel(tf, ti);
                if (!selected) {
                    std::cout << "\n";
                    ti->orig->dump();
                    assert(false && "could not select");
                }
            }
        }
    }

    dbgprint(tp, tcg, std::cout);

    tcg.pasm(tp, os);
}

}
