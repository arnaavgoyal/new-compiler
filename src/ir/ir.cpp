#include <cassert>
#include <iostream>
#include "ir/ir.h"

#define INDENT_INCR 2

namespace ir {

/** ------------------- Def ------------------- */

unsigned int Def::name_counter = 0;

void Def::add_use(Use *use) {
    list.append(use);
}

void Def::remove_use(Use *use) {
    list.remove(use);
}

void Def::dump(int indent) {

}

/** ------------------- DefUser ------------------- */

DefUser::DefUser(Type *ty, unsigned int num_ops)
    : Def(ty), num_ops(num_ops) {
    operands = new Use[num_ops];
    for (int i = 0; i < num_ops; i++) {
        operands[i].idx = i;
        operands[i].user = this;
    }
}
void DefUser::set_operand(unsigned int idx, Def *operand) {
    assert(idx < num_ops);
    Use *u = operands + idx;
    u->set_def(operand);
    std::cout << "adding use\n";
    operand->add_use(u);
    std::cout << "done adding use\n";
}
Use *DefUser::get_operand(unsigned int idx) {
    assert(idx < num_ops);
    return operands + idx;
}

void DefUser::dump_operands(int indent) {
    for (int i = 0; i < num_ops; i++) {
        operands[i].get_def()->dump(indent);
    }
}

/** ------------------- Block ------------------- */



/** ------------------- Instr ------------------- */

// ... 

/** ------------------- BinaryOpInstr ------------------- */

BinaryOpInstr::BinaryOpInstr(ir::instr opc, Def *x1, Def *x2)
    : Instr(x1->get_type(), 2, opc, nullptr) {
    assert(x1->get_type() == x2->get_type());
    set_operand(0, x1);
    set_operand(1, x2);
}

void BinaryOpInstr::dump(int indent) {

}

/** ------------------- Constant ------------------- */

// ... 

/** ------------------- Global ------------------- */



/** ------------------- Function ------------------- */

Function::Function(FunctionType *ty, ir::linkage lty, Program *parent)
    : Global(ty, lty) {
    set_parent(parent);
    int i = 0;
    for (Type *pty : ty->param_tys()) {
        params.emplace_back(pty, this, i);
        i++;
    }
}

/** ------------------- IntegralConstant ------------------- */

IntegralConstant::map_type IntegralConstant::vals;

IntegralConstant *IntegralConstant::get(Type *ty, uint64_t value) {

    // try to find value
    auto it = vals.find(value);

    // not found
    if (it == vals.end()) {

        // insert into map
        return vals.emplace(value, new IntegralConstant(ty, value)).first.operator*().second;
    }

    // found
    return it.operator*().second;
}

}
