#include <cassert>
#include <iostream>
#include "ir/ir.h"

#define INDENT_INCR 2

namespace ir {

/** ------------------- Def ------------------- */

unsigned Def::name_counter = 0;

void Def::add_use(Use *use) {
    list.append(use);
}

void Def::remove_use(Use *use) {
    list.remove(use);
}

/** ------------------- DefUser ------------------- */

DefUser::DefUser(Type *ty, unsigned num_ops)
    : Def(ty), num_ops(num_ops) {
    operands = new Use[num_ops];
    for (unsigned i = 0; i < num_ops; i++) {
        operands[i].idx = i;
        operands[i].user = this;
    }
}
void DefUser::set_operand(unsigned idx, Def *operand) {
    assert(idx < num_ops);
    Use *u = operands + idx;
    u->set_def(operand);
    operand->add_use(u);
}
Use *DefUser::get_operand(unsigned idx) {
    assert(idx < num_ops);
    return operands + idx;
}

void DefUser::dump_operands(unsigned indent) {
    for (unsigned i = 0; i < num_ops; i++) {
        operands[i].get_def()->dump(indent);
    }
}

/** ------------------- Block ------------------- */

void Block::remove_instr(Instr *instr) {
    list.remove(instr);
}

void Block::remove_instr(std::string name) {
    list.remove(name);
}

void Block::dump(unsigned indent) {
    std::cout << std::string(indent, ' ');
    if (has_name()) {
        std::cout << get_name();
    }
    else {
        std::cout << "<unnamed block>";
    }
    std::cout  << ":" << std::endl;
    Instr *curr = list.head();
    unsigned ni = indent + INDENT_INCR;
    for (unsigned i = 0; i < list.size(); i++) {
        curr->dump(ni);
        curr = curr->get_next();
    }
}

/** ------------------- Instr ------------------- */

void ReturnInstr::dump(unsigned indent) {
    std::cout << std::string(indent, ' ') << STR_REPR << std::endl;
    get_operand(0)->get_def()->dump(indent + INDENT_INCR);
}

CallInstr::CallInstr(Function *callee, std::vector<Def *> args, Block *parent)
    : Instr(callee->get_type()->return_ty(), 1 + callee->num_params(), instr::call, parent) {
    assert(callee->num_params() == args.size());
    set_operand(0, callee);
    auto it = callee->params_begin();
    for (unsigned i = 0; i < callee->num_params(); i++) {
        assert(args[i]->get_type() == (*it)->get_type());
        set_operand(i + 1, args[i]);
    }
}

void CallInstr::dump(unsigned indent) {
    std::string is(indent, ' ');
    std::cout << is << get_type()->stringify() << " ";
    if (has_name()) {
        std::cout << get_name();
    }
    else {
        std::cout << "<unnamed>";
    }
    std::cout << " = " << STR_REPR << std::endl;
    Function *callee = static_cast<Function *>(get_operand(0)->get_def());
    callee->dump_as_op(indent + INDENT_INCR);
    for (unsigned i = 1; i < get_num_ops(); i++) {
        get_operand(i)->get_def()->dump(indent + INDENT_INCR);
    }
}

BinaryOpInstr::BinaryOpInstr(ir::instr opc, Def *x, Def *y)
    : Instr(x->get_type(), 2, opc, nullptr) {
    assert(x->get_type() == y->get_type() && "both x and y must have same type");
    set_operand(0, x);
    set_operand(1, y);
}

void BinaryOpInstr::dump(unsigned indent) {

}


/** ------------------- Global ------------------- */



/** ------------------- Function ------------------- */

Function::Function(FunctionType *ty, ir::linkage lty, Program *parent)
    : Global(ty, lty), params(this), blocks(this) {
    set_parent(parent);
    unsigned i = 0;
    for (Type *pty : ty->param_tys()) {
        params.append(new Param(pty, this, i));
        i++;
    }
}

void Function::dump(unsigned indent) {
    std::string is(indent, ' ');
    dump_as_op(indent, false);
    std::cout << " (";
    for (Param *p : params_iterable()) {
        std::cout << " " << p->get_type()->stringify();
    }
    std::cout << ") {" << std::endl;
    Block *curr = blocks.head();
    for (size_t i = 0; i < blocks.size(); i++) {
        curr->dump(indent);
        curr = curr->get_next();
    }
    std::cout << is << "}" << std::endl;
}

void Function::dump_as_op(unsigned indent, bool newline) {
    std::string is(indent, ' ');
    std::cout << is << static_cast<FunctionType *>(get_type())->return_ty()->stringify() << " ";
    if (has_name()) {
        std::cout << get_name();
    }
    else {
        std::cout << "<unnamed function>";
    }
    if (newline) {
        std::cout << std::endl;
    }
}

/** ------------------- IntegralConstant ------------------- */

IntegralConstant::map_type IntegralConstant::vals;

IntegralConstant *IntegralConstant::get(Type *ty, uint64_t value) {

    // try to find value
    auto it = vals.find(value);

    // not found
    if (it == vals.end()) {

        // TODO: determine if value actually fits into type

        // insert into map
        return vals.emplace(value, new IntegralConstant(ty, value)).first->second;
    }

    // found
    return it->second;
}

void IntegralConstant::dump(unsigned indent) {
    std::cout
        << std::string(indent, ' ')
        << get_type()->stringify()
        << " " << value << std::endl;
}

}
