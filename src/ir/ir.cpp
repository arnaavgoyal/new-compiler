#include <cassert>
#include <iostream>
#include "ir/ir.h"
#include "utils/ioformat.h"

#define INDENT_INCR 2

namespace ir {

/** ------------------- Use ------------------- */

void Use::set_def(Def *def) {
    if (this->def) this->def->remove_use(this);
    this->def = def;
    if (def) def->add_use(this);
}

/** ------------------- Def ------------------- */

void Def::add_use(Use *use) {
    list.append(use);
}

void Def::remove_use(Use *use) {
    list.remove(use);
}

void Def::dump(unsigned indent) {
    std::cout << std::string(indent, ' ');
    dump_as_operand();
    std::cout << std::endl;
}

void Def::dump_as_operand() {
    std::cout << get_type()->stringify() << " <def>";
}

/** ------------------- DefUser ------------------- */

DefUser::DefUser(defkind kind, Type *ty, unsigned num)
    : Def(kind, ty) {
    //std::cout << "making defuser (in ctor)\n"
    //    << "  num=" << num << ", num_ops=" << num_ops << std::endl;
    if (num && has_var_oplist) {
        assert(num_ops == 0);
        realloc_oplist(num);
    }
    else {
        //std::cout << (unsigned)kind << std::endl;
        assert(num == num_ops);
    }
    //std::cout << "  done" << std::endl;
}

void *DefUser::operator new(size_t size) {
    //std::cout << "alloc'ing new defuser with var oplist\n";

    // allocate memory for this object + a pointer to an op list
    void *mem = ::operator new(size + sizeof(Use *));

    // get the oplist (ptr to mem)
    Use **oplist = static_cast<Use **>(mem);

    // get this object
    DefUser *this_obj = reinterpret_cast<DefUser *>(oplist + 1);

    // zero the oplist (to indicate that it has not yet been allocated)
    *oplist = nullptr;

    // this object has client-decided variable number of ops
    this_obj->has_var_oplist = true;
    this_obj->num_ops = 0;

    //std::cout << "  done\n";

    return this_obj;
}

void *DefUser::operator new(size_t size, unsigned num_ops) {
    //std::cout << "alloc'ing new defuser with fixed oplist\n";

    // allocate memory for this object + the op list
    void *mem = ::operator new(size + num_ops * sizeof(Use));

    // get the start and end of the op list
    Use *op_start = static_cast<Use *>(mem);
    Use *op_end = op_start + num_ops;

    // ptr to this object is just op_end
    DefUser *this_obj = reinterpret_cast<DefUser *>(op_end);

    // initialize the ops in op list
    for (Use *op_i = op_start; op_i != op_end; op_i++) {
        new(op_i) Use(this_obj, op_i - op_start);
    }

    // this object has fixed number of ops
    this_obj->has_var_oplist = false;
    this_obj->num_ops = num_ops;

    //std::cout << "  done\n";

    return this_obj;
}

void DefUser::operator delete(void *obj) {
    //std::cout << "deleting defuser\n";
    DefUser *this_obj = static_cast<DefUser *>(obj);
    if (this_obj->has_var_oplist) {
        ::operator delete(reinterpret_cast<Use **>(this_obj)[-1]);
    }
    ::operator delete(obj);
    //std::cout << "  done\n";
}

void DefUser::dump(unsigned indent) {
    dump_as_operand();
    dump_operands();
    std::cout << std::endl;
}

void DefUser::dump_as_operand() {
    std::cout << get_type()->stringify() << " <def user>";
}

void DefUser::dump_operands() {
    print_internally_separated_list(
        operands_begin(),
        operands_end(),
        ", ",
        [](Use &u) { u.get_def()->dump_as_operand(); }
    );
}

/** ------------------- Block ------------------- */

AutoRenamingSymbolTable<Instr *> &Block::get_symtable(Instr *) {
    assert(get_parent() && "instr names are uniqued at function scope, so this block must have a parent");
    return get_parent()->get_symtable(static_cast<Instr *>(nullptr));
}

std::string Block::add_instr(Instr *i, std::string name_hint) {

    if (!name_hint.empty()) {
        return list.append(i, name_hint);
    }
    return list.append(i);
}

void Block::remove_instr(Instr *instr) {
    list.remove(instr);
}

Instr *Block::remove_instr(std::string name_hint) {
    return list.remove(name_hint);
}

void Block::dump(unsigned indent) {
    std::cout << std::string(indent, ' ')
        << (has_name() ? get_name() : has_name_hint() ? get_name_hint() : "<unnamed>")
        << ":" << std::endl;
    unsigned ni = indent + INDENT_INCR;
    for (Instr *i : list) {
        i->dump(ni);
    }
}

void Block::dump_as_operand() {
    std::cout << get_type()->stringify() << " "
        << (has_name() ? get_name() : has_name_hint() ? get_name_hint() : "<unnamed>");
}

/** ------------------- Instr ------------------- */

void Instr::dump(unsigned indent) {
    std::cout << std::string(indent, ' ');
    if (has_name()) {
        std::cout << get_name() << " = ";
    }
    else if (has_name_hint()) {
        std::cout << get_name_hint() << " = ";
    }
    std::cout << get_str_repr() << " ";
    dump_operands();
    std::cout << std::endl;
}

void Instr::dump_as_operand() {
    std::cout << get_type()->stringify() << " ";
    if (has_name()) {
        std::cout << get_name();
    }
    else if (has_name_hint()) {
        std::cout << get_name_hint();
    }
    else {
        std::cout << "<unnamed>";
    }
}

void ICmpInstr::dump(unsigned indent) {
    std::cout << std::string(indent, ' ')
        << (has_name() ? get_name() : has_name_hint() ? get_name_hint() : "<unnamed>")
        << " = " << get_str_repr() << " ";

#define CMPCP(kind) case cmpkind::kind: std::cout << #kind; break;
    switch (kind) {
        CMPCP(ugt)
        CMPCP(ult)
        CMPCP(ugte)
        CMPCP(ulte)
        CMPCP(sgt)
        CMPCP(slt)
        CMPCP(sgte)
        CMPCP(slte)
        CMPCP(eq)
        CMPCP(neq)
        default: assert(false && "unknown cmpkind value?");
    }
#undef CMPCP

    std::cout << ", ";
    dump_operands();
    std::cout << std::endl;
}

CallInstr::CallInstr(Function *callee, std::vector<Def *> args, Block *parent, Instr *before, std::string name_hint)
    : Instr(defkind::call, callee->get_type(), 1 + callee->num_params(), parent, before, name_hint) {
    assert(callee->num_params() == args.size());
    set_operand(0, callee);
    auto it = callee->params_begin();
    for (unsigned i = 0; i < callee->num_params(); i++, ++it) {
        // std::cout << "arg: " << args[i]->get_type()->stringify()
        //     << "   param: " << (*it)->get_type()->stringify()
        //     << std::endl;
        assert(args[i]->get_type() == (*it)->get_type());
        set_operand(i + 1, args[i]);
    }
}

BranchInstr::BranchInstr(Def *cond, Block *jmp_true, Block *jmp_false, Block *parent)
    : Instr(defkind::branch, PrimitiveType::get_void_type(), 3, true, parent) {
    assert(cond->get_type() == PrimitiveType::get_i1_type() && "cond must be of i1 type");
    set_operand(0, jmp_true);
    set_operand(1, cond);
    set_operand(2, jmp_false);
    conditional = true;
}
BranchInstr::BranchInstr(Block *jmp, Block *parent)
    : Instr(defkind::branch, PrimitiveType::get_void_type(), 1, true, parent) {
    //std::cout << "ctor branch uncond" << std::endl;
    set_operand(0, jmp);
    conditional = false;
    //std::cout << "  done" << std::endl;
}

void SAllocInstr::dump(unsigned indent) {
    std::cout << std::string(indent, ' ')
        << (has_name() ? get_name() : has_name_hint() ? get_name_hint() : "<unnamed>")
        << " = " << get_str_repr()
        << " " << alloc_ty->stringify() << std::endl;
}

void ReadInstr::dump(unsigned indent) {
    std::cout << std::string(indent, ' ')
        << (has_name() ? get_name() : has_name_hint() ? get_name_hint() : "<unnamed>")
        << " = " << get_str_repr() << " "
        << get_type()->stringify() << ", ";
    dump_operands();
    std::cout << std::endl;
}

void TypecastInstr::dump(unsigned indent) {
    std::cout << std::string(indent, ' ')
        << (has_name() ? get_name() : has_name_hint() ? get_name_hint() : "<unnamed>")
        << " = " << get_str_repr() << " ";
    dump_operands();
    std::cout << ", " << get_type()->stringify() << std::endl;
}

void IUpcastInstr::dump(unsigned indent) {
    std::cout << std::string(indent, ' ')
        << (has_name() ? get_name() : has_name_hint() ? get_name_hint() : "<unnamed>")
        << " = " << get_str_repr() << " ";
    dump_operands();
    std::cout << ", " << get_type()->stringify() << std::endl;
}

void IDowncastInstr::dump(unsigned indent) {
    std::cout << std::string(indent, ' ')
        << (has_name() ? get_name() : has_name_hint() ? get_name_hint() : "<unnamed>")
        << " = " << get_str_repr() << " ";
    dump_operands();
    std::cout << ", " << get_type()->stringify() << std::endl;
}


BinaryOpInstr::BinaryOpInstr(defkind opc, Def *x, Def *y, Block *parent, Instr *before, std::string name_hint)
    : Instr(opc, x->get_type(), 2, parent, before, name_hint) {
    assert(x->get_type() == y->get_type() && "both x and y must have same type");
    set_operand(0, x);
    set_operand(1, y);
}

/** ------------------- Global ------------------- */

void GlobalVar::dump(unsigned indent) {;
    std::cout << std::string(indent, ' ');
    dump_as_operand();
    std::cout << std::endl;
}

void GlobalVar::dump_as_operand() {
    std::cout << get_type()->stringify() << " " << get_name();
}

/** ------------------- Function ------------------- */

void Param::dump(unsigned indent) {
    std::cout << std::string(indent, ' ');
    dump_as_operand();
    std::cout << std::endl;
}

void Param::dump_as_operand() {
    std::cout << get_type()->stringify() << " " << get_name();
}

Function::Function(Type *return_ty, linkage lty, Program *parent, std::string name_hint)
    : Global(defkind::function, return_ty, lty), params(this), blocks(this) {
    assert(!name_hint.empty() && "functions must be named");
    set_name(name_hint);
    set_parent(parent);
}

void Function::dump(unsigned indent) {
    std::string is(indent, ' ');
    std::cout << is << get_type()->stringify() << " " << get_name() << " (";
    auto piter = params_iterable();
    print_internally_separated_list(
        piter.begin(),
        piter.end(),
        ", ",
        [](Param *p) { p->dump_as_operand(); }
    );
    std::cout << ") {" << std::endl;
    for (Block *b : blocks) {
        b->dump(indent);
    }
    std::cout << is << "}" << std::endl;
}

void Function::dump_as_operand() {
    std::cout << get_type()->stringify() << " " << get_name();
}

/** ------------------- IntegralConstant ------------------- */

IntegralConstant::map_type IntegralConstant::vals;

IntegralConstant *IntegralConstant::get(Type *ty, uint64_t value) {

    // try to find value
    auto key = std::make_pair(ty, value);
    auto it = vals.find(key);

    // not found
    if (it == vals.end()) {

        // TODO: determine if value actually fits into type

        // insert into map
        return vals.emplace(key, new IntegralConstant(ty, value)).first->second;
    }

    // found
    return it->second;
}

void IntegralConstant::dump(unsigned indent) {
    std::cout << std::string(indent, ' ');
    dump_as_operand();
    std::cout << std::endl;
}

void IntegralConstant::dump_as_operand() {
    std::cout << get_type()->stringify() << " " << value;
}

void Program::dump() {
    //std::cout << "Program " << name << std::endl;
    for (GlobalVar *gv : gvar_list) {
        //std::cout << "gvar @ " << gv << std::endl;
        gv->dump();
    }
    for (Function *f : func_list) {
        //std::cout << "func @ " << f << std::endl;
        f->dump();
    }
}

}
