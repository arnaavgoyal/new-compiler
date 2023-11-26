#ifndef IR_H
#define IR_H

#include <map>
#include <type_traits>
#include "ir/type.h"
#include "utils/stppilist.h"
#include "utils/symtable.h"

namespace ir {

class Def;
class Use;
class DefUser;
class Block;
class Instr;
class Constant;
class IntegralConstant;
class Global;
class Function;
class Program;

enum class linkage {
    internal,
    external,
};

enum class instr {

// control flow

    // return
    ret,
    // branch
    branch,

// memory

    // stack mem alloc
    salloc,
    // get mem
    get,
    // store mem
    store,
    // index a ptr
    ptridx,

// typecasting

    // upcast (smaller -> bigger)
    upcast,
    // downcast (bigger -> smaller)
    downcast,

// binary operations

    __binop_start,
    iadd,
    isub,
    imul,
    idiv,
    __binop_end,

// other

    // compare integral types
    icmp,
    // call a function
    call,
};

class Use : public IListNode<Use> {
    friend DefUser;
    DefUser *user;
    Def *def;
    unsigned int idx;

public:
    Use() : user(nullptr), def(nullptr), idx(0) { }
    Use(DefUser *user, Def *def, unsigned int idx)
        : user(user), def(def), idx(idx) { }
    Def *get_def() { return def; }
    void set_def(Def *def) { this->def = def; }
    DefUser *get_user() { return user; }
    unsigned int get_idx() { return idx; }
};

class Def {
private:
    Type *type;
    IList<Use> list;
    static unsigned int name_counter;

protected:
    Def(Def &) = default;
    Def(Def &&) = default;
    Def(Type *ty) : type(ty) { }

public:
    Type *get_type() { return type; }
    void add_use(Use *use);
    void remove_use(Use *use);
    virtual void dump(int indent = 0) = 0;
};

class DefUser : public Def {
protected:
    Use *operands;
    unsigned int num_ops;

    DefUser(DefUser &) = default;
    DefUser(DefUser &&) = default;
    DefUser(Type *ty, unsigned int num_ops);

public:
    void set_operand(unsigned int idx, Def *operand);
    Use *get_operand(unsigned int idx);
    void dump_operands(int indent = 0);
};

class Block
    : public Def, public STPPIListNode<Block, Function> {
private:
    STPPIList<Instr, Block> list;
    SymbolTable<Instr> symtable;

    friend STPPIListNode<Instr, Block>;
    STPPIList<Instr, Block> &get_inner_list(Instr *) { return list; } 

protected:
    Block(Block &) = default;
    Block(Block &&) = default;

public:
    Block(Function *func, std::string name) : Def(ir::get_label()) { set_parent(func); set_name(name); }
    void add_instr(Instr *i) { list.append(i, this); }
    void add_instr(Instr *i, std::string name) { list.append_and_rename(i, name, this); }
    Instr *get_instr(std::string name) { return list.get(name); }
    void remove_instr(Instr *instr);
    void dump(int indent) { }
};

/** ------------------- Instructions ------------------- */

class Instr : public DefUser, public STPPIListNode<Instr, Block> {
private:
    instr opcode;

protected:
    Instr(Instr &) = default;
    Instr(Instr &&) = default;
    Instr(Type *ty, int num_ops, instr opc, Block *parent)
        : DefUser(ty, num_ops), opcode(opc) { set_parent(parent); }
    Instr(Type *ty, int num_ops, instr opc, Block *parent, std::string name)
        : Instr(ty, num_ops, opc, parent) { set_name(name); }

public:
    void dump(int indent) { }
};

class ReturnInstr : public Instr {
public:
    static constexpr char *STR_REPR = "return";
    
public:
    ReturnInstr(Def *retval, Block *in)
        : Instr(retval->get_type(), 1, instr::ret, in) { }
    ReturnInstr(Def *retval, Block *in, std::string name)
        : Instr(retval->get_type(), 1, instr::ret, in, name) { }
};

class BranchInstr : public Instr {
public:
    static constexpr char *STR_REPR = "branch";
    
public:
    BranchInstr(Block *jmp, Block *in);
    BranchInstr(Def *cond, Block *jmp_true, Block *jmp_false);
};

class SAllocInstr : public Instr {
public:
    static constexpr char *STR_REPR = "salloc";
};

class GetInstr : public Instr {
public:
    static constexpr char *STR_REPR = "get";
};

class StoreInstr : public Instr {
public:
    static constexpr char *STR_REPR = "store";
};

class PtrIdxInstr : public Instr {
public:
    static constexpr char *STR_REPR = "ptridx";
};

class UpcastInstr : public Instr {
public:
    static constexpr char *STR_REPR = "upcast";
};

class DowncastInstr : public Instr {
public:
    static constexpr char *STR_REPR = "downcast";
};

class ICmpInstr : public Instr {
public:
    static constexpr char *STR_REPR = "icmp";
};

class CallInstr : public Instr {
public:
    static constexpr char *STR_REPR = "call";
    
private:
    Type *callee_ty;

public:
    CallInstr(Function *callee);
    void add_argument();
};

class BinaryOpInstr : public Instr {
public:
    BinaryOpInstr(instr opc, Def *x1, Def *x2);
    void dump(int indent = 0);
};



/** ------------------- Constants ------------------- */

class Constant : public Def {
public:
    void dump(int indent = 0) { }

protected:
    Constant(Constant &) = default;
    Constant(Constant &&) = default;
    Constant(Type *ty) : Def(ty) { }
};

class IntegralConstant : public Constant {
public:
    using map_type = std::map<uint64_t, IntegralConstant *>;

private:
    friend map_type;
    static map_type vals;
    uint64_t value;

    IntegralConstant(IntegralConstant &) = delete;
    IntegralConstant(IntegralConstant &&) = delete;
    IntegralConstant(Type *ty, uint64_t v) : Constant(ty) { }

public:
    static IntegralConstant *get(Type *ty, uint64_t value);
};

/** ------------------- Globals ------------------- */

class Global : public Constant {
public:

private:
    linkage lty;

protected:
    Global(Type *ty, linkage lty) : Constant(ty), lty(lty) { }

public:
    linkage get_linkage() { return lty; }
    void set_linkage(linkage linkage_ty) { lty = linkage_ty; }
};

class GlobalVar : public Global, public STPPIListNode<GlobalVar, Program> {
private:
    bool is_const;

public:
    GlobalVar(Type *ty, linkage lty, Program *parent, bool is_const)
        : Global(ty, lty), is_const(is_const) { set_parent(parent); }
};

class Param : public Def {
    Function *param_of;
    int idx;

public:
    Param(Type *ty, Function *func, int idx)
        : Def(ty), param_of(func), idx(idx) { }
    void dump(int indent = 0) { }
};

class Function
    : public Global, public STPPIListNode<Function, Program> {
private:
    std::vector<Param> params;
    STPPIList<Block, Function> list;

    friend STPPIListNode<Block, Function>;
    STPPIList<Block, Function> &get_inner_list(Block *) { return list; }

public:
    Function(FunctionType *ty, linkage lty, Program *parent);
    Function(FunctionType *ty, linkage lty, Program *parent, std::string name)
        : Function(ty, lty, parent) { set_name(name); }
    FunctionType *get_type() { return (FunctionType *)Global::get_type(); }
    Block *get_block(std::string name)
        { return list.get(name); }
    void add_block(Block *b) { list.append(b, this); }
    void dump(int indent = 0) { }
};

class Program {
private:
    STPPIList<GlobalVar, Program> gvar_list;
    STPPIList<Function, Program> func_list;

    friend STPPIListNode<GlobalVar, Program>;
    STPPIList<GlobalVar, Program> &get_inner_list(GlobalVar *) { return gvar_list; }

    friend STPPIListNode<Function, Program>;
    STPPIList<Function, Program> &get_inner_list(Function *) { return func_list; }

public:
    void add_glob(GlobalVar *gvar) { gvar_list.append(gvar, this); }
    GlobalVar *get_glob(std::string name)
        { return gvar_list.get(name); }
    Function *get_function(std::string name)
        { return func_list.get(name); }
};

}

#endif
