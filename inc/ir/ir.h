#ifndef IR_H
#define IR_H

#include <map>
#include <type_traits>
#include "ir/type.h"
#include "utils/ilist.h"
#include "ir/symtable.h"

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
    Def *get_def() const { return def; }
    void set_def(Def *def) { this->def = def; }
    DefUser *get_user() const { return user; }
    unsigned int get_idx() const { return idx; }
};

class Def {
private:
    Type const *type;
    IList<Use> list;
    static unsigned int name_counter;

protected:
    Def(Def &) = default;
    Def(Def &&) = default;
    Def(Type const *ty) : type(ty) { }

public:
    Type const *get_type() { return type; }
    void add_use(Use *use);
    void remove_use(Use *use);
    void set_name(std::string &new_name);
    virtual void dump(int indent = 0) = 0;
};

class DefUser : public Def {
protected:
    Use *operands;
    unsigned int num_ops;

    DefUser(DefUser &) = default;
    DefUser(DefUser &&) = default;
    DefUser(Type const *ty, unsigned int num_ops);

public:
    void set_operand(unsigned int idx, Def *operand);
    Use *get_operand(unsigned int idx);
    void dump_operands(int indent = 0);
};

class Block
    : public Def, public STPPIListUser<Instr, Block>, public STPPIListNode<Block, Function> {
private:
    STPPIList<Instr, Block> list;
    SymbolTable<Instr> symtable;

    STPPIList<Instr, Block> &get_inner_list(Instr *) override { return list; } 

protected:
    Block(Block &) = default;
    Block(Block &&) = default;

public:
    Block(Function *func) : Def(ir::get_label()) {
        static_assert(std::is_base_of<STPPIListUser<Instr, Block>, Block>::value);
        set_parent(func); }
    void add_instr(Instr *i) { list.append(i, this); }
    void add_instr(Instr *i, std::string &name) { list.append_and_rename(i, name, this); }
    Instr *get_instr(std::string &name) { return list.get_by_name(name); }
    void remove_instr(Instr *instr);
    void dump(int indent) { }
};

/** ------------------- Instructions ------------------- */

class Instr : public DefUser, public STPPIListNode<Instr, Block> {
private:
    ir::instr opcode;

protected:
    Instr(Instr &) = default;
    Instr(Instr &&) = default;
    Instr(Type const *ty, int num_ops, ir::instr opc, Block *parent)
        : DefUser(ty, num_ops), opcode(opc) { set_parent(parent); }

public:
    void dump(int indent) { }
};

class ReturnInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "return";
    
public:
    ReturnInstr(Def *retval, Block *in);
};

class BranchInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "branch";
    
public:
    BranchInstr(Block *jmp, Block *in);
    BranchInstr(Def *cond, Block *jmp_true, Block *jmp_false);
};

class SAllocInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "salloc";
};

class GetInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "get";
};

class StoreInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "store";
};

class PtrIdxInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "ptridx";
};

class UpcastInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "upcast";
};

class DowncastInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "downcast";
};

class ICmpInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "icmp";
};

class CallInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "call";
    
private:
    Type const *callee_ty;

public:
    CallInstr(Function *callee);
    void add_argument();
};

class BinaryOpInstr : public Instr {
public:
    BinaryOpInstr(ir::instr opc, Def *x1, Def *x2);
    void dump(int indent = 0);
};



/** ------------------- Constants ------------------- */

class Constant : public Def {
public:
    void dump(int indent = 0) { }

protected:
    Constant(Constant &) = default;
    Constant(Constant &&) = default;
    Constant(Type const *ty) : Def(ty) { }
};

class IntegralConstant : public Constant {
public:
    using map_type = std::map<uint64_t, IntegralConstant const *>;

private:
    friend map_type;
    static map_type vals;
    uint64_t value;

    IntegralConstant(IntegralConstant &) = delete;
    IntegralConstant(IntegralConstant &&) = delete;
    IntegralConstant(Type const *ty, uint64_t v) : Constant(ty) { }

public:
    static IntegralConstant const *get(Type const *ty, uint64_t value);
};

/** ------------------- Globals ------------------- */

class Global : public Constant {
public:

private:
    linkage lty;

protected:
    Global(Type const *ty, linkage lty) : Constant(ty), lty(lty) { }

public:
    linkage get_linkage() { return lty; }
    void set_linkage(linkage linkage_ty) { lty = linkage_ty; }
};

class GlobalVar : public Global, public STPPIListNode<GlobalVar, Program> {
private:
    bool is_const;

public:
    GlobalVar(Type const *ty, linkage lty, Program *parent, bool is_const)
        : Global(ty, lty), is_const(is_const) { set_parent(parent); }
};

class Param : public Def {
    Function *param_of;
    int idx;

public:
    Param(Type const *ty, Function *func, int idx)
        : Def(ty), param_of(func), idx(idx) { }
    void dump(int indent = 0) { }
};

class Function
    : public Global, public STPPIListUser<Block, Function>,
        public STPPIListNode<Function, Program> {
private:
    std::vector<Param> params;
    STPPIList<Block, Function> list;
    SymbolTable<Block> symtable;

    STPPIList<Block, Function> &get_inner_list(Block *) override { return list; }

public:
    Function(FunctionType const *ty, linkage lty, Program *parent);
    FunctionType *get_type() { return (FunctionType *)Global::get_type(); }
    Block *get_block(std::string &name)
        { return symtable.get(name); }
    void add_block(Block *b) { list.append(b, this); }
    void dump(int indent = 0) { }
};

class Program
    : public STPPIListUser<GlobalVar, Program>, public STPPIListUser<Function, Program> {
private:
    STPPIList<GlobalVar, Program> gvar_list;
    STPPIList<Function, Program> func_list;

    STPPIList<GlobalVar, Program> &get_inner_list(GlobalVar *)
        override { return gvar_list; } 
    STPPIList<Function, Program> &get_inner_list(Function *)
        override { return func_list; } 

public:
    void add_glob(GlobalVar *gvar) { gvar_list.append(gvar, this); }
    GlobalVar *get_glob(std::string &name)
        { return gvar_list.get_by_name(name); }
    Function *get_function(std::string &name)
        { return func_list.get_by_name(name); }
};

}

#endif
