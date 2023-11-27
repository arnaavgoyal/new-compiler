#ifndef IR_H
#define IR_H

#include <map>
#include <type_traits>
#include <ranges>
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
    // read mem
    read,
    // write mem
    write,
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
    unsigned idx;

public:
    Use() : user(nullptr), def(nullptr), idx(0) { }
    Use(DefUser *user, Def *def, unsigned idx)
        : user(user), def(def), idx(idx) { }
    Def *get_def() { return def; }
    void set_def(Def *def) { this->def = def; }
    DefUser *get_user() { return user; }
    unsigned get_idx() { return idx; }
};

class Def {
private:
    Type *type;
    IList<Use> list;
    static unsigned name_counter;

protected:
    Def(Def &) = default;
    Def(Def &&) = default;
    Def(Type *ty) : type(ty) { }

public:
    Type *get_type() { return type; }
    void add_use(Use *use);
    void remove_use(Use *use);
    virtual void dump(unsigned indent = 0) = 0;
};

class DefUser : public Def {
private:
    Use *operands;
    unsigned num_ops;

protected:
    DefUser(DefUser &) = default;
    DefUser(DefUser &&) = default;
    DefUser(Type *ty, unsigned num_ops);

    unsigned get_num_ops() { return num_ops; }
    void set_operand(unsigned idx, Def *operand);
    Use *get_operand(unsigned idx);
    void dump_operands(unsigned indent = 0);
};

class Block : public Def, public STPPIListNode<Block, Function> {
private:
    using list_ty = STPPIList<Instr, Block>;
    list_ty list;

    SymbolTable<Instr> symtable;

    friend list_ty::node_type;
    list_ty &get_inner_list(Instr *) { return list; } 

protected:
    Block(Block &) = default;
    Block(Block &&) = default;

public:
    Block(Function *func, std::string name = "")
        : Def(ir::PrimitiveType::get_label_type()), list(this) {
        if (!name.empty()) {
            set_name(name);
        }
        set_parent(func);
    }
    void add_instr(Instr *i) { list.append(i); }
    void add_instr(Instr *i, std::string name) { list.append_and_rename(i, name); }
    Instr *get_instr(std::string name) { return list.get(name); }
    void remove_instr(Instr *instr);
    void remove_instr(std::string name);
    void dump(unsigned indent = 0);
};

/** ------------------- Instructions ------------------- */

class Instr : public DefUser, public STPPIListNode<Instr, Block> {
private:
    instr kind;

protected:
    Instr(Instr &) = default;
    Instr(Instr &&) = default;
    Instr(Type *ty, unsigned num_ops, instr kind, Block *parent = nullptr, std::string name = "")
        : DefUser(ty, num_ops), kind(kind) {
        if (!name.empty()) {
            set_name(name);
        }
        if (parent) {
            set_parent(parent);
        }
    }

public:
    instr get_instr_kind() { return kind; }
    void dump(unsigned indent) { }
};

class ReturnInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "return";
    
public:
    ReturnInstr(Def *retval, Block *in, std::string name = "")
        : Instr(retval->get_type(), 1, instr::ret, in, name) { set_operand(0, retval); }
    void dump(unsigned indent = 0);
};

class BranchInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "branch";
    
public:
    BranchInstr(Def *cond, Block *jmp_true, Block *jmp_false, Block *parent = nullptr, std::string name = "")
        : Instr(cond->get_type(), 3, instr::branch, parent) { }
    BranchInstr(Block *jmp, Block *parent = nullptr, std::string name = "")
        : BranchInstr(nullptr, jmp, nullptr, parent, name) { }
};

class SAllocInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "salloc";

private:
    Type *alloc_ty;

public:
    SAllocInstr(Type *alloc_type, Block *parent = nullptr, std::string name = "")
        : Instr(PrimitiveType::get_ptr_type(), 0, instr::salloc, parent, name), alloc_ty(alloc_type) { }
};

class ReadInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "read";

public:
    ReadInstr(Type *val_type, Def *mem_ptr, Block *parent = nullptr, std::string name = "")
        : Instr(val_type, 1, instr::read, parent) {
        assert(mem_ptr->get_type() == PrimitiveType::get_ptr_type() && "mem_ptr must be of pointer type");
        set_operand(0, mem_ptr);
    }
};

class WriteInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "write";

public:
    WriteInstr(Def *val, Def *mem, Block *parent = nullptr, std::string name = "")
        : Instr(val->get_type(), 1, instr::write, parent) { set_operand(0, mem); }
};

class PtrIdxInstr : public Instr {
public:
    static constexpr char const *const STR_REPR = "ptridx";

public:
    PtrIdxInstr(Def *ptr_val, Def *idx_val, Block *parent = nullptr, std::string name = "")
        : Instr(PrimitiveType::get_ptr_type(), 2, instr::ptridx, parent, name) {
        assert(ptr_val->get_type() == PrimitiveType::get_ptr_type() && "ptr_val must be of pointer type");
        set_operand(0, ptr_val);
        set_operand(1, idx_val);
    }
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

public:
    CallInstr(Function *callee, std::vector<Def *> args, Block *parent = nullptr);
    void dump(unsigned indent = 0);
};

class BinaryOpInstr : public Instr {
public:
    BinaryOpInstr(instr opc, Def *x1, Def *x2);
    void dump(unsigned indent = 0);
};



/** ------------------- Constants ------------------- */

class Constant : public Def {
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
    IntegralConstant(Type *ty, uint64_t num_value)
        : Constant(ty), value(num_value) { }

public:
    static IntegralConstant *get(Type *ty, uint64_t value);
    void dump(unsigned indent = 0);
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

class Param : public Def, public STPPIListNode<Param, Function> {
    Function *param_of;
    unsigned idx;

public:
    Param(Type *ty, Function *func, unsigned idx)
        : Def(ty), param_of(func), idx(idx) { }
    void dump(unsigned indent = 0) { }
};

class Function
    : public Global, public STPPIListNode<Function, Program> {
private:
    using param_list_ty = STPPIList<Param, Function>;
    using block_list_ty = STPPIList<Block, Function>;
    param_list_ty params;
    block_list_ty blocks;

    friend block_list_ty::node_type;
    block_list_ty &get_inner_list(Block *) { return blocks; }
    friend param_list_ty::node_type;
    param_list_ty &get_inner_list(Param *) { return params; }

public:
    Function(FunctionType *ty, linkage lty, Program *parent);
    Function(FunctionType *ty, linkage lty, Program *parent, std::string name)
        : Function(ty, lty, parent) { set_name(name); }
    FunctionType *get_type() { return static_cast<FunctionType *>(Global::get_type()); }
    Block *get_block(std::string name)
        { return blocks.get(name); }
    void add_block(Block *b) { blocks.append(b); }
    unsigned num_params() { return params.size(); }
    block_list_ty::forward_iterator begin() { return blocks.begin(); }
    block_list_ty::forward_iterator end() { return blocks.end(); }
    param_list_ty::forward_iterator params_begin() { return params.begin(); }
    param_list_ty::forward_iterator params_end() { return params.end(); }
    auto params_iterable() { return std::ranges::subrange(params.begin(), params.end()); }
    void dump(unsigned indent = 0);
    void dump_as_op(unsigned indent = 0, bool newline = true);
};

class Program {
private:
    std::string name;

    using gvar_list_ty = STPPIList<GlobalVar, Program>;
    using func_list_ty = STPPIList<Function, Program>;
    STPPIList<GlobalVar, Program> gvar_list;
    STPPIList<Function, Program> func_list;

    friend gvar_list_ty::node_type;
    gvar_list_ty &get_inner_list(GlobalVar *) { return gvar_list; }

    friend func_list_ty::node_type;
    func_list_ty &get_inner_list(Function *) { return func_list; }

public:
    Program(std::string name)
        : name(name), gvar_list(this), func_list(this) { }
    void add_glob(GlobalVar *gvar) { gvar_list.append(gvar); }
    GlobalVar *get_glob(std::string name)
        { return gvar_list.get(name); }
    Function *get_function(std::string name)
        { return func_list.get(name); }
};

}

#endif
