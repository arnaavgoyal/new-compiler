#ifndef IR_H
#define IR_H

#include <map>
#include "ir/type.h"
#include "utils/stppilist.h"
#include "utils/symtable.h"
#include "utils/iterator.h"

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

    // integral upcast (smaller -> bigger)
    iupcast,
    // integral downcast (bigger -> smaller)
    idowncast,

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

enum class cmpkind {
    ugt,
    ult,
    ugte,
    ulte,
    sgt,
    slt,
    sgte,
    slte,
    eq,
    neq
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

protected:
    Def(Def &) = default;
    Def(Def &&) = default;
    Def(Type *ty) : type(ty) { }

public:
    Type *get_type() { return type; }
    void add_use(Use *use);
    void remove_use(Use *use);
    virtual void dump(unsigned indent = 0);
    virtual void dump_as_operand();

    // iterators
    IList<Use>::iterator uses_begin() { return list.begin(); }
    IList<Use>::iterator uses_end() { return list.end(); }
    IList<Use>::reverse_iterator uses_rbegin() { return list.rbegin(); }
    IList<Use>::reverse_iterator uses_rend() { return list.rend(); }
    iterator_range<IList<Use>::iterator> uses_iterable()
        { return make_iterator_range(uses_begin(), uses_end()); }
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
    void dump(unsigned indent = 0);
    void dump_as_operand();
    void dump_operands();

private:
    class fwiter : public bidirectional_iterator<Use *, fwiter> {
    private:
        using bidirectional_iterator<Use *, fwiter>::curr;

        void go_forward() override { curr++; }
        void go_backward() override { curr--; }

    public:
        fwiter() { curr = nullptr; }
        fwiter(Use *ptr) { curr = ptr; }
    };
    class bwiter : public bidirectional_iterator<Use *, bwiter> {
    private:
        using bidirectional_iterator<Use *, bwiter>::curr;

        void go_forward() override { curr--; }
        void go_backward() override { curr++; }

    public:
        bwiter() { curr = nullptr; }
        bwiter(Use *ptr) { curr = ptr; }
    };

public:
    using iterator = fwiter;
    iterator operands_begin() { return iterator(operands); }
    iterator operands_end() { return iterator(operands + num_ops); }

    using reverse_iterator = bwiter;
    reverse_iterator operands_rbegin() { return reverse_iterator(operands + num_ops - 1); }
    reverse_iterator operands_rend() { return reverse_iterator(operands - 1); }
};

class Block : public Def, public STPPIListNode<Block, Function> {
private:
    using list_ty = STPPIList<Instr, Block>;
    list_ty list;

    friend list_ty::node_type;
    list_ty &get_inner_list(Instr *) { return list; }

    Instr *terminator = nullptr;

protected:
    Block(Block &) = default;
    Block(Block &&) = default;

public:
    Block(Function *parent, std::string name_hint = "")
        : Def(ir::PrimitiveType::get_label_type()), list(this) {
        if (!name_hint.empty()) {
            set_name(name_hint);
        }
        set_parent(parent);
    }
    std::string add_instr(Instr *i, std::string name_hint = "");
    Instr *get_instr(std::string name) { return list.get(name); }
    void remove_instr(Instr *instr);
    Instr *remove_instr(std::string name);
    unsigned size() { return list.size(); }
    Instr *get_terminator() { return terminator; }
    list_ty::iterator begin() { return list.begin(); }
    list_ty::iterator end() { return list.end(); }
    Instr *get_first_instr() { return list.first(); }
    Instr *get_last_instr() { return list.last(); }
    void dump(unsigned indent = 0);
    void dump_as_operand();
};

/** ------------------- Instructions ------------------- */

class Instr : public DefUser, public STPPIListNode<Instr, Block> {
private:
    static constexpr char const *const STR_REPR = "<instr>";
    instr kind;
    bool term;

protected:
    Instr(Instr &) = default;
    Instr(Instr &&) = default;
    Instr(Type *ty, unsigned num_ops, instr kind, bool term,
        Block *parent = nullptr, Instr *before = nullptr,
        std::string name_hint = "")
        : DefUser(ty, num_ops), kind(kind), term(term) {
        if (parent) { set_parent(parent); }
        if (!name_hint.empty()) { set_name(name_hint); }
    }
    Instr(Type *ty, unsigned num_ops, instr kind,
        Block *parent = nullptr, Instr *before = nullptr,
        std::string name_hint = "")
        : Instr(ty, num_ops, kind, false, parent, before, name_hint) { }

public:
    instr get_instr_kind() { return kind; }
    void dump(unsigned indent = 0);
    void dump_as_operand();
    virtual std::string get_str_repr() { return STR_REPR; }
    bool is_terminator() { return term; }
};

class ReturnInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "return";

public:
    std::string get_str_repr() { return STR_REPR; }
    
public:
    ReturnInstr(Def *retval, Block *parent)
        : Instr(retval->get_type(), 1, instr::ret, true, parent) { set_operand(0, retval); }
};

class BranchInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "branch";

public:
    std::string get_str_repr() { return STR_REPR; }

private:
    bool conditional;

public:
    BranchInstr(Block *jmp_true, Def *cond, Block *jmp_false, Block *parent = nullptr);
    BranchInstr(Block *jmp, Block *parent = nullptr);
    bool is_conditional() { return conditional; }
    Block *get_jmp_true() { return static_cast<Block *>(get_operand(0)->get_def()); }
    Block *get_jmp_false() {
        assert(is_conditional() && "unconditional branch instrs do not have a jmp false block");
        return static_cast<Block *>(get_operand(2)->get_def());
    }
    Def *get_cond() {
        assert(is_conditional() && "unconditional branch instrs do not have a cond");
        return get_operand(1)->get_def();
    }
};

class SAllocInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "salloc";

public:
    std::string get_str_repr() { return STR_REPR; }

private:
    Type *alloc_ty;

public:
    SAllocInstr(Type *alloc_type, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(PrimitiveType::get_ptr_type(), 0, instr::salloc, parent, before, name_hint),
        alloc_ty(alloc_type) { }
    Type *get_alloc_ty() { return alloc_ty; }
    void dump(unsigned indent = 0);
};

class ReadInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "read";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    ReadInstr(Type *val_type, Def *mem_ptr, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(val_type, 1, instr::read, parent, before, name_hint) {
        assert(mem_ptr->get_type() == PrimitiveType::get_ptr_type() && "mem_ptr must be of pointer type");
        set_operand(0, mem_ptr);
    }
    void dump(unsigned indent = 0);
};

class WriteInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "write";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    WriteInstr(Def *val, Def *mem, Block *parent = nullptr)
        : Instr(PrimitiveType::get_void_type(), 2, instr::write, parent) {
        set_operand(0, val);
        set_operand(1, mem);
    }
};

class PtrIdxInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "ptridx";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    PtrIdxInstr(Def *ptr_val, Def *idx_val, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(PrimitiveType::get_ptr_type(), 2, instr::ptridx, parent, before, name_hint) {
        assert(ptr_val->get_type() == PrimitiveType::get_ptr_type() && "ptr_val must be of pointer type");
        set_operand(0, ptr_val);
        set_operand(1, idx_val);
    }
};

class IUpcastInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "iuc";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    IUpcastInstr(Def *val, Type *ty, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(ty, 1, instr::iupcast, parent, before, name_hint) {
        set_operand(0, val);
        // TODO: check for invalid upcast
    }
    void dump(unsigned indent = 0);
};

class IDowncastInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "idc";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    IDowncastInstr(Def *val, Type *ty, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(ty, 1, instr::idowncast, parent, before, name_hint) {
        set_operand(0, val);
        // TODO: check for invalid downcast
    }
    void dump(unsigned indent = 0);
};

class ICmpInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "icmp";

public:
    std::string get_str_repr() { return STR_REPR; }

private:
    cmpkind kind;

public:
    ICmpInstr(cmpkind kind, Def *x, Def *y, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(PrimitiveType::get_i8_type(), 2, instr::icmp, parent, before, name_hint), kind(kind) {
        assert(x->get_type() == y->get_type() && "operands must be of equivalent type");
        assert(x->get_type()->is_integral_type() && "operands must be of integral type");
        set_operand(0, x);
        set_operand(1, y);
    }
    void dump(unsigned indent = 0);
};

class CallInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "call";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    CallInstr(Function *callee, std::vector<Def *> args, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "");
};

class BinaryOpInstr : public Instr {
public:
    BinaryOpInstr(instr opc, Def *x1, Def *x2, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "");
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
    void dump_as_operand();
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
    GlobalVar(Type *ty, linkage lty, Program *parent, bool is_const, std::string name_hint)
        : Global(ty, lty), is_const(is_const) {
        assert(!name_hint.empty() && "global variables must be named");
        set_name(name_hint);
        set_parent(parent);
    }
    void dump(unsigned indent = 0);
    void dump_as_operand();
};

class Param : public Def, public STPPIListNode<Param, Function> {
    Function *param_of;
    unsigned idx;

public:
    Param(Type *ty, Function *func, unsigned idx, std::string name_hint = "")
        : Def(ty), param_of(func), idx(idx) {
        if (!name_hint.empty()) {
            set_name(name_hint);
        }
        set_parent(func);
    }
    void dump(unsigned indent = 0);
    void dump_as_operand();
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
    Function(Type *return_ty, linkage lty, Program *parent, std::string name_hint);
    Type *get_type() { return Global::get_type(); }
    Block *get_block(std::string name) { return blocks.get(name); }
    void add_block(Block *b) { blocks.append(b); }
    unsigned num_params() { return params.size(); }
    block_list_ty::iterator begin() { return blocks.begin(); }
    block_list_ty::iterator end() { return blocks.end(); }
    Block *get_first_block() { return blocks.first(); }
    Block *get_last_block() { return blocks.last(); }
    param_list_ty::iterator params_begin() { return params.begin(); }
    param_list_ty::iterator params_end() { return params.end(); }
    auto params_iterable() { return make_iterator_range(params.begin(), params.end()); }
    void dump(unsigned indent = 0);
    void dump_as_operand();
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
    GlobalVar *get_glob(std::string name) { return gvar_list.get(name); }
    void add_function(Function *func) { func_list.append(func); }
    Function *get_function(std::string name) { return func_list.get(name); }
    void dump();

    // iterator forwarding
    func_list_ty::iterator begin() { return func_list.begin(); }
    func_list_ty::iterator end() { return func_list.end(); }
    gvar_list_ty::iterator gvar_begin() { return gvar_list.begin(); }
    gvar_list_ty::iterator gvar_end() { return gvar_list.end(); }
};

}

#endif
