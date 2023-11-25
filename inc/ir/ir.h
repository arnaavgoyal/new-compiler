#ifndef IR_H
#define IR_H

#include <map>
#include "analyzer/type.h"
#include "utils/ll.h"

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

namespace ir {

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
};

class Use : public ilist_node<Use> {
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
    ilist<Use> list;
    static unsigned int name_counter;

protected:
    Def(Def &) = default;
    Def(Def &&) = default;
    Def(Type const *ty) : type(ty) { }

public:
    Type const *get_type() { return type; }
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
    DefUser(Type const *ty, unsigned int num_ops);

public:
    void set_operand(unsigned int idx, Def *operand);
    Use *get_operand(unsigned int idx);
    void dump_operands(int indent = 0);
};

class Block : public Def, public ilist_node<Block> {
private:
    Function *parent;
    ilist<Instr> list;

protected:
    Block(Block &) = default;
    Block(Block &&) = default;

public:
    Block(Function *func);
    void add_instr(Instr *i);
    void dump(int indent) { }
};

/** ------------------- Instructions ------------------- */

class Instr : public DefUser, public ilist_node<Instr> {
private:
    ir::instr opcode;
    Block *parent;

protected:
    Instr(Instr &) = default;
    Instr(Instr &&) = default;
    Instr(Type const *ty, int num_ops, ir::instr opc, Block *b)
        : DefUser(ty, num_ops), opcode(opc), parent(b) { }

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
    ir::linkage linkage;

public:
    Global(Type const *ty, ir::linkage lty) : Constant(ty), linkage(lty) { }
    ir::linkage get_linkage() { return linkage; }
    void set_linkage(ir::linkage lty) { linkage = lty; }
};

class GlobalVar : public Global {
private:
    bool is_const;

public:
    GlobalVar(Type const *ty, ir::linkage lty, bool is_const)
        : Global(ty, lty), is_const(is_const) { }
};

class Param : public Def {
    Function *param_of;
    int idx;

public:
    Param(Type const *ty, Function *func, int idx)
        : Def(ty), param_of(func), idx(idx) { }
    void dump(int indent = 0) { }
};

class Function : public Global {
public:
    using symtable_ty = std::map<std::string *, Def *>;

private:
    std::vector<Param> params;
    symtable_ty symtable;
    ilist<Block> list;

public:
    Function(Type const *ty, ir::linkage lty);
    symtable_ty const &get_symtable() const { return symtable; };
    void add_block(Block *b) { list.append(b); }
    void dump(int indent = 0) { }
};

class Program {
public:
    std::vector<Global *> globs;
    std::vector<Function *> funcs;
};

#endif
