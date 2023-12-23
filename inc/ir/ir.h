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

enum class defkind {

    param,
    block,

    // constant
    _constant_start,

        integral_constant,

        // global
        globalvar,
        function,

    _constant_end,


    // instruction
    _instr_start,

        // return
        ret,
        // branch
        branch,

        // stack mem alloc
        salloc,
        // read mem
        read,
        // write mem
        write,
        // index a ptr
        ptridx,

        // typecast between types of same size
        typecast,
        // integral upcast (smaller -> bigger)
        iupcast,
        // integral downcast (bigger -> smaller)
        idowncast,

        // binary operations
        _binop_start,
        iadd,
        isub,
        imul,
        idiv,
        _binop_end,

        // compare integral types
        icmp,
        // call a function
        call,
        // ssa phi function
        phi,
    
    _instr_end,
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
    DefUser *owner = nullptr;
    Def *def = nullptr;
    unsigned idx = 0;

public:
    Use(DefUser *user, unsigned idx)
        : owner(user), idx(idx) { }
    Def *get_def() { return def; }
    void set_def(Def *def);
    DefUser *get_user() { return owner; }
    unsigned get_idx() { return idx; }
};

class Def {
private:
    defkind kind;
    Type *type;
    IList<Use> list;

    friend Use;
    void add_use(Use *use);
    void remove_use(Use *use);

protected:
    Def(Def &) = default;
    Def(Def &&) = default;
    Def(defkind kind, Type *ty) : kind(kind), type(ty) {
        //std::cout << " ctor Def" << std::endl;
        //std::cout << "  done ctor Def" << std::endl;
    }

public:
    virtual ~Def() = default;
    defkind get_kind() { return kind; }
    Type *get_type() { return type; }
    virtual void dump(unsigned indent = 0);
    virtual void dump_as_operand();

    bool is_instr() { return kind > defkind::_instr_start && kind < defkind::_instr_end; }

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
    unsigned num_ops;
    bool has_var_oplist;

    Use *&var_oplist() {
        assert(has_var_oplist);
        return reinterpret_cast<Use **>(this)[-1];
    }

protected:
    void *operator new(size_t size);
    void *operator new(size_t size, unsigned num_ops);
    DefUser(DefUser &) = default;
    DefUser(DefUser &&) = default;
    DefUser(defkind kind, Type *ty, unsigned num_ops);
    
    Use &use(unsigned idx) {
        assert(idx < num_ops && "idx out of range");
        return oplist()[idx];
    }
    Use *oplist() {
        if (has_var_oplist) {
            return var_oplist();
        }
        return reinterpret_cast<Use *>(this) - num_ops;
    }
    void set_num_ops(unsigned num) {
        assert(has_var_oplist && "cannot change num ops with fixed oplist");
        num_ops = num;
    }
    void set_oplist(Use *oplist) {
        assert(has_var_oplist && "cannot change oplist with fixed oplist");
        var_oplist() = oplist;
    }
    void realloc_oplist(unsigned num) {
        assert(has_var_oplist && "");
        assert(num > num_ops);
        //std::cout << "realloc'ing var oplist\n"
        //    << "  curr size: " << num_ops << "\n"
        //    << "  new size: " << num << "\n";
        Use *list = static_cast<Use *>(::operator new(num * sizeof(Use)));
        if (oplist()) {
            auto oplist_start = oplist();
            for (unsigned i = 0; i < num_ops; i++) {
                new(list + i) Use(this, i);
                list[i].set_def(oplist_start[i].get_def());
            }
        }
        else {
            assert(num_ops == 0);
        }
        for (unsigned i = num_ops; i < num; i++) {
            new(list + i) Use(this, i);
        }
        num_ops = num;
        var_oplist() = list;
        //std::cout << "  done\n";
    }

public:
    void operator delete(void *obj);
    void operator delete(void *obj, unsigned) { DefUser::operator delete(obj); }
    unsigned get_num_ops() { return num_ops; }
    void set_operand(unsigned idx, Def *operand) { use(idx).set_def(operand); }
    Def *get_operand(unsigned idx) { return use(idx).get_def(); }
    Use *get_operand_list() { return oplist(); }
    void dump(unsigned indent = 0);
    void dump_as_operand();
    void dump_operands();

private:
    class fwiter : public bidirectional_iterator<Def *, fwiter> {
    private:
        using bidirectional_iterator<Def *, fwiter>::curr;
        Use *use_curr;

        void go_forward() override { use_curr++; curr = use_curr->get_def(); }
        void go_backward() override { use_curr--; curr = use_curr->get_def(); }

    public:
        fwiter() { use_curr = nullptr; curr = nullptr; }
        fwiter(Use *ptr) { use_curr = ptr; curr = use_curr->get_def(); }
    };
    class bwiter : public bidirectional_iterator<Def *, bwiter> {
    private:
        using bidirectional_iterator<Def *, bwiter>::curr;
        Use *use_curr;

        void go_forward() override { use_curr--; curr = use_curr->get_def(); }
        void go_backward() override { use_curr++; curr = use_curr->get_def(); }

    public:
        bwiter() { use_curr = nullptr; curr = nullptr; }
        bwiter(Use *ptr) { use_curr = ptr; curr = use_curr->get_def(); }
    };

public:
    using iterator = Use *;
    iterator operands_begin() { return oplist(); }
    iterator operands_end() { return oplist() + num_ops; }

    using reverse_iterator = Use *;
    reverse_iterator operands_rbegin() { return oplist() + num_ops - 1; }
    reverse_iterator operands_rend() { return oplist() - 1; }

    using def_iterator = fwiter;
    def_iterator def_operands_begin() { return def_iterator(oplist()); }
    def_iterator def_operands_end() { return def_iterator(oplist() + num_ops); }

    using def_reverse_iterator = bwiter;
    def_reverse_iterator def_operands_rbegin() { return def_reverse_iterator(oplist() + num_ops - 1); }
    def_reverse_iterator def_operands_rend() { return def_reverse_iterator(oplist() - 1); }
};

class Block : public Def, public STPPIListNode<Block, Function> {
private:
    using list_ty = STPPIList<Instr, Block>;
    list_ty list;

    friend list_ty::node_type;
    list_ty &get_inner_list(Instr *) { return list; }

    friend list_ty;
    AutoRenamingSymbolTable<Instr *> &get_symtable(Instr *);

protected:
    Block(Block &) = default;
    Block(Block &&) = default;

public:
    Block(Function *parent, std::string name_hint = "")
        : Def(defkind::block, ir::PrimitiveType::get_block_type()), list(this) {
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
    bool term;

protected:
    void *operator new(size_t size) {
        //std::cout << "instr new var entry" << std::endl;
        auto ptr = DefUser::operator new(size);
        //std::cout << "  instr new var exit" << std::endl;
        return ptr;
    }
    void *operator new(size_t size, unsigned num_ops) {
        //std::cout << "instr new fixed entry" << std::endl;
        auto ptr = DefUser::operator new(size, num_ops);
        //std::cout << "  instr new fixed exit" << std::endl;
        return ptr;
    }
    Instr(Instr &) = default;
    Instr(Instr &&) = default;
    Instr(defkind kind, Type *ty, unsigned num_ops, bool term,
        Block *parent = nullptr, Instr *before = nullptr,
        std::string name_hint = "")
        : DefUser(kind, ty, num_ops), term(term) {
        //std::cout << "ctor Instr" << std::endl;
        if (parent) { set_parent(parent); }
        if (!name_hint.empty()) { set_name(name_hint); }
        //std::cout << "  done ctor Instr" << std::endl;
    }
    Instr(defkind kind, Type *ty, unsigned num_ops,
        Block *parent = nullptr, Instr *before = nullptr,
        std::string name_hint = "")
        : Instr(kind, ty, num_ops, false, parent, before, name_hint) { }

public:
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
    void *operator new(size_t size) { return Instr::operator new(size, 1); }
    ReturnInstr(Def *retval, Block *parent)
        : Instr(defkind::ret, retval->get_type(), 1, true, parent) { set_operand(0, retval); }
};

class BranchInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "branch";

public:
    std::string get_str_repr() { return STR_REPR; }

private:
    bool conditional;

public:
    void *operator new(size_t size) {
        //std::cout << "branch new var entry" << std::endl;
        auto ptr = Instr::operator new(size);
        //std::cout << "  branch new var exit " << ptr << std::endl;
        return ptr;
    }
    BranchInstr(Def *cond, Block *jmp_true, Block *jmp_false, Block *parent = nullptr);
    BranchInstr(Block *jmp, Block *parent = nullptr);
    bool is_conditional() { return conditional; }
    Block *get_jmp_true() { return static_cast<Block *>(get_operand(0)); }
    Block *get_jmp_false() {
        assert(is_conditional() && "unconditional branch instrs do not have a jmp false block");
        return static_cast<Block *>(get_operand(2));
    }
    Def *get_cond() {
        assert(is_conditional() && "unconditional branch instrs do not have a cond");
        return get_operand(1);
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
    void *operator new(size_t size) { return Instr::operator new(size, 0); }
    SAllocInstr(Type *alloc_type, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(defkind::salloc, PrimitiveType::get_ptr_type(), 0, parent, before, name_hint),
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
    void *operator new(size_t size) { return Instr::operator new(size, 1); }
    ReadInstr(Type *val_type, Def *mem_ptr, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(defkind::read, val_type, 1, parent, before, name_hint) {
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
    void *operator new(size_t size) { return Instr::operator new(size, 2); }
    WriteInstr(Def *val, Def *mem, Block *parent = nullptr)
        : Instr(defkind::write, PrimitiveType::get_void_type(), 2, parent) {
        set_operand(0, val);
        set_operand(1, mem);
    }
    Def *get_val() { return get_operand(0); }
};

class PtrIdxInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "ptridx";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    void *operator new(size_t size) { return Instr::operator new(size, 2); }
    PtrIdxInstr(Def *ptr_val, Def *idx_val, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(defkind::ptridx, PrimitiveType::get_ptr_type(), 2, parent, before, name_hint) {
        assert(ptr_val->get_type() == PrimitiveType::get_ptr_type() && "ptr_val must be of pointer type");
        set_operand(0, ptr_val);
        set_operand(1, idx_val);
    }
};

class TypecastInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "cast";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    void *operator new(size_t size) { return Instr::operator new(size, 1); }
    TypecastInstr(Def *val, Type *ty, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(defkind::typecast, ty, 1, parent, before, name_hint) {
        set_operand(0, val);
        // TODO: check for invalid cast (types are different sizes)
    }
    void dump(unsigned indent = 0);
};

class IUpcastInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "iucast";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    void *operator new(size_t size) { return Instr::operator new(size, 1); }
    IUpcastInstr(Def *val, Type *ty, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(defkind::iupcast, ty, 1, parent, before, name_hint) {
        set_operand(0, val);
        // TODO: check for invalid upcast
    }
    void dump(unsigned indent = 0);
};

class IDowncastInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "idcast";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    void *operator new(size_t size) { return Instr::operator new(size, 1); }
    IDowncastInstr(Def *val, Type *ty, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(defkind::idowncast, ty, 1, parent, before, name_hint) {
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
    void *operator new(size_t size) { return Instr::operator new(size, 2); }
    ICmpInstr(cmpkind kind, Def *x, Def *y, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "")
        : Instr(defkind::icmp, PrimitiveType::get_i1_type(), 2, parent, before, name_hint), kind(kind) {
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
    void *operator new(size_t size) { return Instr::operator new(size); }
    CallInstr(Function *callee, std::vector<Def *> args, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "");
};

class PhiInstr : public Instr {
private:
    static constexpr char const *const STR_REPR = "phi";

public:
    std::string get_str_repr() { return STR_REPR; }

public:
    void *operator new(size_t size) { return Instr::operator new(size); }
    PhiInstr(Type *ty, Block *parent = nullptr, std::string name_hint = "")
        : Instr(defkind::phi, ty, 0, nullptr, nullptr, name_hint) {
        if (parent) {
            insert_before(parent->get_first_instr());
        }
    }

private:
    template <typename... Args>
    void add_alternatives_impl(unsigned idx, Args... args) {
        assert(false && "parameters are wrong");
    }
    void add_alternatives_impl(unsigned idx) { }
    template <typename... Args>
    void add_alternatives_impl(unsigned idx, Block *from, Def *value, Args... args) {
        use(idx).set_def(from);
        use(idx + 1).set_def(value);
        add_alternatives_impl(idx + 2, args...);
    }
public:
    template <typename... Args>
    void add_alternatives(Block *from, Def *value, Args... args) {
        unsigned idx = get_num_ops();
        realloc_oplist(idx + 2 + sizeof...(Args));
        add_alternatives_impl(idx, from, value, args...);
    }
};

class BinaryOpInstr : public Instr {
public:
    void *operator new(size_t size) { return Instr::operator new(size, 2); }
    BinaryOpInstr(defkind opc, Def *x1, Def *x2, Block *parent = nullptr, Instr *before = nullptr, std::string name_hint = "");
};

/** ------------------- Constants ------------------- */

class Constant : public Def {
protected:
    Constant(Constant &) = default;
    Constant(Constant &&) = default;
    Constant(defkind kind, Type *ty) : Def(kind, ty) { }
};

class IntegralConstant : public Constant {
public:
    using map_type = std::map<std::pair<Type *, uint64_t>, IntegralConstant *>;

private:
    friend map_type;
    static map_type vals;
    uint64_t value;

    IntegralConstant(IntegralConstant &) = delete;
    IntegralConstant(IntegralConstant &&) = delete;
    IntegralConstant(Type *ty, uint64_t num_value)
        : Constant(defkind::integral_constant, ty), value(num_value) { }

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
    Global(defkind kind, Type *ty, linkage lty) : Constant(kind, ty), lty(lty) { }

public:
    linkage get_linkage() { return lty; }
    void set_linkage(linkage linkage_ty) { lty = linkage_ty; }
};

class GlobalVar : public Global, public STPPIListNode<GlobalVar, Program> {
private:
    bool is_const;

public:
    GlobalVar(Type *ty, linkage lty, Program *parent, bool is_const, std::string name_hint)
        : Global(defkind::globalvar, ty, lty), is_const(is_const) {
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
        : Def(defkind::param, ty), param_of(func), idx(idx) {
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
    using block_symtable_ty = AutoRenamingSymbolTable<Block *>;
    using param_symtable_ty = AutoRenamingSymbolTable<Param *>;
    using instr_symtable_ty = AutoRenamingSymbolTable<Instr *>;
    block_symtable_ty block_symtable;
    param_symtable_ty param_symtable;
    instr_symtable_ty instr_symtable;

    using param_list_ty = STPPIList<Param, Function>;
    using block_list_ty = STPPIList<Block, Function>;
    param_list_ty params;
    block_list_ty blocks;

    friend block_list_ty::node_type;
    block_list_ty &get_inner_list(Block *) { return blocks; }
    friend param_list_ty::node_type;
    param_list_ty &get_inner_list(Param *) { return params; }
    friend block_list_ty;
    block_symtable_ty &get_symtable(Block *) { return block_symtable; }
    friend param_list_ty;
    param_symtable_ty &get_symtable(Param *) { return param_symtable; }
    friend Block;
    instr_symtable_ty &get_symtable(Instr *) { return instr_symtable; }

public:
    Function(Type *return_ty, linkage lty, Program *parent, std::string name_hint);
    Type *get_type() { return Global::get_type(); }
    void dump(unsigned indent = 0);
    void dump_as_operand();

    unsigned size() { return blocks.size(); }
    Block *get_block(std::string name) { return blocks.get(name); }
    void add_block(Block *b) { blocks.append(b); }
    block_list_ty::iterator begin() { return blocks.begin(); }
    block_list_ty::iterator end() { return blocks.end(); }
    Block *get_first_block() { return blocks.first(); }
    Block *get_last_block() { return blocks.last(); }

    unsigned num_params() { return params.size(); }
    param_list_ty::iterator params_begin() { return params.begin(); }
    param_list_ty::iterator params_end() { return params.end(); }
    iterator_range<IList<ir::Param>::iterator> params_iterable()
        { return make_iterator_range(params.begin(), params.end()); }
};

class Program {
private:
    std::string name;

    using gvar_symtable_ty = AutoRenamingSymbolTable<GlobalVar *>;
    using func_symtable_ty = AutoRenamingSymbolTable<Function *>;
    gvar_symtable_ty gvar_symtable;
    func_symtable_ty func_symtable;

    using gvar_list_ty = STPPIList<GlobalVar, Program>;
    using func_list_ty = STPPIList<Function, Program>;
    STPPIList<GlobalVar, Program> gvar_list;
    STPPIList<Function, Program> func_list;

    friend gvar_list_ty::node_type;
    gvar_list_ty &get_inner_list(GlobalVar *) { return gvar_list; }
    friend func_list_ty::node_type;
    func_list_ty &get_inner_list(Function *) { return func_list; }
    friend gvar_list_ty;
    gvar_symtable_ty &get_symtable(GlobalVar *) { return gvar_symtable; }
    friend func_list_ty;
    func_symtable_ty &get_symtable(Function *) { return func_symtable; }

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
