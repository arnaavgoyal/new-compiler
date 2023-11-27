#ifndef IR_BUILDER_H
#define IR_BUILDER_H

#include <map>
#include "lexer/tokentypes.h"
#include "ir/ir.h"

namespace ir {

class Builder {
private:
    std::map<std::string const, Function *> funcs;

public:

    /** ------------------- Builtin Types ------------------- */

    static Type const *const bi_void;
    static Type const *const bi_i8;
    static Type const *const bi_u8;
    static Type const *const bi_i16;
    static Type const *const bi_u16;
    static Type const *const bi_i32;
    static Type const *const bi_u32;
    static Type const *const bi_i64;
    static Type const *const bi_u64;
    static Type const *const bi_f32;
    static Type const *const bi_f64;

    /** ------------------- Instructions ------------------- */

    static   ReturnInstr *make_return();
    static   BranchInstr *make_branch();
    static   SAllocInstr *make_salloc();
    static      ReadInstr *make_get();
    static    WriteInstr *make_store();
    static   PtrIdxInstr *make_ptridx();
    static   UpcastInstr *make_upcast();
    static DowncastInstr *make_downcast();
    static     ICmpInstr *make_icmp();
    static     CallInstr *make_call(Function *callee);

    /** ------------------- Constants ------------------- */

    static IntegralConstant *int_const(uint64_t value);

    /** ------------------- Other IR Constructs ------------------- */

    static Block *make_block();
    static Function *make_function(std::string const &name, Type const *func_type, ir::linkage linkage);
    static GlobalVar *make_global_var();

    static Function *get_func(std::string const &name);
};

}

#endif