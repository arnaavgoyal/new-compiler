#ifndef CODEGEN_X86_64_GEN
#define CODEGEN_X86_64_GEN

#include "codegen/codegen.h"
#include "ir/ir.h"

namespace be {

struct x86_64CodeGen : public TargetCodeGen {
    void ccvl(TargetFunction &tf, ir::Function *f) override;
    bool isel(TargetFunction &tf, TargetInstr *ti) override;
    unsigned ralloc() override;
    void rfree(unsigned reg) override;
    void rfree_all() override;
    void pasm(TargetProgram &tp, std::ostream &os) override;

    std::string targetstr() { return "x86"; }
    std::string instrstr(unsigned opcode);
    std::string pregstr(unsigned preg);
    bool is_preg(unsigned reg);
    unsigned new_vreg();

private:
    void parg(TargetValue *tv, std::ostream &os);
};

}

#endif
