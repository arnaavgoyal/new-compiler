#ifndef CODEGEN_H
#define CODEGEN_H

#include "ir/ir.h"

class CodeGenerator {
public:
    void gen(ir::Program *prog);
};

#endif