#ifndef IR_CFG_H
#define IR_CFG_H

#include <vector>
#include <map>
#include "ir/ir.h"

std::vector<ir::Block *> predecessors(ir::Block *b);

std::vector<ir::Block *> successors(ir::Block *b);

void dump_cfg(ir::Function *cfg, std::ostream &out);

#endif
