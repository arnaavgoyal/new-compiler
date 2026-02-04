#ifndef ANALYZER_ANALYZER_H
#define ANALYZER_ANALYZER_H

#include <string>

#include "analyzer/type.h"
#include "ast/xast.h"
#include "utils/memory.h"

namespace fe {

void analyze(xast::Node *root, std::vector<fe::PrimitiveType *> const &primitives, StringPool &strings);

}

#endif
