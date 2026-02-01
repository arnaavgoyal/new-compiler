#ifndef ANALYZER_ANALYZER_H
#define ANALYZER_ANALYZER_H

#include <string>

#include "analyzer/type.h"
#include "ast/xast.h"

namespace fe {

void analyze(xast::Node *root, std::vector<fe::PrimitiveType *> const &primitives);

}

#endif
