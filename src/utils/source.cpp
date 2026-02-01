#include <cassert>
#include <fstream>
#include <iostream>

#include "utils/source.h"

SourceLocation operator>>(SourceLocation x, SourceLocation y) {
    return { x.start, (y.start + y.len) - x.start };
}

SourceLocation &SourceLocation::operator>>=(SourceLocation y) {
    *this = operator>>(*this, y);
    return *this;
}

SourceManager *SourceManager::me = nullptr;
