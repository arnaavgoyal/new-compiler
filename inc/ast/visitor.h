#ifndef AST_VISITOR_H
#define AST_VISITOR_H

#include <type_traits>

#include "ast/scope.h"
#include "ast/xast.h"

namespace fe {
namespace xast {

template <typename Ret, typename... Ctx>
struct Visitor {

    Ret dispatch(Node *n, Ctx... ctx) {
        if (!n) {
            if constexpr (std::is_void_v<Ret>) { return; }
            else { return {}; }
        }
        switch(n->kind) {
        
#define NK(x, num, c) \
    case nk::x: return visit_##x(n, std::forward<Ctx>(ctx)...);
#include "ast/nodekinds"

        default: assert(false && "this should be unreachable");

        }
    }

#define NK(x, num, c)                                           \
    virtual Ret visit_##x(Node *n, Ctx... ctx) {                \
        for (auto edge : n->opedges) {                          \
            dispatch(edge->used(), std::forward<Ctx>(ctx)...);  \
        }                                                       \
        if constexpr (std::is_void_v<Ret>) { return; }          \
        else { return {}; }                                     \
    }
#include "ast/nodekinds"


    // entry function
    Ret operator()(Node *n, Ctx... ctx) {
        return dispatch(n, std::forward<Ctx>(ctx)...);
    }

};

} // xast
} // fe

#endif
