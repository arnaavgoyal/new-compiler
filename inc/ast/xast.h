#ifndef AST_XAST_H
#define AST_XAST_H

#include <cassert>
#include <string>
#include <vector>

#include "ast/op.h"
#include "utils/identifier.h"
#include "utils/ilist.h"
#include "utils/source.h"

namespace fe {

class Type;
class Scope;

namespace xast {

enum class nk : uint8_t {

    __decl_start,

#define DECL(x, n, c) x,
#include "ast/nodekinds"

    __decl_end,

#define NK(x, n, c) x,
#define DECL(x, n, c)
#include "ast/nodekinds"

};

bool is_decl(nk k);

struct Node;

struct Use : public IListNode<Use> {
private:
    Node *_user = nullptr;
    Node *_used = nullptr;

public:
    Use(Node *user) : _user(user) { }
    Node *used() { return _used; }
    void set(Node *n);
    Node *user() { return _user; }
};

struct NodeData {
    enum kind {
        _none,
        _ident,
        _ival,
        _op
    } payload = _none;

    union {
        char none;
        Identifier ident;
        uint64_t ival;
        Op op;
    };

    NodeData() : payload(_none), none(0) { }

#define CONSTRUCTOR(type, member, enumval) \
    NodeData(type const v) : payload(enumval), member(v) { }
    CONSTRUCTOR(Identifier, ident, _ident)
    CONSTRUCTOR(uint64_t, ival, _ival)
    CONSTRUCTOR(Op, op, _op)
#undef CONSTRUCTOR

#define ASSIGNER(type, member, enumval) \
    NodeData &operator=(type const v) {  \
        if (payload == enumval) {       \
            member = v;                 \
        } else {                        \
            member = v;                 \
            payload = enumval;          \
        }                               \
        return *this;                   \
    }
    ASSIGNER(Identifier, ident, _ident)
    ASSIGNER(uint64_t, ival, _ival)
    ASSIGNER(Op, op, _op)
#undef ASSIGNER

#define CONVERTER(type, member, enumval) \
    operator type() {                    \
        assert(payload == enumval);      \
        return member;                   \
    }
    CONVERTER(Identifier, ident, _ident)
    CONVERTER(uint64_t, ival, _ival)
    CONVERTER(Op, op, _op)
#undef CONVERTER

#define CHECKER(member, enumval)   \
    bool is_##member() {           \
        return payload == enumval; \
    }
    CHECKER(ident, _ident)
    CHECKER(ival, _ival)
    CHECKER(op, _op)
#undef CHECKER

};

struct Node {

    // DO NOT MODIFY FROM HERE
    IList<Use> uses;
    std::vector<Use*> opedges;

    nk kind = nk::null;
    SourceLocation sloc;
    NodeData data;
    bool meta = false;

    // populated during analysis
    Scope *scope = nullptr; // if this node creates a new scope
    Type *type = nullptr;
    

    // Default constructor
    Node() = default;

    // Constructor that reserves concrete children based on node kind
    Node(nk k) : kind(k) {
        size_t num = num_children_for_kind(k);
        for (size_t i = 0; i < num; ++i) {
            add(nullptr);
        }
    }

    Node &add(Node *n) {
        auto edge = new Use(this);
        opedges.push_back(edge);
        edge->set(n);
        return *this;
    }

private:
    // Helper to get the number of concrete (required) children for a node kind
    static constexpr size_t num_children_for_kind(nk k) {
        switch(k) {
#define NK(x, num, children) case nk::x: return num;
#include "ast/nodekinds"
#undef NK
        }
        return 0;
    }
    
};

void dump(Node *node, std::string stem = "");

namespace c {

// proxy object for accessing child nodes
class ChildNodeRef {
private:
    Use* edge;
    
public:
    ChildNodeRef(Use* e) : edge(e) {
        assert(e);
    }
    
    // read: implicit conversion to Node*
    operator Node *() {
        return edge->used();
    }
    
    // read: pointer dereference
    Node *operator->() {
        return edge->used();
    }
    
    // write: assignment updates the use edge
    ChildNodeRef &operator=(Node* n) {
        edge->set(n);
        return *this;
    }
    
    Node *get() {
        return edge->used();
    }
};

// Helper function to safely get child at index
// Returns nullptr if child doesn't exist or index is out of bounds
inline ChildNodeRef at(Node* node, int index) {
    assert(node);
    auto cnt = (int)node->opedges.size();
    if (index < 0) {
        assert(cnt + index >= 0 && "reverse index out of bounds");
        return node->opedges[cnt + index];
    }
    assert(index < node->opedges.size() && "index out of bounds");
    return node->opedges[index];
}

inline size_t count(Node* node) {
    assert(node);
    return node->opedges.size();
}

#define NK(x, num_concrete, children) \
    namespace x { children }

// concrete accessors (fixed position children)
#define ACCESSOR(nodekind, accessor_name, index)    \
    inline ChildNodeRef accessor_name(Node* node) { \
        assert(node && node->kind == nk::nodekind); \
        return at(node, index);                     \
    }

// variadic accessors (variable-length children)
#define VARIADIC_ACCESSOR(nodekind, accessor_name, start)             \
    inline ChildNodeRef accessor_name(Node* node, size_t var_index) { \
        assert(node && node->kind == nk::nodekind);                   \
        auto ind = start + var_index;                                 \
        auto cnt = count(node);                                       \
        assert(ind < cnt && "field index out of range");              \
        return at(node, ind);                                         \
    }                                                                 \
    inline size_t accessor_name##_count(Node* node) {                 \
        assert(node);                                                 \
        auto cnt = count(node);                                       \
        assert(cnt >= start && "no concrete before variadic?");       \
        auto num_var = cnt - start;                                   \
        return num_var;                                               \
    }

#include "ast/nodekinds"

#undef VARIADIC_ACCESSOR
#undef ACCESSOR
#undef NK

} // c


} // xast
} // fe

#endif
