#ifndef IR_ANALYSIS_H
#define IR_ANALYSIS_H

#include <vector>
#include <map>
#include "ir/ir.h"

std::vector<ir::Block *> predecessors(ir::Block *b);

std::vector<ir::Block *> successors(ir::Block *b);

struct CFGVertex;

struct CFGEdge {
    CFGVertex *from;
    CFGVertex *to;
    CFGEdge(CFGVertex *from, CFGVertex *to) : from(from), to(to) { }
};

struct CFGVertex {
    using adjlist_ty = std::vector<CFGEdge *>;
    adjlist_ty out_edges;
    ir::Block *val;
    CFGVertex(ir::Block *val) : val(val) { }
};

class CFG {
public:
    using vertex_ty = CFGVertex;
    using edge_ty = CFGEdge;

private:
    using vertex_map_ty = std::map<ir::Block *, vertex_ty *>;
    vertex_map_ty vertices;
    std::vector<edge_ty *> edges;
    vertex_ty *root;

public:
    CFG(ir::Block *root) {
        this->root = add_vertex(root);
    }
    vertex_ty *get_root() { return root; }
    vertex_ty *get_vertex(ir::Block *b) {
        auto it = vertices.find(b);
        if (it == vertices.end()) {
            return nullptr;
        }
        return it.operator*().second;
    }
    vertex_ty *add_vertex(ir::Block *b) {
        vertex_ty *v = vertices.emplace(b, new vertex_ty(b)).first.operator*().second;
        return v;
    }
    edge_ty *add_edge(vertex_ty *from, vertex_ty *to) {
        edge_ty *edge = new edge_ty(from, to);
        edges.push_back(edge);
        from->out_edges.push_back(edge);
        return edge;
    }
    void dump(std::ostream &out);
};

CFG make_cfg(ir::Function *f);

#endif
