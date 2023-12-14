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
    adjlist_ty in_edges;
    ir::Block *val;
    unsigned id;

    CFGVertex(ir::Block *val, unsigned id) : val(val), id(id) { }
};

class CFG {
public:
    using vertex_ty = CFGVertex;
    using edge_ty = CFGEdge;

public:
    using vertex_map_ty = std::map<ir::Block *, vertex_ty *>;
    vertex_map_ty vmap;
    std::vector<vertex_ty *> vertices;
    std::vector<edge_ty *> edges;
    vertex_ty *root;
    unsigned id_counter = 0;

public:
    CFG(ir::Block *root) {
        this->root = add_vertex(root);
    }
    vertex_ty *get_root() { return root; }
    vertex_ty *get_vertex(ir::Block *b) {
        auto it = vmap.find(b);
        if (it == vmap.end()) {
            return nullptr;
        }
        return it.operator*().second;
    }
    vertex_ty *get_vertex(unsigned id) {
        assert(id < vertices.size() && "invalid id");
        return vertices[id];
    }
    vertex_ty *add_vertex(ir::Block *b) {
        vertex_ty *v = new vertex_ty(b, id_counter++);
        vertices.push_back(v);
        vmap.emplace(b, v).first.operator*().second;
        return v;
    }
    edge_ty *add_edge(vertex_ty *from, vertex_ty *to) {
        edge_ty *edge = new edge_ty(from, to);
        edges.push_back(edge);
        from->out_edges.push_back(edge);
        to->in_edges.push_back(edge);
        return edge;
    }
    void dump(std::ostream &out);
};

CFG make_cfg(ir::Function *f);

#endif
