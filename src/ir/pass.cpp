#include "ir/pass.h"
#include <algorithm>
#include <deque>
#include <set>
#include <iostream>
#include <fstream>
#include "utils/ioformat.h"
#include <string.h>

// dominance tree and IDF calculation based on:
// https://dl.acm.org/doi/pdf/10.1145/199448.199464

struct DJGVertex;

struct DJGEdge {
    enum djgekind : bool { d_edge = false, j_edge = true } kind;
    DJGVertex *from = nullptr;
    DJGVertex *to = nullptr;
    DJGEdge(djgekind kind, DJGVertex *from, DJGVertex *to)
        : kind(kind), from(from), to(to) { }
};

struct DJGVertex {
    bool visited = false;
    bool inphi = false;
    bool alpha = false;
    unsigned level = 0;
    ir::Block *block = nullptr;
    std::vector<DJGEdge *> out_edges;
    DJGEdge *in_d_edge = nullptr;

    DJGVertex(ir::Block *block, unsigned level)
        : level(level), block(block) { }
    void reset() {
        visited = false;
        inphi = false;
        alpha = false;
    }
};

class DJG {
public:
    using vertex_ty = DJGVertex;
    using edge_ty = DJGEdge;

public:
    using vertex_map_ty = std::map<ir::Block *, vertex_ty *>;
    vertex_map_ty vertices;
    std::vector<edge_ty *> edges;
    vertex_ty *root;
    unsigned max_level = 0;

private:
    edge_ty *add_edge(DJGEdge::djgekind kind, vertex_ty *from, vertex_ty *to) {
        edge_ty *edge = new edge_ty(kind, from, to);
        edges.push_back(edge);
        from->out_edges.push_back(edge);
        to->in_d_edge = edge;
        return edge;
    }

public:
    DJG(ir::Block *b) {
        root = vertices.emplace(b, new vertex_ty(b, 0)).first.operator*().second;
        add_edge(DJGEdge::d_edge, root, root);
    }
    vertex_ty *get_root() { return root; }
    vertex_ty *get_vertex(ir::Block *b) {
        auto it = vertices.find(b);
        if (it == vertices.end()) {
            return nullptr;
        }
        return it.operator*().second;
    }
    vertex_ty *add_vertex(vertex_ty *from, ir::Block *to) {
        unsigned lvl = from->level + 1;
        if (lvl > max_level) { max_level = lvl; }
        vertex_ty *v = vertices.emplace(to, new vertex_ty(to, lvl)).first.operator*().second;
        add_edge(DJGEdge::d_edge, from, v);
        return v;
    }
    edge_ty *add_j_edge(vertex_ty *from, vertex_ty *to) {
        return add_edge(DJGEdge::j_edge, from, to);
    }
    void dump(std::ostream &out) {
        out << "digraph {\n";
        for (edge_ty *e : edges) {
            out << "\t" << e->from->block->get_name()
                << " -> " << e->to->block->get_name() << " [";
            if (e->kind == DJGEdge::j_edge) {
                out << "style=\"dashed\"";
            }
            else {
                out << "weight=3";
            }
            out << " headlabel=\"" << e->to->level << "\"]";
            out << std::endl;
        }
        out << "}" << std::endl;
    }
};

static DJG make_djg(CFG &cfg) {
    std::set<CFG::vertex_ty *> found;
    std::deque<CFG::vertex_ty *> working;
    CFG::vertex_ty *curr = cfg.get_root();
    found.insert(curr);
    DJG djg(curr->val);
    for (auto e : curr->out_edges) {
        djg.add_vertex(djg.get_root(), e->to->val);
        found.insert(e->to);
        working.push_back(e->to);
    }
    while (!working.empty()) {
        curr = working.front();
        working.pop_front();
        for (auto e : curr->out_edges) {
            if (found.contains(e->to)) {
                // make a j edge
                djg.add_j_edge(djg.get_vertex(e->from->val), djg.get_vertex(e->to->val));
            }
            else {
                // make a d edge
                djg.add_vertex(djg.get_vertex(e->from->val), e->to->val);
                working.push_back(e->to);
                found.insert(e->to);
            }
        }
    }
    return djg;
}

struct PB {
    DJGVertex *v = nullptr;
    PB *next = nullptr;
};

static void pb_insert(PB **pb, PB *x) {
    x->next = pb[x->v->level];
    pb[x->v->level] = x;
}

static PB *pb_get(PB **pb, unsigned &cl) {
    //std::cout << "getting next node...\n";
    if (pb[cl]) {
        PB *x = pb[cl];
        pb[cl] = x->next;
        //std::cout << "  found " << x->v->block->get_name() << " at curr lvl (" << cl << ")\n";
        return x;
    }
    //std::cout << "  nothing found at curr lvl (" << cl << ")\n";
    for (unsigned i = cl - 1; i > 0; i--) {
        //std::cout << "  trying lvl " << i << std::endl;
        if (pb[i]) {
            cl = i;
            PB *x = pb[i];
            pb[i] = x->next;
            //std::cout << "  found " << x->v->block->get_name() << " at lvl (" << i << ")\n";
            return x;
        }
    }
    //std::cout << "  nothing found -- pb is empty\n";
    return nullptr;
}

static void pb_visit(PB **pb, DJGVertex *x, PB *cr, std::set<ir::Block *> &idf) {
    for (auto e : x->out_edges) {
        //std::cout << "  edge " << x->block->get_name() << " -> " << e->to->block->get_name() << std::endl;
        DJGVertex *y = e->to;
        if (e->kind == DJGEdge::j_edge) {
            if (y->level <= cr->v->level) {
                if (y->inphi == false) {
                    y->inphi = true;
                    idf.insert(y->block);
                    if (y->alpha == false) {
                        pb_insert(pb, new PB{y, nullptr});
                    }
                }
            }
        }
        else {
            if (y->visited == false) {
                y->visited = true;
                pb_visit(pb, y, cr, idf);
            }
        }
    }
}

static std::set<ir::Block *> compute_idf(DJG &djg, std::set<DJGVertex *> &alpha) {

    // init
    std::set<ir::Block *> idf;
    PB *pb[djg.max_level + 1];
    memset(pb, 0, sizeof(pb));
    unsigned curr_lvl = djg.max_level;
    for (auto &[b, v] : djg.vertices) {
        v->reset();
    }

    //std::cout << "initialized\n";

    // main
    for (auto &v : alpha) {
        v->alpha = true;
        pb_insert(pb, new PB{v, nullptr});
    }
    //std::cout << "all alpha vertices inserted\n";
    PB *x;
    PB *curr_root;
    while ((x = pb_get(pb, curr_lvl)) != nullptr) {
        curr_root = x;
        x->v->visited = true;
        //std::cout << "visiting " << x->v->block->get_name() << std::endl;
        pb_visit(pb, x->v, curr_root, idf);
        //std::cout << "  done visiting " << x->v->block->get_name() << std::endl;
    }

    return idf;
}

static bool eligible(ir::SAllocInstr *sa, DJG &djg, std::set<DJGVertex *> &alpha) {
    for (ir::Use *u : sa->uses_iterable()) {
        ir::Instr *i = static_cast<ir::Instr *>(u->get_user());
        if (i->get_instr_kind() == ir::instr::write) {
            alpha.insert(djg.get_vertex(i->get_parent()));
        }
        else if (i->get_instr_kind() != ir::instr::read) {
            return false;
        }
    }
    return true;
}

static void stackpromote(ir::SAllocInstr *sa, std::set<ir::Block *> idf, DJG &djg) {

    // bfs structures
    std::deque<DJG::vertex_ty *> working;
    std::set<DJG::vertex_ty *> found;

    // stackpromote structures
    std::map<ir::Block *, ir::Def *> most_recents;

    DJG::vertex_ty *curr = djg.get_root();
    found.insert(curr);
    working.push_back(curr);
    while (!working.empty()) {
        curr = working.front();
        working.pop_front();

        // visit curr
        ir::Block *b = curr->block;
        std::cout << "visiting " << b->get_name() << std::endl;

        // if b is in the iterated dominance frontier
        if (idf.contains(b)) {
            std::cout << "  in idf\n";

            // preds to this block may not be visited yet, which causes this to fail
            // maybe visit b in idf only after all other blocks at the same level as
            // it in the DJG have been visited already
            // ...

            // make phi args
            std::vector<std::pair<ir::Block *, ir::Def *>> phi_args;
            for (auto pred : predecessors(b)) {
                auto mrdef = most_recents[pred];
                std::cout << pred->get_name() << std::endl;
                assert(mrdef && "no most recent def?");
                phi_args.push_back(std::make_pair(pred, mrdef));
            }

            // insert phi node
            auto phi = new ir::PhiInstr(sa->get_alloc_ty(), phi_args, b, sa->get_name());
            most_recents[b] = phi;
        }
        else {
            // propagate the most recent def from idom(b)
            most_recents[b] = most_recents[djg.get_vertex(b)->in_d_edge->from->block];
        }

        // visit each instr in order
        for (auto i : *b) {

            // determine which instrs in b are relevant
            if (std::find_if(sa->uses_begin(), sa->uses_end(),
                [=](ir::Use *const &u) { return i == u->get_user(); }) == sa->uses_end()) {

                // irrelevant instr
                continue;
            }

            // if instr is read
            if (i->get_instr_kind() == ir::instr::read) {

                // get the most recent def of sa for b
                ir::Def *mrdef = most_recents[b];
                assert(mrdef && "no most recent def?");

                // then replace all uses of instr with the def
                for (auto u : i->uses_iterable()) {
                    u->set_def(mrdef);
                }

                // finally, remove instr
                i->remove_from_parent();
            }

            // if instr is write
            else {

                assert(i->get_instr_kind() == ir::instr::write && "instr that uses SAllocInstr which isnt read or write?");

                // remove the instr (the var will be used via the inner
                // def, aka, the one that was being written)
                i->remove_from_parent();
                
                // then update the most recent def for b
                most_recents[b] = static_cast<ir::WriteInstr *>(i)->get_val();
            }
        }

        // add new (not found before) vertices
        for (auto e : curr->out_edges) {
            if (!found.contains(e->to)) {
                found.insert(e->to);
                working.push_back(e->to);
            }
        }
    }

    // std::map<ir::Block *, ir::Def *> last_defs;
    // ir::Def *most_recent_def = nullptr;
    // unsigned counter = 1;
    // std::cout << "stackpromoting " << sa->get_name() << std::endl;
    // ir::Block *sa_parent = sa->get_parent();
    // sa->remove_from_parent();
    // for (auto u : sa->uses_iterable()) {
    //     assert(u && "use is nullptr?");
    //     assert(u->get_user() && "use def is nullptr?");
    //     auto i = static_cast<ir::Instr *>(u->get_user());
    //     std::cout << "  found ";
    //     if (i->get_instr_kind() == ir::instr::read) {

    //         // get the immediate dominator to the parent block of this
    //         // read instr with respect to the given variable (sa)
    //         ir::Block *idom = djg.get_vertex(i->get_parent())->in_d_edge->from->block;

    //         // get the last def from the immediate dominator
    //         // this is the replacement for this read instr
    //         ir::Def *rdef = last_defs.find(idom).operator*().second;
    //         assert(rdef && "no def from immediate dominator?");
    //         auto ri = static_cast<ir::ReadInstr *>(i);
    //         std::cout << "read: " << ri->get_name() << "\n    adding to reads...\n";
    //         reads.push_back(ri);
    //         // replace all uses of this read instr
    //         std::cout << "    replacing all uses with: ";
    //         most_recent_def->dump();
    //         for (auto u : i->uses_iterable()) {
    //             u->get_user()->dump(8);
    //             std::cout << "        -> ";
    //             u->set_def(most_recent_def);
    //             u->get_user()->dump(0);
    //         }
    //         std::cout << "    removing from parent...\n";
    //         i->remove_from_parent();
    //     }
    //     else {
    //         std::cout << "write\n";
    //         assert(i->get_instr_kind() == ir::instr::write);
    //         auto wi = static_cast<ir::WriteInstr *>(i);
    //         auto inner = static_cast<ir::Instr *>(wi->get_operand(0)->get_def());
    //         std::cout << "    removing from parent...\n";
    //         ir::Block *wi_parent = wi->get_parent();
    //         wi->remove_from_parent();
    //         std::cout << "    setting inner's name...\n";
    //         // TODO: fix this stopgap naming once instr names are uniqued across funcs
    //         inner->set_name(sa->get_name() + std::to_string(counter));
    //         counter++;
    //         inner->dump(4);
    //         most_recent_def = inner;
    //     }
    //     std::cout << "    done!" << std::endl;
    // }
    // std::cout << "  stackpromotion finished!" << std::endl;
}

void run_stackpromotion(CFG &cfg) {
    DJG djg = make_djg(cfg);
    std::ofstream djgfile("djg.dot");
    djg.dump(djgfile);
    ir::Block *entry = djg.get_root()->block;
    for (ir::Instr *i : *entry) {
        if (i->get_instr_kind() == ir::instr::salloc) {
            ir::SAllocInstr *sa = static_cast<ir::SAllocInstr *>(i);
            std::set<DJGVertex *> alpha;
            if (eligible(sa, djg, alpha)) {
                std::cout << "alpha for " << ioformat::YELLOW << sa->get_name()
                    << ioformat::RESET << " is { ";
                print_internally_separated_list(
                    alpha.begin(),
                    alpha.end(),
                    ", ",
                    [](DJGVertex *v) { std::cout << v->block->get_name(); }
                );
                std::cout << " }\n";
                std::set<ir::Block *> idf = compute_idf(djg, alpha);
                std::cout << "idf for " << ioformat::YELLOW << sa->get_name()
                    << ioformat::RESET << " is { ";
                print_internally_separated_list(
                    idf.begin(),
                    idf.end(),
                    ", ",
                    [](ir::Block *b) { std::cout << b->get_name(); }
                );
                std::cout << " }\n";
                stackpromote(sa, idf, djg);
                return;
            }
        }
    }
}
