#include "ir/pass.h"
#include <algorithm>
#include <deque>
#include <set>
#include <iostream>
#include <fstream>
#include "utils/ioformat.h"
#include <string.h>

/** ---------------- DJ-graph ---------------- */

// DJ-graphs to implement the IDF algorithm in
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
    std::vector<DJGEdge *> in_j_edges;
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
    vertex_ty *add_vertex(ir::Block *val) {
        vertex_ty *v = vertices.emplace(val, new vertex_ty(val, 0)).first.operator*().second;
        return v;
    }
    edge_ty *add_edge(DJGEdge::djgekind kind, vertex_ty *from, vertex_ty *to) {
        edge_ty *edge = new edge_ty(kind, from, to);
        edges.push_back(edge);
        from->out_edges.push_back(edge);
        if (kind == DJGEdge::d_edge) {
            assert(!to->in_d_edge && "a vertex cannot have multiple incoming d-edges");
            to->in_d_edge = edge;
            to->level = from->level + 1;
            if (max_level < to->level) {
                max_level = to->level;
            }
        }
        else {
            to->in_j_edges.push_back(edge);
        }
        return edge;
    }
    void dump(std::ostream &out) {
        out << "digraph {\n";
        for (edge_ty *e : edges) {

            //std::cout << e->from->block->get_name()
            //    << " -> " << e->to->block->get_name() << "\n";

            out << "\t" << e->from->block->get_name()
                << " -> " << e->to->block->get_name() << " [";
            if (e->kind == DJGEdge::j_edge) {
                out << "constraint=false";
                out << " color=\"black\"";
                out << " penwidth=2";
            }
            else {
                out << "style=\"dashed\"";
                out << " color=\"crimson\"";
            }
            out << " headlabel=\"" << e->to->level << "\"";
            out << "]\n";
        }
        // for (unsigned i = 0; i <= max_level; i++) {
        //     out << "{rank=same;";
        //     for (auto &[b, v] : vertices) {
        //         if (v->level == i) {
        //             out << " " << b->get_name() << ";";
        //         }
        //     }
        //     out << "}\n";
        // }
        out << "}" << std::endl;
    }
};

/** ---------------- Dominator Tree ---------------- */

// dominator tree computation algorithm used from
// https://web.cse.ohio-state.edu/~rountev.1/788/papers/cooper-spe01.pdf

static void calc_po_nums(
    std::vector<ir::Block *> &po2v,
    std::map<ir::Block *, unsigned> &v2po,
    ir::Block *v,
    std::set<ir::Block *> &visited,
    std::vector<unsigned> &order,
    unsigned &i
) {
    assert(!visited.count(v) && "v was already visited?");

    //std::cout << "    checking " << v->get_name() << std::endl;

    auto res = visited.insert(v);
    assert(res.second && "insertion of v failed");
    //std::cout << "      inserted into visited\n";

    // visit successors first
    for (auto succ : successors(v)) {

        //std::cout << "      out-edge " << v->get_name() << " -> " << succ->get_name() << std::endl;

        // only visit if not already visited
        if (!visited.count(succ)) {
            //std::cout << "        visiting!" << std::endl;
            calc_po_nums(po2v, v2po, succ, visited, order, i);
        }
    }

    //std::cout << "      children done\n";
    //std::cout << "      num is " << i << "\n";

    //std::cout << "    visiting " << v->get_name()
    //   << " block@" << v << "\n";

    for (auto v : visited) {
        //std::cout << "      alr visited " << v->get_name() << "\n";
    }

    // visit v
    po2v[i] = v;
    //std::cout << "      added to po2v\n";
    v2po[v] = i;
    //std::cout << "      added to v2po\n";
    order.push_back(i);
    //std::cout << "      added to order\n";
    i++;
    //std::cout << "      incr'd i\n";
}

static unsigned intersect(unsigned *doms, unsigned b1, unsigned b2) {
    auto f1 = b1, f2 = b2;
    //std::cout << "    intersecting " << b1 << " and " << b2 << std::endl
    //    << "      using doms=[";
    for (unsigned i = 1; i < 10; i++) {
        //std::cout << " " << doms[i];
    }
    //std::cout << " ]\n";
    while (f1 != f2) {
        if (f1 < f2) {
            f1 = doms[f1];
        }
        if (f2 < f1) {
            f2 = doms[f2];
        }
        //std::cout << "      f1=" << f1 << ", f2=" << f2 << std::endl;
    }
    return f1;
}

static DJG make_djg(ir::Function *f) {

    //std::cout << "making djg\n";

    //std::cout << "  calculating postorder nums\n";

    // calculate postorder numbers
    std::vector<ir::Block *> po2v(f->size() + 1);
    std::map<ir::Block *, unsigned> v2po;
    std::set<ir::Block *> visited;
    std::vector<unsigned> postorder;
    unsigned num = 1;
    calc_po_nums(po2v, v2po, f->get_first_block(), visited, postorder, num);
    assert(num - 1 == f->size() && "number of vertices visited does not match the number of cfg vertices");
    unsigned start_node = num - 1;

    //std::cout << "  calculated postorder nums\n";
    for (unsigned i = start_node; i > 0; i--) {
        //std::cout << "    " << i << " " << po2v[i]->get_name() << std::endl;
    }

    // initialize the dominators array
    unsigned doms[num];
    memset(doms, 0, sizeof(doms));

    //std::cout << "  init'd doms array\n";

    doms[start_node] = start_node;

    bool changed = true;
    while (changed) {
        changed = false;

        //std::cout << "  new iteration:\n";

        assert(start_node > 0);
        // iterate in reverse postorder (skipping start_node)
        for (unsigned b = start_node - 1; b > 0; b--) {
            auto block = po2v[b];
            //std::cout << "    " << block->get_name() << std::endl;
            auto preds = predecessors(block);
            unsigned new_idom = 0;
            ir::Block *chosen_pred = nullptr;
            for (auto pred : preds) {
                //std::cout << "      " << pred->get_name() << std::endl;
                auto tmp = v2po[pred];
                if (doms[tmp] != 0) {
                    chosen_pred = pred;
                    new_idom = tmp;
                    //std::cout << "        chosen" << std::endl;
                    break;
                }
            }
            //std::cout << "    new_idom=" << new_idom << "\n";
            assert(chosen_pred && "no pred that is processed already?");

            // iterate backwards over the preds, skipping new_idom
            for (auto pred : preds) {
                if (pred != chosen_pred) {
                    auto p = v2po[pred];
                    if (doms[p]) {
                        new_idom = intersect(doms, p, new_idom);
                    }
                }
            }

            if (doms[b] != new_idom) {
                doms[b] = new_idom;
                changed = true;
            }
        }
    }

    //std::cout << "  final doms:\n";
    for (unsigned i = start_node - 1; i > 0; i--) {
        //std::cout << "    idom(" << po2v[i]->get_name() << ") = " << po2v[doms[i]]->get_name() << "\n";
    }

    //std::cout << "  making djg\n";

    DJG djg(f->get_first_block());

    // add d-edges
    for (unsigned i = start_node - 1; i > 0; i--) {
        auto v = djg.add_vertex(po2v[i]);
        auto idomv = djg.get_vertex(po2v[doms[i]]);
        //std::cout << "    d-edge: " << po2v[doms[i]]->get_name() << " -> " << v->block->get_name() << std::endl;
        djg.add_edge(DJGEdge::d_edge, idomv, v);
    }

    // add j-edges
    for (unsigned i = start_node - 1; i > 0; i--) {
        auto v = po2v[i];
        auto idom_v = po2v[doms[i]];
        for (auto pred : predecessors(v)) {
            if (pred != idom_v) {
                djg.add_edge(DJGEdge::j_edge, djg.get_vertex(pred), djg.get_vertex(v));
            }
        }
    }

    //std::cout << "    done!\n";

    // std::ofstream out("djg.dot");
    // djg.dump(out);
    // out.flush();
    // out.close();

    return djg;
}

/** ---------------- Iterated Dominance Frontier ---------------- */

// iterated dominance frontier calculation algorithm used from
// https://dl.acm.org/doi/pdf/10.1145/199448.199464

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
    // if (cl == 0) {
    //     return nullptr;
    // }

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
    //std::cout << "  visiting " << x->block->get_name() << std::endl;
    for (auto e : x->out_edges) {
        //std::cout << "  edge " << x->block->get_name() << " -> " << e->to->block->get_name() << std::endl;
        DJGVertex *y = e->to;
        if (e->kind == DJGEdge::j_edge) {
            //std::cout << "    j-edge found\n";
            if (y->level <= cr->v->level) {
                if (y->inphi == false) {
                    y->inphi = true;
                    idf.insert(y->block);
                    if (y->alpha == false) {
                        pb_insert(pb, new PB{y, nullptr});
                    }
                    //std::cout << "    inserted " << y->block->get_name()
                    //    << " into idf" << std::endl;
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
    //std::cout << "    done\n";
}

static std::set<ir::Block *> compute_idf(DJG &djg, std::set<DJGVertex *> &alpha) {
    //std::cout << "computing idf\n";

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
        //std::cout << "  new block " << x->v->block->get_name() << std::endl;
        curr_root = x;
        x->v->visited = true;
        pb_visit(pb, x->v, curr_root, idf);
    }

    //std::cout << "  done\n";

    return idf;
}

static bool eligible(ir::SAllocInstr *sa, DJG &djg, std::set<DJGVertex *> &alpha) {
    //std::cout << "eligibility of " << sa->get_name() << ": ";
    for (ir::Use *u : sa->uses_iterable()) {
        auto d = u->get_user();
        //std::cout << "  user@" << d << std::endl;
        if (d->get_kind() == ir::defkind::write) {
            alpha.insert(djg.get_vertex(static_cast<ir::Instr *>(d)->get_parent()));
        }
        else if (d->get_kind() != ir::defkind::read) {
            //std::cout << "ineligible :(\n";
            return false;
        }
    }
    //std::cout << "eligible :)\n";
    return true;
}

static void stackpromote(ir::SAllocInstr *sa, std::set<ir::Block *> idf, DJG &djg) {

    //std::cout << "starting stackpromote for " << sa->get_name() << std::endl;

    // naming stopgap until uniqued names are fixed
    // unsigned counter = 1;

    // bfs structures
    std::deque<DJG::vertex_ty *> working;
    std::set<DJG::vertex_ty *> found;

    // stackpromote structures
    std::map<ir::Block *, ir::Def *> most_recents;
    std::deque<std::pair<ir::PhiInstr *, std::vector<ir::Block *>>> unfinished_phis;

    DJG::vertex_ty *curr = djg.get_root();
    found.insert(curr);
    working.push_back(curr);
    while (!working.empty()) {
        curr = working.front();
        working.pop_front();

        // visit curr
        ir::Block *b = curr->block;
        //std::cout << "  visiting " << b->get_name() << std::endl;

        // if b is in the iterated dominance frontier
        if (idf.contains(b)) {
            //std::cout << "    idf" << std::endl;

            // insert an empty phi node
            auto phi = new ir::PhiInstr(sa->get_alloc_ty(), b, sa->get_name() /* + std::to_string(counter++) */);
            //std::cout << "    inserted phi" << std::endl;

            // use the phi as this block's most recent def
            most_recents[b] = phi;
            //std::cout << "    updated most recent" << std::endl;

            std::vector<ir::Block *> needs;
            for (auto pred : predecessors(b)) {
                auto rec = most_recents[pred];
                if (rec) {
                    //std::cout << "    adding alternative from " << pred->get_name() << std::endl;
                    phi->add_alternatives(pred, rec);
                    //std::cout << "      done" << std::endl;
                }
                else {
                    //std::cout << "    adding def from " << pred->get_name() << " as a need" << std::endl;
                    needs.push_back(pred);
                    //std::cout << "      done" << std::endl;
                }
            }

            if (needs.size() > 0) {
                unfinished_phis.push_back(std::make_pair(phi, needs));
            }
        }

        // b is not in idf
        else {

            // propagate the most recent def from idom(b)
            most_recents[b] = most_recents[curr->in_d_edge->from->block];
            //std::cout << "    propagated ";
            if (most_recents[b]) {
                //most_recents[b]->dump_as_operand();
                //std::cout << " <" << most_recents[b] << ">";
            }
            else {
                //std::cout << "null";
            }
            //std::cout << " from " << curr->in_d_edge->from->block->get_name() << std::endl;
        }

        //std::cout << "    rectifying reads/writes" << std::endl;

        // visit each instr in order
        for (auto d : *b) {

            // determine which instrs in b are relevant
            if (std::find_if(sa->uses_begin(), sa->uses_end(),
                [=](ir::Use *const &u) { return d == u->get_user(); }) == sa->uses_end()) {
                // irrelevant instr
                continue;
            }

            // if instr is read
            if (d->get_kind() == ir::defkind::read) {
                ir::Instr *i = static_cast<ir::Instr *>(d);

                // get the most recent def of sa for b
                ir::Def *mrdef = most_recents[b];
                assert(mrdef && "stack var used before defined");

                //std::cout << "    mrdef to replace " << i->get_name() << " : ";
                //mrdef->dump();

                // then replace all uses of instr with the def
                for (auto u : i->uses_iterable()) {
                    u->set_def(mrdef);
                }

                // finally, remove instr
                i->remove_from_parent();
            }

            // if instr is write
            else {
                ir::Instr *i = static_cast<ir::Instr *>(d);

                assert(i->get_kind() == ir::defkind::write && "instr that uses SAllocInstr which isnt read or write?");
                
                // remove the instr (the var will be used via the inner
                // def, aka, the one that was being written)
                i->remove_from_parent();
                
                // rename the inner def
                ir::Def *inner = static_cast<ir::WriteInstr *>(i)->get_val();
                
                if (inner->is_instr()) {
                    static_cast<ir::Instr *>(inner)->set_name(sa->get_name() /* + std::to_string(counter++) */);
                }

                // then update the most recent def for b
                most_recents[b] = inner;
            }
        }

        //std::cout << "    most_recent = ";
        //most_recents[b]->dump_as_operand();
        //std::cout << "<" << most_recents[b] << ">" << std::endl;

        // add new (not found before) vertices
        for (auto e : curr->out_edges) {
            if (!found.contains(e->to)) {
                found.insert(e->to);
                working.push_back(e->to);
            }
        }
    }

    for (auto &[phi, needs] : unfinished_phis) {
        for (auto need : needs) {
            auto mrdef = most_recents[need];
            assert(mrdef);
            phi->add_alternatives(need, mrdef);
        }
    }

    // finally, remove the salloc instr
    sa->remove_from_parent();

    //std::cout << "  done (stackpromote)\n";
}

static void run_stackpromotion(ir::Function *f) {
    DJG djg = make_djg(f);
    std::ofstream djgfile("djg.dot");
    djg.dump(djgfile);
    ir::Block *entry = djg.get_root()->block;
    for (ir::Instr *i : *entry) {
        if (i->get_kind() == ir::defkind::salloc) {
            ir::SAllocInstr *sa = static_cast<ir::SAllocInstr *>(i);
            std::set<DJGVertex *> alpha;
            //std::cout << "determining eligibility\n";
            bool el = eligible(sa, djg, alpha);
            //std::cout << "  done\n";
            if (el) {
                //std::cout << "alpha for " << ioformat::YELLOW << sa->get_name()
                //    << ioformat::RESET << " is { ";
                // print_internally_separated_list(
                //     alpha.begin(),
                //     alpha.end(),
                //     ", ",
                //     [](DJGVertex *v) { std::cout << v->block->get_name(); }
                // );
                //std::cout << " }\n";
                std::set<ir::Block *> idf = compute_idf(djg, alpha);
                //std::cout << "idf for " << ioformat::YELLOW << sa->get_name()
                //    << ioformat::RESET << " is { ";
                // print_internally_separated_list(
                //     idf.begin(),
                //     idf.end(),
                //     ", ",
                //     [](ir::Block *b) { std::cout << b->get_name(); }
                // );
                //std::cout << " }\n";
                //continue;
                stackpromote(sa, idf, djg);
                //return;
            }
        }
    }
}

void run_stackpromotion(ir::Program *p) {
    for (auto f : *p) {
        run_stackpromotion(f);
    }
}
