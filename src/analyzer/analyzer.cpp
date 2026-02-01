#include <algorithm>
#include <cassert>
#include <iostream>
#include <unordered_map>
#include <vector>

#include "ast/scope.h"
#include "ast/visitor.h"
#include "diag/diagnostic.h"
#include "utils/identifier.h"
#include "utils/memory.h"

#include "analyzer/analyzer.h"

using namespace fe;

using xast::Identifier;

xast::Node *find_symbol_in_current_scope(
    Identifier ident,
    Scope *scope
) {
    auto iter = scope->symbols.find(*ident),
         end = scope->symbols.end();

    if (iter == end) {
        return nullptr;
    }

    return iter->second;
}

Type *find_type_in_current_scope(
    Identifier ident,
    Scope *scope
) {
    auto iter = scope->types.find(*ident),
         end = scope->types.end();

    if (iter == end) {
        return nullptr;
    }

    return iter->second;
}

xast::Node *find_symbol_in_any_active_scope(
    Identifier ident,
    Scope *scope
) {

    Scope *curr = scope;
    while (curr) {
        auto ptr = find_symbol_in_current_scope(ident, curr);
        if (ptr) return ptr;

        curr = curr->parent;
    }

    // not found
    return nullptr;
}

Type *find_type_in_any_active_scope(
    Identifier ident,
    Scope *scope
) {

    Scope *curr = scope;
    while (curr) {
        auto ptr = find_type_in_current_scope(ident, curr);
        if (ptr) return ptr;

        curr = curr->parent;
    }

    // not found
    return nullptr;
}

Type *typify(xast::Node *n, Scope *s) {
    Type *ty = nullptr;
    switch (n->kind) {
    case xast::nk::struct_: {
        ty = new StructType(n);
        break;
    }
    case xast::nk::union_: {
        ty = new UnionType(n);
        break;
    }
    case xast::nk::te_name: {
        assert(n->ident);
        ty = find_type_in_any_active_scope(n->ident, s);
        if (!ty) {
            DiagnosticHandler::make(diag::id::use_of_undeclared_type, n->sloc)
                .add(std::string((*n->ident)))
                .finish();
            ty = ErrorType::get();
        }
        break;
    }
    case xast::nk::tmplinstantiation: {
        assert(n->opedges.size());
        auto tmpl = n->opedges.back()->used();
        assert(tmpl->kind == xast::nk::te_name && tmpl->ident);
        auto tmplty = find_type_in_any_active_scope(tmpl->ident, s);
        if (!tmplty) {
            DiagnosticHandler::make(diag::id::use_of_undeclared_type, n->sloc)
                .add(std::string((*n->ident)))
                .finish();
            ty = ErrorType::get();
        }
        else if (tmplty->get_kind() != typekind::templated_t) {
            DiagnosticHandler::make(diag::id::type_is_not_templated, tmpl->sloc)
                .add(tmplty->stringify())
                .finish();
            ty = ErrorType::get();
        }
        else {
            assert(tmplty && tmplty->get_kind() == typekind::templated_t);
            ty = new InstantiatedType((TemplatedType *)tmplty);
        }
        break;
    }
    default: assert(false && "invalid type expr");
    }

    assert(ty);
    return ty;
}

struct IndexingPass : xast::Visitor<void, Scope *, Identifier> {

    void setup_tmpl(xast::Node *tmpl, Scope *s) {
        for (int i = 0; i < xast::c::tmpldecl::param_count(tmpl); ++i) {
            auto param = xast::c::tmpldecl::param(tmpl, i);
            switch (param->kind) {
            case xast::nk::tmplparamdecl: {
                // type param
                assert(param->ident);
                auto otype = find_type_in_current_scope(param->ident, s);
                if (otype) {
                    assert(otype->is_decl());
                    DiagnosticHandler::make(diag::id::type_redeclaration, param->ident.sloc)
                        .add(std::string((*param->ident)))
                        .finish();
                    DiagnosticHandler::make(diag::id::note_original_declaration,
                            ((DeclType *)otype)->decl->sloc)
                        .finish();
                }
                else {
                    s->types[std::string((*param->ident))] = new PlaceholderType(param);
                }
                break;
            }
            case xast::nk::param:
                // value param
                assert(param->ident);
                s->symbols[std::string((*param->ident))] = param;
                break;
            default: assert(false && "non-type, non-value tmpl param?");
            }
        }
    }

    void visit_tmpldecl(xast::Node *node, Scope *s, Identifier parent_name) override {
        assert(node->kind == xast::nk::tmpldecl && node->ident);
        
        auto decl_node = xast::c::tmpldecl::decl(node);
        assert(
            decl_node
              && ( decl_node->kind == xast::nk::typebind
                || decl_node->kind == xast::nk::valbind
                || decl_node->kind == xast::nk::funcbind
            )
        );
        
        // create a new scope for template parameters
        auto tmpl_scope = new Scope;
        tmpl_scope->parent = s;
        node->scope = tmpl_scope;
        
        setup_tmpl(node, tmpl_scope);
        
        if (decl_node->kind == xast::nk::typebind) {
            
            // Check for redeclaration in parent scope
            auto otype = find_type_in_current_scope(node->ident, s);
            if (otype) {
                assert(otype->is_decl());
                DiagnosticHandler::make(diag::id::type_redeclaration, node->ident.sloc)
                    .add(std::string((*node->ident)))
                    .finish();
                DiagnosticHandler::make(diag::id::note_original_declaration,
                        ((DeclType *)otype)->decl->sloc)
                    .finish();
            }
            else {
                // Register as templated type in parent scope
                s->types[std::string((*node->ident))] = new TemplatedType(node);
            }
        }
        else {
            auto osym = find_symbol_in_current_scope(node->ident, s);
            if (osym) {
                DiagnosticHandler::make(diag::id::symbol_redeclaration, node->ident.sloc)
                    .add(std::string((*node->ident)))
                    .finish();
                DiagnosticHandler::make(diag::id::note_original_declaration, osym->ident.sloc)
                    .finish();
            }
            else {
                s->symbols[std::string((*node->ident))] = node;
            }
        }

        decl_node->scope = tmpl_scope;

        for (auto e : node->opedges) {
            dispatch(e->used(), tmpl_scope, node->ident);
        }
    }

    void visit_valbind(xast::Node *node, Scope *s, Identifier parent_name) override {

        if (node->ident) {
            auto osym = find_symbol_in_current_scope(node->ident, s);
            if (osym) {
                DiagnosticHandler::make(diag::id::symbol_redeclaration, node->ident.sloc)
                    .add(std::string((*node->ident)))
                    .finish();
                DiagnosticHandler::make(diag::id::note_original_declaration, osym->ident.sloc)
                    .finish();
            }
            else {
                s->symbols[std::string((*node->ident))] = node;
            }
        }
        // else, it is templated

        for (auto e : node->opedges) {
            dispatch(e->used(), s, parent_name);
        }
    }

    void visit_funcbind(xast::Node *node, Scope *s, Identifier parent_name) override {
        
        auto newscope = s;

        if (node->ident) {
            auto osym = find_symbol_in_current_scope(node->ident, s);
            if (osym) {
                DiagnosticHandler::make(diag::id::symbol_redeclaration, node->ident.sloc)
                    .finish();
                DiagnosticHandler::make(diag::id::note_original_declaration, osym->ident.sloc)
                    .finish();
            }
            else {
                s->symbols[std::string((*node->ident))] = node;
            }

            newscope = new Scope;
            newscope->parent = s;
            node->scope = newscope;
            parent_name = node->ident;
        }
        // else, this node is templated

        for (auto e : node->opedges) {
            dispatch(e->used(), newscope, parent_name);
        }
    }

    void visit_func(xast::Node *node, Scope *s, Identifier parent_name) override {
        
        auto newscope = s;

        newscope = new Scope;
        newscope->parent = s;
        node->scope = newscope;
        parent_name = node->ident;

        for (auto e : node->opedges) {
            dispatch(e->used(), newscope, parent_name);
        }
    }

    void visit_typebind(xast::Node *node, Scope *s, Identifier parent_name) override {
        
        auto newscope = s;
        
        if(node->ident) {

            auto otype = find_type_in_current_scope(node->ident, s);
            if (otype) {
                assert(otype->is_decl());
                DiagnosticHandler::make(diag::id::type_redeclaration, node->ident.sloc)
                    .add(std::string((*node->ident)))
                    .finish();
                DiagnosticHandler::make(diag::id::note_original_declaration,
                        ((DeclType *)otype)->decl->sloc)
                    .finish();
            }
            else {
                // Non-templated case - typify the definition
                assert(node->opedges.size() >= 1);
                auto def = node->opedges[0]->used();
                Type *ty = typify(def, s);
                s->types[std::string((*node->ident))] = ty;
            }

            newscope = new Scope;
            newscope->parent = s;
            node->scope = newscope;
            parent_name = node->ident;
        }
        // else, its templated!

        for (auto e : node->opedges) {
            dispatch(e->used(), newscope, parent_name);
        }
    }

    void visit_param(xast::Node *node, Scope *s, Identifier parent_name) override {
        assert(node->ident);
        auto osym = find_symbol_in_current_scope(node->ident, s);
        if (osym) {
            DiagnosticHandler::make(diag::id::symbol_redeclaration, node->ident.sloc)
                .add(std::string((*node->ident)))
                .finish();
            DiagnosticHandler::make(diag::id::note_original_declaration, osym->ident.sloc)
                .finish();
        }
        else {
            s->symbols[std::string((*node->ident))] = node;
        }

        for (auto e : node->opedges) {
            dispatch(e->used(), s, parent_name);
        }
    }
};

namespace fe {

void analyze(xast::Node *root, std::vector<fe::PrimitiveType *> const &primitives) {

    assert(root && root->kind == xast::nk::prog);

    // create global scope
    auto gscope = new Scope;
    for (auto ty : primitives) {
        std::cout << ty->stringify() << "\n";
        gscope->types[ty->stringify()] = ty;
    }
    root->scope = gscope;
    gscope->dump();

    // scanning pass
    std::cout << "indexing\n";
    IndexingPass()(root, gscope, Identifier{});

}

} // namespace fe
