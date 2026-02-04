#include <algorithm>
#include <cassert>
#include <iostream>
#include <memory>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <stack>
#include <sstream>

#include "ast/op.h"
#include "ast/scope.h"
#include "ast/visitor.h"
#include "diag/diagnostic.h"
#include "utils/identifier.h"
#include "utils/memory.h"

#include "analyzer/analyzer.h"

namespace fe {

xast::Node *find_symbol(Scope *s, std::string_view name) {
    auto it = s->symbols.find(name);
    if (it != s->symbols.end()) {
        return it->second;
    }
    return nullptr;
}

xast::Node *find_symbol_in_any(Scope *s, std::string_view name) {
    while (s != nullptr) {
        auto it = s->symbols.find(name);
        if (it != s->symbols.end()) {
            return it->second;
        }
        s = s->parent;
    }
    return nullptr;
}

bool maybe_insert_symbol(Scope *s, xast::Node *decl) {
    assert(decl->data.is_ident());
    Identifier ident = decl->data;

    // TODO: check for shadowing
    if (auto orig = find_symbol(s, *ident)) {
        DiagnosticHandler::make(diag::id::symbol_redeclaration, decl->data.ident.sloc)
            .add(*ident)
            .finish();
        DiagnosticHandler::make(diag::id::note_original_declaration, orig->data.ident.sloc)
            .finish();
        return false;
    }

    s->symbols[std::string(*ident)] = decl;
    return true;
}

struct IndexingPass : xast::Visitor<void, Scope *> {
    void visit_bind(xast::Node *node, Scope *scope) override {
        assert(node->data.is_ident());
        Identifier ident = node->data;
        
        // If the binding's definition is a meta node (like a composite type),
        // propagate the meta flag to the binding
        xast::Node *def = xast::c::bind::def(node);
        if (!node->meta && def && def->meta && def->kind == xast::nk::composite) {
            node->meta = true;
        }
        
        if (maybe_insert_symbol(scope, node)) {
            Scope *bind_scope = new Scope;
            bind_scope->parent = scope;
            node->scope = bind_scope;
            for (auto edge : node->opedges) {
                dispatch(edge->used(), bind_scope);
            }
        }
    }
    void visit_field(xast::Node *node, Scope *scope) override {
        assert(node->data.is_ident());
        Identifier ident = node->data;
        if (maybe_insert_symbol(scope, node)) {
            for (auto edge : node->opedges) {
                dispatch(edge->used(), scope);
            }
        }
    }
    void visit_variant(xast::Node *node, Scope *scope) override {
        assert(node->data.is_ident());
        Identifier ident = node->data;
        if (maybe_insert_symbol(scope, node)) {
            for (auto edge : node->opedges) {
                dispatch(edge->used(), scope);
            }
        }
    }
    void visit_param(xast::Node *node, Scope *scope) override {
        assert(node->data.is_ident());
        Identifier ident = node->data;
        if (maybe_insert_symbol(scope, node)) {
            for (auto edge : node->opedges) {
                dispatch(edge->used(), scope);
            }
        }
    }
    void visit_func(xast::Node *node, Scope *scope) override {
        // create new scope for function body
        Scope *func_scope = new Scope;
        func_scope->parent = scope;
        node->scope = func_scope;

        for (auto edge : node->opedges) {
            dispatch(edge->used(), func_scope);
        }
    }
    void visit_composite(xast::Node *node, Scope *scope) override {
        // Create a new scope for the composite type
        // This scope will contain the temp name if specified
        Scope *composite_scope = new Scope;
        composite_scope->parent = scope;
        node->scope = composite_scope;

        // Check for optional temp name (e.g., "type as Self { ... }")
        xast::Node *tmpname = xast::c::composite::tmpname(node);
        if (tmpname && tmpname->data.is_ident()) {
            Identifier ident = tmpname->data;
            // Register the temp name as referring to this composite node
            composite_scope->symbols[std::string(*ident)] = node;
        }

        // Traverse layout (struct_ or union_) with the composite's scope
        dispatch(xast::c::composite::layout(node), composite_scope);

        // Traverse namespace (where clause) with the composite's scope
        dispatch(xast::c::composite::namespc(node), composite_scope);
    }
    void visit_branch(xast::Node *node, Scope *scope) override {
        // For meta branches (if!), skip indexing the branches entirely
        // They will be simplified away and re-indexed after simplification
        if (node->meta) {
            // Only index the condition (for symbol references)
            dispatch(xast::c::branch::cond(node), scope);
            // Skip indexing then/else branches - they may define conflicting symbols
            // that will be resolved after meta evaluation
            return;
        }
        
        // For regular branches, create scopes and index normally
        xast::Node *then_node = xast::c::branch::then(node);
        if (then_node) {
            Scope *then_scope = new Scope;
            then_scope->parent = scope;
            then_node->scope = then_scope;  // Set scope on the block
            dispatch(then_node, then_scope);
        }
        
        xast::Node *else_node = xast::c::branch::else_(node);
        if (else_node) {
            Scope *else_scope = new Scope;
            else_scope->parent = scope;
            else_node->scope = else_scope;  // Set scope on the block
            dispatch(else_node, else_scope);
        }
    }
    void visit_loop(xast::Node *node, Scope *scope) override {
        // For meta loops (while!), skip indexing the body
        if (node->meta) {
            dispatch(xast::c::loop::cond(node), scope);
            return;
        }
        
        // For regular loops, index with a new scope
        xast::Node *body = xast::c::loop::body(node);
        Scope *loop_scope = new Scope;
        loop_scope->parent = scope;
        body->scope = loop_scope;  // Set scope on the body block
        dispatch(xast::c::loop::cond(node), loop_scope);
        dispatch(body, loop_scope);
    }
    void visit_int_lit(xast::Node *node, Scope *scope) override {
        node->meta = true;
    }
    void visit_str_lit(xast::Node *node, Scope *scope) override {
        node->meta = true;
    }
    void visit_char_lit(xast::Node *node, Scope *scope) override {
        node->meta = true;
    }
};

// ============================================================================
// Expression Universe Validator
// ============================================================================
// Validates that expressions are valid in their context (type or value universe).
// 
// Type-only operators:
//   - op::arrow (->)  - function type constructor (A -> B)
//
// Value-only operators:
//   - Arithmetic: add, sub, mult, div, mod, neg
//   - Relational: gt, lt, gte, lte, eq, neq
//   - Logical: land, lor, lnot
//   - Side-effecting: preincr, postincr, predecr, postdecr
//   - Assignment: assign
//   - Dot (for field access on values): dot
//
// Operators valid in both:
//   - op::indirect (*) - pointer dereference (value) / pointer type (type)
//   - op::slice ([]) - array subscript (value) / slice type (type)
//   - op::addr (&) - address-of (value) / reference type (type) -- if supported
//   - op::group (parentheses)
//
// Meta expressions (if!, while!, call! to meta fn) are valid in both universes
// as long as their result matches the expected universe.
//
// The validator propagates universe context downward and checks operators.

enum class ExprUniverse {
    Unknown,    // Not yet determined (e.g., independent meta expressions)
    Value,      // Runtime value expression
    Type,       // Compile-time type expression
    Either      // Can be either (e.g., parentheses, some meta expressions)
};

struct ExpressionUniverseValidator : xast::Visitor<ExprUniverse, ExprUniverse, Scope *> {
    // Cache of node -> detected universe (for meta expressions that compute types/values)
    std::unordered_map<xast::Node*, ExprUniverse> computed_universe;
    
    // Get operator name for diagnostics
    std::string get_op_name(op::kind k) {
        switch (k) {
            case op::arrow: return "->";
            case op::add: return "+";
            case op::sub: return "-";
            case op::mult: return "*";
            case op::div: return "/";
            case op::mod: return "%";
            case op::neg: return "-";
            case op::gt: return ">";
            case op::lt: return "<";
            case op::gte: return ">=";
            case op::lte: return "<=";
            case op::eq: return "==";
            case op::neq: return "!=";
            case op::land: return "&&";
            case op::lor: return "||";
            case op::lnot: return "!";
            case op::preincr: case op::postincr: return "++";
            case op::predecr: case op::postdecr: return "--";
            case op::assign: return "=";
            case op::dot: return ".";
            case op::indirect: return "*";
            case op::slice: return "[]";
            case op::addr: return "&";
            default: return "<unknown>";
        }
    }
    
    std::string universe_name(ExprUniverse u) {
        switch (u) {
            case ExprUniverse::Value: return "value";
            case ExprUniverse::Type: return "type";
            case ExprUniverse::Either: return "either";
            default: return "unknown";
        }
    }
    
    // Check if an operator is type-only
    bool is_type_only_op(op::kind k) {
        return k == op::arrow;
    }
    
    // Check if an operator is value-only
    bool is_value_only_op(op::kind k) {
        return op::is_val_op(k) ||     // Arithmetic
               op::is_rel_op(k) ||     // Relational
               op::is_log_op(k) ||     // Logical
               op::is_lval_op(k);      // Lvalue ops (incr, decr, assign)
        // Note: dot is NOT value-only because it can be used for namespace access (Type.member)
    }
    
    // Binary operations
    ExprUniverse visit_binary_op(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        assert(node->data.is_op());
        Op op = node->data;
        
        // Check operator validity for the context
        if (expected == ExprUniverse::Type && is_value_only_op(op.kind)) {
            DiagnosticHandler::make(diag::id::value_only_operator_in_type_context, node->sloc)
                .add(get_op_name(op.kind))
                .finish();
            return ExprUniverse::Value; // Return what it actually is
        }
        
        if (expected == ExprUniverse::Value && is_type_only_op(op.kind)) {
            DiagnosticHandler::make(diag::id::type_only_operator_in_value_context, node->sloc)
                .add(get_op_name(op.kind))
                .finish();
            return ExprUniverse::Type; // Return what it actually is
        }
        
        // Determine the required context for children based on operator
        ExprUniverse child_context = expected;
        if (op.kind == op::arrow) {
            // Arrow is type-only: (ParamTypes) -> ReturnType
            child_context = ExprUniverse::Type;
        } else if (op.kind == op::dot) {
            // Dot is polymorphic: LHS can be type (for namespace access) or value (for field access)
            // Let LHS be either, and RHS depends on LHS
            dispatch(xast::c::binary_op::lhs(node), ExprUniverse::Either, scope);
            dispatch(xast::c::binary_op::rhs(node), ExprUniverse::Either, scope);
            // Result universe depends on what we're accessing
            return expected;
        }
        
        // Validate children
        dispatch(xast::c::binary_op::lhs(node), child_context, scope);
        dispatch(xast::c::binary_op::rhs(node), child_context, scope);
        
        // Determine result universe
        if (is_type_only_op(op.kind)) return ExprUniverse::Type;
        if (is_value_only_op(op.kind)) return ExprUniverse::Value;
        return expected;
    }
    
    // Unary operations
    ExprUniverse visit_unary_op(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        assert(node->data.is_op());
        Op op = node->data;
        
        // Check operator validity
        if (expected == ExprUniverse::Type && is_value_only_op(op.kind)) {
            DiagnosticHandler::make(diag::id::value_only_operator_in_type_context, node->sloc)
                .add(get_op_name(op.kind))
                .finish();
            return ExprUniverse::Value;
        }
        
        if (expected == ExprUniverse::Value && is_type_only_op(op.kind)) {
            DiagnosticHandler::make(diag::id::type_only_operator_in_value_context, node->sloc)
                .add(get_op_name(op.kind))
                .finish();
            return ExprUniverse::Type;
        }
        
        // indirect and slice are valid in both contexts
        // In type context: *T means pointer-to-T, []T means slice-of-T
        // In value context: *p means dereference, arr[i] means subscript
        
        dispatch(xast::c::unary_op::operand(node), expected, scope);
        
        // The result universe is the same as expected for polymorphic ops
        if (op.kind == op::indirect || op.kind == op::slice) {
            return expected;
        }
        
        if (is_type_only_op(op.kind)) return ExprUniverse::Type;
        if (is_value_only_op(op.kind)) return ExprUniverse::Value;
        return expected;
    }
    
    // References - check if the symbol matches the expected universe
    ExprUniverse visit_ref(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        if (!node->data.is_ident()) return expected;
        
        Identifier ident = node->data;
        xast::Node *decl = find_symbol_in_any(scope, *ident);
        
        if (!decl) {
            // Symbol not found - will be caught by later passes
            return expected;
        }
        
        // Check if this is a type binding (has a type that is MetaType or is a primitive type binding)
        bool is_type_binding = false;
        
        // Check for primitive type references (i8, i16, i32, i64, u8, u16, u32, u64, f32, f64)
        std::string_view name = *ident;
        if (name == "i8" || name == "i16" || name == "i32" || name == "i64" ||
            name == "u8" || name == "u16" || name == "u32" || name == "u64" ||
            name == "f32" || name == "f64" || name == "type") {
            is_type_binding = true;
        }
        
        // Check if it's a parameter with a type annotation of 'type'
        // (This makes T a type-valued parameter in fn!(T: type))
        if (decl->kind == xast::nk::param) {
            xast::Node *type_annot = xast::c::param::type_annot(decl);
            if (type_annot && type_annot->kind == xast::nk::ref && type_annot->data.is_ident()) {
                if (*type_annot->data.ident == "type") {
                    is_type_binding = true;
                }
            }
        }
        
        // Check if it's a let! binding that defines a type (type constructor or type alias)
        if (decl->kind == xast::nk::bind && decl->meta) {
            xast::Node *def = xast::c::bind::def(decl);
            if (def) {
                // If definition is a composite, it's a type
                if (def->kind == xast::nk::composite) {
                    is_type_binding = true;
                }
                // If definition is a fn! returning type, it's a type constructor
                if (def->kind == xast::nk::func && def->meta) {
                    xast::Node *return_ty = xast::c::func::return_ty(def);
                    if (return_ty && return_ty->kind == xast::nk::ref && return_ty->data.is_ident()) {
                        if (*return_ty->data.ident == "type") {
                            is_type_binding = true;
                        }
                    }
                }
            }
        }
        
        // Validate universe match
        if (is_type_binding && expected == ExprUniverse::Value) {
            DiagnosticHandler::make(diag::id::type_used_in_value_context, node->sloc)
                .add(*ident)
                .finish();
            return ExprUniverse::Type;
        }
        
        if (!is_type_binding && expected == ExprUniverse::Type && decl->kind == xast::nk::bind && !decl->meta) {
            DiagnosticHandler::make(diag::id::value_used_in_type_context, node->sloc)
                .add(*ident)
                .finish();
            return ExprUniverse::Value;
        }
        
        return is_type_binding ? ExprUniverse::Type : ExprUniverse::Value;
    }
    
    // Literals - always value universe
    ExprUniverse visit_int_lit(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        // However, in type context, literal could be used for array size, etc.
        // If meta context, they can contribute to type computations
        if (node->meta) return expected; // Meta literals are polymorphic
        return ExprUniverse::Value;
    }
    
    ExprUniverse visit_str_lit(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        return ExprUniverse::Value;
    }
    
    ExprUniverse visit_char_lit(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        if (node->meta) return expected;
        return ExprUniverse::Value;
    }
    
    // Parentheses - pass through
    ExprUniverse visit_paren_expr(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        return dispatch(xast::c::paren_expr::inner(node), expected, scope);
    }
    
    // Comma expressions - last element determines type (C semantics)
    ExprUniverse visit_comma_expr(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        ExprUniverse result = ExprUniverse::Unknown;
        for (auto edge : node->opedges) {
            result = dispatch(edge->used(), expected, scope);
        }
        return result;
    }
    
    // Calls - depends on what's being called
    ExprUniverse visit_call(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        // call! to meta function - can return either type or value
        // Regular call - returns value
        // Type constructor call (meta) - returns type
        
        // For meta calls, we accept either universe; the actual check happens during instantiation
        if (node->meta) {
            // Meta call arguments should be validated in meta context (type or value)
            dispatch(xast::c::call::callable(node), ExprUniverse::Either, scope);
            dispatch(xast::c::call::args(node), ExprUniverse::Either, scope);
            return expected; // Meta calls are polymorphic in their result universe
        }
        
        // Regular (runtime) calls return values
        if (expected == ExprUniverse::Type) {
            // Can't use a runtime call in type context
            DiagnosticHandler::make(diag::id::call_on_type, node->sloc)
                .finish();
        }
        
        dispatch(xast::c::call::callable(node), ExprUniverse::Value, scope);
        dispatch(xast::c::call::args(node), ExprUniverse::Value, scope);
        return ExprUniverse::Value;
    }
    
    // Subscript - depends on context
    ExprUniverse visit_subscript(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        // In value context: array[index] - array subscript
        // In type context: [N]T style is handled differently (as unary_op slice)
        // This node is specifically the subscript operator arr[i]
        
        if (expected == ExprUniverse::Type) {
            DiagnosticHandler::make(diag::id::subscript_on_type, node->sloc)
                .finish();
        }
        
        dispatch(xast::c::subscript::array(node), ExprUniverse::Value, scope);
        dispatch(xast::c::subscript::index(node), ExprUniverse::Value, scope);
        return ExprUniverse::Value;
    }
    
    // Cast - value operation
    ExprUniverse visit_cast(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        // Cast: (type)expr - always a value operation
        // Type part is a type expression, expr part is value
        dispatch(xast::c::cast::type(node), ExprUniverse::Type, scope);
        dispatch(xast::c::cast::expr(node), ExprUniverse::Value, scope);
        
        if (expected == ExprUniverse::Type) {
            DiagnosticHandler::make(diag::id::invalid_operation_on_type, node->sloc)
                .finish();
        }
        return ExprUniverse::Value;
    }
    
    // Branch (if/if!) - meta version valid in type context
    ExprUniverse visit_branch(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        // if! is valid in both contexts - condition is always value/meta
        // Regular if is value-only
        
        if (!node->meta && expected == ExprUniverse::Type) {
            DiagnosticHandler::make(diag::id::invalid_operation_on_type, node->sloc)
                .finish();
            return ExprUniverse::Value;
        }
        
        // For meta branches (if!), the condition can involve type comparisons
        // For regular branches, the condition must be a value expression
        ExprUniverse cond_expected = node->meta ? ExprUniverse::Unknown : ExprUniverse::Value;
        dispatch(xast::c::branch::cond(node), cond_expected, scope);
        
        // Then and else branches inherit the expected context
        ExprUniverse then_universe = dispatch(xast::c::branch::then(node), expected, scope);
        
        xast::Node *else_node = xast::c::branch::else_(node);
        ExprUniverse else_universe = ExprUniverse::Unknown;
        if (else_node) {
            else_universe = dispatch(else_node, expected, scope);
        }
        
        // For if!, both branches must return the same universe
        if (node->meta && else_node) {
            if (then_universe != ExprUniverse::Unknown && 
                else_universe != ExprUniverse::Unknown &&
                then_universe != else_universe &&
                then_universe != ExprUniverse::Either &&
                else_universe != ExprUniverse::Either) {
                DiagnosticHandler::make(diag::id::if_meta_branch_type_mismatch, node->sloc)
                    .add(universe_name(then_universe))
                    .add(universe_name(else_universe))
                    .finish();
            }
        }
        
        if (node->meta) {
            return expected; // Meta branches are polymorphic
        }
        return ExprUniverse::Value;
    }
    
    // Loop (while/while!) - while! is NOT valid in type context
    ExprUniverse visit_loop(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        // while! can't be used in type expressions (loops don't produce types)
        // while can only be used in value context anyway
        
        if (expected == ExprUniverse::Type) {
            if (node->meta) {
                DiagnosticHandler::make(diag::id::while_meta_not_allowed_in_type, node->sloc)
                    .finish();
            } else {
                DiagnosticHandler::make(diag::id::invalid_operation_on_type, node->sloc)
                    .finish();
            }
            return ExprUniverse::Value;
        }
        
        dispatch(xast::c::loop::cond(node), ExprUniverse::Value, scope);
        dispatch(xast::c::loop::body(node), ExprUniverse::Value, scope);
        return ExprUniverse::Value;
    }
    
    // Function definition
    ExprUniverse visit_func(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        Scope *func_scope = node->scope ? node->scope : scope;
        
        // Parameters have type annotations
        dispatch(xast::c::func::params(node), ExprUniverse::Unknown, func_scope);
        
        // Return type is a type expression
        xast::Node *return_ty = xast::c::func::return_ty(node);
        dispatch(return_ty, ExprUniverse::Type, scope);
        
        // Determine body context based on return type
        // If the return type is 'type', the body produces a type
        // Otherwise, the body produces a value
        ExprUniverse body_context = ExprUniverse::Value;
        if (node->meta && return_ty) {
            // Check if return type is 'type'
            if (return_ty->kind == xast::nk::ref && return_ty->data.is_ident()) {
                std::string_view ret_type_name = *return_ty->data.ident;
                if (ret_type_name == "type") {
                    body_context = ExprUniverse::Type;
                }
            }
        }
        
        // Body context depends on return type
        dispatch(xast::c::func::body(node), body_context, func_scope);
        
        // A function definition in expression position is a value (closure/lambda)
        // unless it's a meta function defining a type
        return node->meta ? expected : ExprUniverse::Value;
    }
    
    // Binding - process definition and type annotation
    ExprUniverse visit_bind(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        // Type annotation is type context
        xast::Node *type_annot = xast::c::bind::type_annot(node);
        if (type_annot) {
            dispatch(type_annot, ExprUniverse::Type, scope);
        }
        
        // Definition:
        // - For let! bindings defining types: type context
        // - For let! bindings defining values: value context
        // - For regular let: value context
        xast::Node *def = xast::c::bind::def(node);
        if (def) {
            ExprUniverse def_context = ExprUniverse::Value;
            if (node->meta) {
                // Meta binding - could be defining a type or a value
                // Try to infer from the definition
                if (def->kind == xast::nk::composite || 
                    def->kind == xast::nk::struct_ || 
                    def->kind == xast::nk::union_) {
                    def_context = ExprUniverse::Type;
                } else if (def->kind == xast::nk::func && def->meta) {
                    // Meta function - needs to check return type
                    def_context = ExprUniverse::Either;
                } else if (def->kind == xast::nk::call && def->meta) {
                    // Meta call - context is polymorphic
                    def_context = ExprUniverse::Either;
                } else {
                    // For other meta bindings, inherit from enclosing context
                    // This handles cases like let! result = A > B inside a type-returning fn
                    if (expected == ExprUniverse::Type) {
                        def_context = ExprUniverse::Type;
                    }
                    // If expected is Unknown or Value, use Value context
                }
            }
            dispatch(def, def_context, scope);
        }
        
        return ExprUniverse::Unknown; // Bindings aren't expressions themselves
    }
    
    // Composite type - always type context
    ExprUniverse visit_composite(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        Scope *composite_scope = node->scope ? node->scope : scope;
        dispatch(xast::c::composite::layout(node), ExprUniverse::Type, composite_scope);
        dispatch(xast::c::composite::namespc(node), ExprUniverse::Value, composite_scope);
        return ExprUniverse::Type;
    }
    
    ExprUniverse visit_struct_(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), ExprUniverse::Type, scope);
        }
        return ExprUniverse::Type;
    }
    
    ExprUniverse visit_union_(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), ExprUniverse::Type, scope);
        }
        return ExprUniverse::Type;
    }
    
    ExprUniverse visit_field(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        dispatch(xast::c::field::type_annot(node), ExprUniverse::Type, scope);
        return ExprUniverse::Type;
    }
    
    ExprUniverse visit_variant(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        xast::Node *payload = xast::c::variant::payload(node);
        if (payload) {
            dispatch(payload, ExprUniverse::Type, scope);
        }
        return ExprUniverse::Type;
    }
    
    ExprUniverse visit_param(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        dispatch(xast::c::param::type_annot(node), ExprUniverse::Type, scope);
        return ExprUniverse::Unknown;
    }
    
    ExprUniverse visit_params(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), expected, scope);
        }
        return ExprUniverse::Unknown;
    }
    
    ExprUniverse visit_captures(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        return ExprUniverse::Unknown;
    }
    
    ExprUniverse visit_block(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        ExprUniverse last = ExprUniverse::Unknown;
        for (auto edge : node->opedges) {
            last = dispatch(edge->used(), expected, scope);
        }
        return last;
    }
    
    ExprUniverse visit_prog(xast::Node *node, ExprUniverse expected, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), ExprUniverse::Unknown, scope);
        }
        return ExprUniverse::Unknown;
    }
};

// ============================================================================
// Constant Expression Evaluator
// ============================================================================
// Evaluates compile-time constant expressions.
// This enables:
// - Constant folding for literals and const bindings
// - if! evaluation with compile-time conditions
// - while! evaluation for compile-time loops
// - Meta function execution with constant arguments
//
// Results are stored in a ConstValue variant type and can be integers,
// strings, types, or error values.

struct ConstValue {
    enum Kind { None, Integer, String, Type, Node, Error };
    Kind kind = None;
    
    // Value storage
    int64_t int_val = 0;
    std::string str_val;
    fe::Type *type_val = nullptr;
    xast::Node *node_val = nullptr;  // For AST nodes that are compile-time values
    
    // Track instantiation source for composite nodes
    // This allows us to create proper InstantiatedType with Type* args
    std::string template_name;                    // e.g., "Vector"
    std::vector<fe::Type *> template_args;        // e.g., [i32] (canonical Type*)
    
    ConstValue() : kind(None) {}
    ConstValue(int64_t v) : kind(Integer), int_val(v) {}
    ConstValue(std::string s) : kind(String), str_val(std::move(s)) {}
    ConstValue(fe::Type *t) : kind(Type), type_val(t) {}
    ConstValue(xast::Node *n) : kind(Node), node_val(n) {}
    
    // Create a node value with instantiation info
    static ConstValue node_with_instantiation(xast::Node *n, const std::string &tmpl_name, 
                                               const std::vector<fe::Type *> &args) {
        ConstValue v(n);
        v.template_name = tmpl_name;
        v.template_args = args;
        return v;
    }
    
    // Check if this has instantiation info
    bool has_instantiation_info() const {
        return !template_name.empty();
    }
    
    // Build instantiation name string (for display/caching)
    std::string instantiation_name() const {
        if (template_name.empty()) return "";
        std::string str = template_name + "!(";
        for (size_t i = 0; i < template_args.size(); ++i) {
            if (i > 0) str += ", ";
            str += template_args[i] ? template_args[i]->get_canonical()->stringify() : "?";
        }
        str += ")";
        return str;
    }
    
    static ConstValue error() { ConstValue v; v.kind = Error; return v; }
    
    bool is_none() const { return kind == None; }
    bool is_integer() const { return kind == Integer; }
    bool is_string() const { return kind == String; }
    bool is_type() const { return kind == Type; }
    bool is_node() const { return kind == Node; }
    bool is_error() const { return kind == Error; }
    
    // Truthy for if! conditions
    bool is_truthy() const {
        switch (kind) {
            case Integer: return int_val != 0;
            case String: return !str_val.empty();
            case Type: return type_val != nullptr;
            case Node: return node_val != nullptr;
            default: return false;
        }
    }
    
    std::string stringify() const {
        switch (kind) {
            case Integer: return std::to_string(int_val);
            case String: return "\"" + str_val + "\"";
            case Type: return type_val ? type_val->stringify() : "<null type>";
            case Node: return node_val ? "<node>" : "<null node>";
            case Error: return "<error>";
            default: return "<none>";
        }
    }
};

// ============================================================================
// Global pointer to the StringPool for identifier strings
static StringPool *g_string_pool = nullptr;

static std::string_view intern_string_global(std::string_view s) {
    assert(g_string_pool && "StringPool not initialized");
    return g_string_pool->get(s);
}

// AST Cloner with Type Substitution
// ============================================================================
// Clones an AST subtree while substituting type references according to a map

struct ASTCloner {
    std::unordered_map<std::string, Type *> &type_subst;
    // For composite type substitutions, we also store the node to inline
    std::unordered_map<std::string, xast::Node *> node_subst;
    
    ASTCloner(std::unordered_map<std::string, Type *> &subst)
        : type_subst(subst) {}
    
    void set_node_subst(const std::string &name, xast::Node *node) {
        node_subst[name] = node;
    }
    
    xast::Node *clone(xast::Node *node) {
        if (!node) return nullptr;
        
        // Check if this is a type reference that needs substitution
        if (node->kind == xast::nk::ref && node->data.is_ident()) {
            std::string name = std::string(*node->data.ident);
            
            // First check for node substitutions (composite types)
            auto node_it = node_subst.find(name);
            if (node_it != node_subst.end()) {
                // Return the substituted node directly - it's already been cloned with proper substitutions
                return node_it->second;
            }
            
            // Then check for type substitutions (primitive types)
            auto it = type_subst.find(name);
            if (it != type_subst.end()) {
                // Create a new ref node with the substituted type
                xast::Node *new_node = new xast::Node(xast::nk::ref);
                std::string_view new_name = intern_string_global(it->second->stringify());
                new_node->data = Identifier{ new_name, node->sloc };
                new_node->meta = node->meta;
                new_node->type = it->second;
                new_node->sloc = node->sloc;
                return new_node;
            }
        }
        
        // Clone the node - use default constructor to avoid pre-creating edges
        xast::Node *new_node = new xast::Node();
        new_node->kind = node->kind;
        new_node->data = node->data;
        new_node->meta = node->meta;
        new_node->sloc = node->sloc;
        // Don't copy the type - it will be set during later analysis
        
        // Clone children
        for (auto edge : node->opedges) {
            xast::Use *new_edge = new xast::Use(new_node);
            new_edge->set(clone(edge->used()));
            new_node->opedges.push_back(new_edge);
        }
        
        return new_node;
    }
};
struct ConstantExprEvaluator : xast::Visitor<ConstValue, Scope *> {
    // Cache of evaluated constant values per node
    std::unordered_map<xast::Node*, ConstValue> const_cache;
    
    // Type cache for looking up type bindings
    std::unordered_map<std::string, Type *> &type_cache;
    
    // Cache for interned InstantiatedTypes (key = canonical string)
    std::unordered_map<std::string, InstantiatedType *> instantiated_type_cache;
    
    // Current type parameter substitutions (for type constructor calls)
    std::unordered_map<std::string, Type *> current_subst;
    
    // Current node substitutions (for composite type arguments)
    std::unordered_map<std::string, xast::Node *> current_node_subst;
    
    ConstantExprEvaluator(std::unordered_map<std::string, Type *> &cache)
        : type_cache(cache) {}
    
    // Get or create an InstantiatedType, ensuring canonical args and interning
    Type *get_or_create_instantiated_type(const std::string &template_name,
                                           const std::vector<Type *> &args,
                                           xast::Node *inst_node) {
        // Build canonical key with canonical arg types
        std::string key = template_name + "!(";
        std::vector<Type *> canonical_args;
        for (size_t i = 0; i < args.size(); ++i) {
            Type *arg = args[i];
            Type *canonical_arg = arg ? arg->get_canonical() : nullptr;
            canonical_args.push_back(canonical_arg);
            if (i > 0) key += ", ";
            key += canonical_arg ? canonical_arg->stringify() : "?";
        }
        key += ")";
        
        // Check if already interned
        auto it = instantiated_type_cache.find(key);
        if (it != instantiated_type_cache.end()) {
            return it->second;
        }
        
        // Create new InstantiatedType with canonical args
        // It is its own canonical type (self-referential)
        InstantiatedType *ty = new InstantiatedType(template_name, canonical_args, inst_node);
        instantiated_type_cache[key] = ty;
        type_cache[key] = ty;  // Also add to main type cache
        
        return ty;
    }
    
    // Integer literals
    ConstValue visit_int_lit(xast::Node *node, Scope *scope) override {
        if (node->data.is_ival()) {
            int64_t val = static_cast<int64_t>(node->data.ival);
            const_cache[node] = ConstValue(val);
            return ConstValue(val);
        }
        return ConstValue::error();
    }
    
    // Character literals (stored as ival, convert to integer)
    ConstValue visit_char_lit(xast::Node *node, Scope *scope) override {
        if (node->data.is_ival()) {
            int64_t val = static_cast<int64_t>(node->data.ival);
            const_cache[node] = ConstValue(val);
            return ConstValue(val);
        }
        return ConstValue::error();
    }
    
    // String literals (stored as Identifier)
    ConstValue visit_str_lit(xast::Node *node, Scope *scope) override {
        if (node->data.is_ident()) {
            std::string s(*node->data.ident);
            const_cache[node] = ConstValue(s);
            return ConstValue(s);
        }
        return ConstValue::error();
    }
    
    // References - look up const value from binding
    ConstValue visit_ref(xast::Node *node, Scope *scope) override {
        if (!node->data.is_ident()) return ConstValue();
        
        Identifier ident = node->data;
        xast::Node *decl = find_symbol_in_any(scope, *ident);
        
        if (!decl) return ConstValue();
        
        // Check if we've already evaluated this binding
        auto it = const_cache.find(decl);
        if (it != const_cache.end()) {
            return it->second;
        }
        
        // Check if it's a meta binding
        if (!decl->meta) {
            // Non-meta binding - not evaluable at compile time
            return ConstValue();
        }
        
        // Check if it's a type binding
        if (decl->type != nullptr) {
            auto type_it = type_cache.find(std::string(*ident));
            if (type_it != type_cache.end()) {
                return ConstValue(type_it->second);
            }
        }
        
        // Try to evaluate the definition
        xast::Node *def = xast::c::bind::def(decl);
        if (def) {
            // Use the binding's scope for evaluation
            Scope *bind_scope = decl->scope ? decl->scope : scope;
            ConstValue result = dispatch(def, bind_scope);
            const_cache[decl] = result;
            return result;
        }
        
        return ConstValue();
    }
    
    // Binary operations
    ConstValue visit_binary_op(xast::Node *node, Scope *scope) override {
        if (!node->data.is_op()) return ConstValue::error();
        
        Op op = node->data;
        ConstValue lhs = dispatch(xast::c::binary_op::lhs(node), scope);
        ConstValue rhs = dispatch(xast::c::binary_op::rhs(node), scope);
        
        // Both operands must be evaluable
        if (lhs.is_none() || lhs.is_error() || rhs.is_none() || rhs.is_error()) {
            return ConstValue();
        }
        
        // Integer arithmetic
        if (lhs.is_integer() && rhs.is_integer()) {
            int64_t a = lhs.int_val;
            int64_t b = rhs.int_val;
            int64_t result = 0;
            
            switch (op.kind) {
                case op::add: result = a + b; break;
                case op::sub: result = a - b; break;
                case op::mult: result = a * b; break;
                case op::div:
                    if (b == 0) {
                        DiagnosticHandler::make(diag::id::const_eval_division_by_zero, node->sloc)
                            .finish();
                        return ConstValue::error();
                    }
                    result = a / b;
                    break;
                case op::mod:
                    if (b == 0) {
                        DiagnosticHandler::make(diag::id::const_eval_division_by_zero, node->sloc)
                            .finish();
                        return ConstValue::error();
                    }
                    result = a % b;
                    break;
                case op::gt: result = a > b ? 1 : 0; break;
                case op::lt: result = a < b ? 1 : 0; break;
                case op::gte: result = a >= b ? 1 : 0; break;
                case op::lte: result = a <= b ? 1 : 0; break;
                case op::eq: result = a == b ? 1 : 0; break;
                case op::neq: result = a != b ? 1 : 0; break;
                case op::land: result = (a && b) ? 1 : 0; break;
                case op::lor: result = (a || b) ? 1 : 0; break;
                default:
                    return ConstValue(); // Not a compile-time evaluable operation
            }
            
            const_cache[node] = ConstValue(result);
            return ConstValue(result);
        }
        
        // String concatenation for add
        if (lhs.is_string() && rhs.is_string() && op.kind == op::add) {
            std::string result = lhs.str_val + rhs.str_val;
            const_cache[node] = ConstValue(result);
            return ConstValue(result);
        }
        
        // String equality
        if (lhs.is_string() && rhs.is_string()) {
            if (op.kind == op::eq) {
                int64_t result = lhs.str_val == rhs.str_val ? 1 : 0;
                const_cache[node] = ConstValue(result);
                return ConstValue(result);
            }
            if (op.kind == op::neq) {
                int64_t result = lhs.str_val != rhs.str_val ? 1 : 0;
                const_cache[node] = ConstValue(result);
                return ConstValue(result);
            }
        }
        
        // Type equality - for compile-time type comparisons like T == i32
        if (lhs.is_type() && rhs.is_type()) {
            if (op.kind == op::eq) {
                // Types are equal if they have the same canonical form
                // For now, compare by string representation
                int64_t result = lhs.type_val->stringify() == rhs.type_val->stringify() ? 1 : 0;
                const_cache[node] = ConstValue(result);
                return ConstValue(result);
            }
            if (op.kind == op::neq) {
                int64_t result = lhs.type_val->stringify() != rhs.type_val->stringify() ? 1 : 0;
                const_cache[node] = ConstValue(result);
                return ConstValue(result);
            }
        }
        
        return ConstValue(); // Can't evaluate
    }
    
    // Parentheses - pass through
    ConstValue visit_paren_expr(xast::Node *node, Scope *scope) override {
        return dispatch(xast::c::paren_expr::inner(node), scope);
    }
    
    // Comma expression - return last value
    ConstValue visit_comma_expr(xast::Node *node, Scope *scope) override {
        ConstValue result;
        for (auto edge : node->opedges) {
            result = dispatch(edge->used(), scope);
        }
        return result;
    }
    
    // if! - compile-time conditional
    ConstValue visit_branch(xast::Node *node, Scope *scope) override {
        if (!node->meta) {
            // Non-meta if - not evaluable at compile time
            return ConstValue();
        }
        
        // Evaluate condition
        ConstValue cond = dispatch(xast::c::branch::cond(node), scope);
        
        if (cond.is_none() || cond.is_error()) {
            return ConstValue();
        }
        
        // Select branch based on condition truthiness
        if (cond.is_truthy()) {
            return dispatch(xast::c::branch::then(node), scope);
        } else {
            xast::Node *else_node = xast::c::branch::else_(node);
            if (else_node) {
                return dispatch(else_node, scope);
            }
            return ConstValue(); // No else branch
        }
    }
    
    // while! - compile-time loop
    ConstValue visit_loop(xast::Node *node, Scope *scope) override {
        if (!node->meta) {
            return ConstValue();
        }
        
        // Execute the loop at compile time
        // Be careful of infinite loops - add an iteration limit
        const int MAX_ITERATIONS = 10000;
        int iterations = 0;
        ConstValue result;
        
        while (iterations < MAX_ITERATIONS) {
            ConstValue cond = dispatch(xast::c::loop::cond(node), scope);
            
            if (cond.is_none() || cond.is_error()) {
                return ConstValue();
            }
            
            if (!cond.is_truthy()) {
                break;
            }
            
            result = dispatch(xast::c::loop::body(node), scope);
            iterations++;
        }
        
        if (iterations >= MAX_ITERATIONS) {
            // Potential infinite loop
            DiagnosticHandler::make(diag::id::const_eval_overflow, node->sloc)
                .finish();
            return ConstValue::error();
        }
        
        return result;
    }
    
    // Block - evaluate all statements, return last
    ConstValue visit_block(xast::Node *node, Scope *scope) override {
        ConstValue result;
        for (auto edge : node->opedges) {
            result = dispatch(edge->used(), scope);
            if (result.is_error()) return result;
        }
        return result;
    }
    
    // Check if a name is a built-in type intrinsic
    bool is_type_intrinsic(const std::string &name) {
        return name == "_is_pointer" || name == "_is_struct" || name == "_is_union" ||
               name == "_is_array" || name == "_is_function" || name == "_is_primitive" ||
               name == "_is_integral" || name == "_is_signed" || name == "_is_unsigned" ||
               name == "_sizeof" || name == "_alignof" || name == "_type_name";
    }
    
    // Evaluate a type intrinsic
    ConstValue eval_type_intrinsic(const std::string &name, const std::vector<ConstValue> &args, Scope *scope) {
        if (args.empty()) return ConstValue::error();
        
        // Most intrinsics take a type argument
        if (!args[0].is_type()) return ConstValue();
        
        Type *ty = args[0].type_val;
        if (!ty) return ConstValue();
        
        if (name == "_is_pointer") {
            return ConstValue(ty->get_kind() == typekind::pointer_t ? 1 : 0);
        }
        if (name == "_is_struct") {
            return ConstValue(ty->get_kind() == typekind::struct_t ? 1 : 0);
        }
        if (name == "_is_union") {
            return ConstValue(ty->get_kind() == typekind::union_t ? 1 : 0);
        }
        if (name == "_is_array") {
            return ConstValue(ty->get_kind() == typekind::array_t ? 1 : 0);
        }
        if (name == "_is_function") {
            return ConstValue(ty->get_kind() == typekind::function_t ? 1 : 0);
        }
        if (name == "_is_primitive") {
            return ConstValue(ty->is_primitive() ? 1 : 0);
        }
        if (name == "_is_integral") {
            return ConstValue(ty->is_integral() ? 1 : 0);
        }
        if (name == "_is_signed") {
            return ConstValue(ty->is_signed() ? 1 : 0);
        }
        if (name == "_is_unsigned") {
            return ConstValue(ty->is_unsigned() ? 1 : 0);
        }
        if (name == "_sizeof") {
            return ConstValue(static_cast<int64_t>(ty->get_size()));
        }
        if (name == "_alignof") {
            // For now, alignment = size (simplified)
            return ConstValue(static_cast<int64_t>(ty->get_size()));
        }
        if (name == "_type_name") {
            return ConstValue(ty->stringify());
        }
        
        return ConstValue();
    }
    
    // Calls - try to evaluate meta calls including intrinsics
    ConstValue visit_call(xast::Node *node, Scope *scope) override {
        xast::Node *callable = xast::c::call::callable(node);
        xast::Node *args_node = xast::c::call::args(node);
        
        // Get the callable name
        std::string callable_name;
        if (callable && callable->kind == xast::nk::ref && callable->data.is_ident()) {
            callable_name = std::string(*callable->data.ident);
        }
        
        // Evaluate arguments - always try to evaluate them
        std::vector<ConstValue> args;
        if (args_node) {
            if (args_node->kind == xast::nk::comma_expr) {
                for (auto edge : args_node->opedges) {
                    ConstValue arg = dispatch(edge->used(), scope);
                    args.push_back(arg);
                }
            } else {
                ConstValue arg = dispatch(args_node, scope);
                args.push_back(arg);
            }
        }
        
        // Check for type intrinsics - these always work regardless of meta flag
        if (is_type_intrinsic(callable_name)) {
            return eval_type_intrinsic(callable_name, args, scope);
        }
        
        // For non-meta calls, stop here
        if (!node->meta) {
            return ConstValue();
        }
        
        // Look up the callable as a meta function
        xast::Node *fn_decl = find_symbol_in_any(scope, callable_name);
        if (!fn_decl || !fn_decl->meta) {
            return ConstValue();
        }
        
        // Get the function definition
        xast::Node *fn_def = xast::c::bind::def(fn_decl);
        
        // If no direct definition, check if this is a function parameter whose value is cached
        if (!fn_def || fn_def->kind != xast::nk::func) {
            auto it = const_cache.find(fn_decl);
            if (it != const_cache.end()) {
                ConstValue fn_val = it->second;
                // If the cached value is a node (function), use that
                if (fn_val.is_node() && fn_val.node_val && fn_val.node_val->kind == xast::nk::func) {
                    fn_def = fn_val.node_val;
                } else if (fn_val.is_node() && fn_val.node_val && fn_val.node_val->kind == xast::nk::bind) {
                    // It's a binding to a function, get the definition
                    fn_def = xast::c::bind::def(fn_val.node_val);
                }
            }
        }
        
        if (!fn_def || fn_def->kind != xast::nk::func) {
            return ConstValue();
        }
        
        // Create a local scope for the function call with parameter bindings
        Scope *fn_scope = new Scope();
        fn_scope->parent = fn_decl->scope ? fn_decl->scope : scope;
        
        // Save and set up type substitutions for this call
        auto old_subst = current_subst;
        auto old_node_subst = current_node_subst;
        current_subst.clear();
        current_node_subst.clear();
        
        // Bind arguments to parameters
        xast::Node *params = xast::c::func::params(fn_def);
        if (params) {
            size_t i = 0;
            for (auto edge : params->opedges) {
                xast::Node *param = edge->used();
                if (param && param->data.is_ident() && i < args.size()) {
                    std::string param_name = std::string(*param->data.ident);
                    
                    // Create a synthetic binding for the parameter
                    xast::Node *param_bind = new xast::Node(xast::nk::bind);
                    param_bind->data = param->data;
                    param_bind->meta = true;
                    
                    // If the argument is a type, set the type field and substitution
                    if (args[i].is_type()) {
                        param_bind->type = args[i].type_val;
                        type_cache[param_name] = args[i].type_val;
                        current_subst[param_name] = args[i].type_val;
                    } else if (args[i].is_node()) {
                        // If the argument is a composite node, create an anonymous type for it
                        xast::Node *arg_node = args[i].node_val;
                        if (arg_node && arg_node->kind == xast::nk::composite) {
                            // Generate a unique name for this anonymous type
                            static int anon_counter = 0;
                            std::string anon_name = "__anon_" + std::to_string(anon_counter++);
                            
                            // Determine if it's a struct or union
                            xast::Node *layout = xast::c::composite::layout(arg_node);
                            Type *anon_ty = nullptr;
                            if (layout && layout->kind == xast::nk::struct_) {
                                anon_ty = new StructType(arg_node, anon_name);
                            } else if (layout && layout->kind == xast::nk::union_) {
                                anon_ty = new UnionType(arg_node, anon_name);
                            }
                            
                            if (anon_ty) {
                                param_bind->type = anon_ty;
                                type_cache[param_name] = anon_ty;
                                current_subst[param_name] = anon_ty;
                                // Also store the node for inlining during cloning
                                current_node_subst[param_name] = arg_node;
                            }
                        }
                    }
                    
                    // Cache the argument value
                    const_cache[param_bind] = args[i];
                    fn_scope->symbols[param_name] = param_bind;
                }
                i++;
            }
        }
        
        // Evaluate the function body
        xast::Node *body = xast::c::func::body(fn_def);
        ConstValue result;
        if (body) {
            result = dispatch(body, fn_scope);
        }
        
        // Track instantiation info for this call
        // If the result is a node (composite) and doesn't already have instantiation info,
        // record the template name and args as Type* pointers
        if (result.is_node() && !result.has_instantiation_info()) {
            result.template_name = callable_name;
            
            // Convert args to Type* pointers (canonical form)
            for (const auto &arg : args) {
                if (arg.is_type() && arg.type_val) {
                    result.template_args.push_back(arg.type_val->get_canonical());
                } else if (arg.is_node() && arg.has_instantiation_info()) {
                    // Nested instantiation - need to create an InstantiatedType for it
                    // This handles cases like Vector!(Option!(i32))
                    Type *nested_ty = get_or_create_instantiated_type(
                        arg.template_name, arg.template_args, arg.node_val);
                    result.template_args.push_back(nested_ty);
                } else {
                    // For non-type args (like integer constants), we'd need a different approach
                    // For now, push nullptr as placeholder
                    result.template_args.push_back(nullptr);
                }
            }
            
            // Now that we have the instantiation info, create the concrete type
            // and attach it to the composite node
            if (result.node_val && result.node_val->kind == xast::nk::composite) {
                Type *inst_ty = get_or_create_instantiated_type(
                    result.template_name, result.template_args, result.node_val);
                result.node_val->type = inst_ty;
            }
        }
        
        // Restore old substitutions
        current_subst = old_subst;
        current_node_subst = old_node_subst;
        
        return result;
    }
    
    // Composite types - clone with type substitutions applied
    ConstValue visit_composite(xast::Node *node, Scope *scope) override {
        // If we have substitutions to apply, clone the composite with them
        if (!current_subst.empty() || !current_node_subst.empty()) {
            ASTCloner cloner(current_subst);
            // Add node substitutions for composite types
            for (auto &[k, v] : current_node_subst) {
                cloner.set_node_subst(k, v);
            }
            xast::Node *cloned = cloner.clone(node);
            
            // Run the indexing pass on the cloned composite to set up scopes properly
            // This is crucial for functions in where-clauses to have their scopes set up
            IndexingPass indexer;
            indexer.dispatch(cloned, scope);
            
            return ConstValue(cloned);
        }
        // Return the composite node itself - it will be processed during instantiation
        return ConstValue(node);
    }
    
    // Function definitions - for meta functions, return the node
    ConstValue visit_func(xast::Node *node, Scope *scope) override {
        if (node->meta) {
            return ConstValue(node);
        }
        return ConstValue();
    }
    
    // Bindings - evaluate the definition
    ConstValue visit_bind(xast::Node *node, Scope *scope) override {
        xast::Node *def = xast::c::bind::def(node);
        if (def && node->meta) {
            return dispatch(def, scope);
        }
        return ConstValue();
    }
    
    // Cast - try to evaluate if both parts are constant
    ConstValue visit_cast(xast::Node *node, Scope *scope) override {
        ConstValue expr = dispatch(xast::c::cast::expr(node), scope);
        // For now, just return the expression value - type casting is deferred
        return expr;
    }
    
    // Subscript - try to evaluate
    ConstValue visit_subscript(xast::Node *node, Scope *scope) override {
        ConstValue array = dispatch(xast::c::subscript::array(node), scope);
        ConstValue index = dispatch(xast::c::subscript::index(node), scope);
        
        // String indexing
        if (array.is_string() && index.is_integer()) {
            int64_t idx = index.int_val;
            if (idx >= 0 && idx < static_cast<int64_t>(array.str_val.size())) {
                return ConstValue(static_cast<int64_t>(array.str_val[idx]));
            }
        }
        
        return ConstValue();
    }
    
    // Unary operations - handle pointer/slice type construction
    ConstValue visit_unary_op(xast::Node *node, Scope *scope) override {
        if (!node->data.is_op()) return ConstValue::error();
        
        Op op = node->data;
        ConstValue operand = dispatch(xast::c::unary_op::operand(node), scope);
        
        if (operand.is_none() || operand.is_error()) {
            return operand;
        }
        
        // For type expressions, handle pointer and slice construction
        if (operand.is_type()) {
            Type *inner_type = operand.type_val;
            if (op.kind == op::indirect) {
                // *T -> pointer type
                Type *ptr_type = new PointerType(nullptr, inner_type);
                return ConstValue(ptr_type);
            }
            if (op.kind == op::slice) {
                // []T -> array/slice type
                Type *arr_type = new ArrayType(nullptr, inner_type, 0);
                return ConstValue(arr_type);
            }
        }
        
        // For integer operands
        if (operand.is_integer()) {
            int64_t val = operand.int_val;
            int64_t result = 0;
            
            switch (op.kind) {
                case op::neg: result = -val; break;
                case op::lnot: result = !val ? 1 : 0; break;
                default:
                    return ConstValue();
            }
            
            const_cache[node] = ConstValue(result);
            return ConstValue(result);
        }
        
        return ConstValue();
    }
    
    // Type structure nodes - not directly evaluable
    ConstValue visit_struct_(xast::Node *node, Scope *scope) override { return ConstValue(); }
    ConstValue visit_union_(xast::Node *node, Scope *scope) override { return ConstValue(); }
    ConstValue visit_field(xast::Node *node, Scope *scope) override { return ConstValue(); }
    ConstValue visit_variant(xast::Node *node, Scope *scope) override { return ConstValue(); }
    ConstValue visit_param(xast::Node *node, Scope *scope) override { return ConstValue(); }
    ConstValue visit_params(xast::Node *node, Scope *scope) override { return ConstValue(); }
    ConstValue visit_captures(xast::Node *node, Scope *scope) override { return ConstValue(); }
    ConstValue visit_prog(xast::Node *node, Scope *scope) override { return ConstValue(); }
};

// ============================================================================
// Meta Propagation Pass
// ============================================================================
// This pass propagates the 'meta' flag through expressions.
// An expression is meta if:
// 1. It was explicitly marked meta (like fn!, call!, if!, while!, let!)
// 2. All of its operands are meta (for operators, calls, etc.)
// 3. It is a literal (int, char, string)
// 4. It references a meta symbol
//
// The pass returns true if the node is meta, and sets node->meta accordingly.

struct MetaPropagationPass : xast::Visitor<bool, Scope *> {
    // Literals are always meta
    bool visit_int_lit(xast::Node *node, Scope *scope) override {
        node->meta = true;
        return true;
    }
    bool visit_str_lit(xast::Node *node, Scope *scope) override {
        node->meta = true;
        return true;
    }
    bool visit_char_lit(xast::Node *node, Scope *scope) override {
        node->meta = true;
        return true;
    }

    // References: meta if the referenced symbol is meta
    bool visit_ref(xast::Node *node, Scope *scope) override {
        assert(node->data.is_ident());
        Identifier ident = node->data;
        xast::Node *decl = find_symbol_in_any(scope, *ident);
        if (decl && decl->meta) {
            node->meta = true;
            return true;
        }
        return false;
    }

    // Binary operations: meta if both operands are meta
    bool visit_binary_op(xast::Node *node, Scope *scope) override {
        bool lhs_meta = dispatch(xast::c::binary_op::lhs(node), scope);
        bool rhs_meta = dispatch(xast::c::binary_op::rhs(node), scope);
        node->meta = lhs_meta && rhs_meta;
        return node->meta;
    }

    // Unary operations: meta if operand is meta
    bool visit_unary_op(xast::Node *node, Scope *scope) override {
        bool operand_meta = dispatch(xast::c::unary_op::operand(node), scope);
        node->meta = operand_meta;
        return node->meta;
    }

    // Parenthesized expressions: meta if inner is meta
    bool visit_paren_expr(xast::Node *node, Scope *scope) override {
        bool inner_meta = dispatch(xast::c::paren_expr::inner(node), scope);
        node->meta = inner_meta;
        return node->meta;
    }

    // Calls: if already marked meta (call!), propagate through arguments
    // Non-meta calls are not meta even if args are meta
    bool visit_call(xast::Node *node, Scope *scope) override {
        // Process callable and args regardless
        dispatch(xast::c::call::callable(node), scope);
        dispatch(xast::c::call::args(node), scope);
        // Only meta if explicitly marked
        return node->meta;
    }

    // Comma expressions: meta if all children are meta
    bool visit_comma_expr(xast::Node *node, Scope *scope) override {
        bool all_meta = true;
        for (auto edge : node->opedges) {
            if (!dispatch(edge->used(), scope)) {
                all_meta = false;
            }
        }
        node->meta = all_meta;
        return node->meta;
    }

    // Functions: if marked meta (fn!), the function itself is meta
    // Process children with the function's scope
    bool visit_func(xast::Node *node, Scope *scope) override {
        Scope *func_scope = node->scope ? node->scope : scope;
        
        // If this is a meta function (fn!), mark all parameters as meta
        if (node->meta) {
            xast::Node *params = xast::c::func::params(node);
            if (params) {
                for (auto edge : params->opedges) {
                    xast::Node *param = edge->used();
                    if (param && param->kind == xast::nk::param) {
                        param->meta = true;
                    }
                }
            }
        }
        
        // Process captures, params, return type, body
        for (auto edge : node->opedges) {
            dispatch(edge->used(), func_scope);
        }
        return node->meta;
    }

    // Bindings: meta if explicitly marked (let!)
    // The definition's meta status is determined separately
    bool visit_bind(xast::Node *node, Scope *scope) override {
        // Process the definition
        dispatch(xast::c::bind::def(node), scope);
        dispatch(xast::c::bind::type_annot(node), scope);
        return node->meta;
    }

    // Params are meta if their function is meta
    bool visit_param(xast::Node *node, Scope *scope) override {
        dispatch(xast::c::param::type_annot(node), scope);
        return node->meta;
    }

    // Blocks: process all statements
    bool visit_block(xast::Node *node, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), scope);
        }
        return node->meta;
    }

    // Branches: meta if explicitly marked (if!)
    bool visit_branch(xast::Node *node, Scope *scope) override {
        dispatch(xast::c::branch::cond(node), scope);
        dispatch(xast::c::branch::then(node), scope);
        dispatch(xast::c::branch::else_(node), scope);
        return node->meta;
    }

    // Loops: meta if explicitly marked (while!)
    bool visit_loop(xast::Node *node, Scope *scope) override {
        dispatch(xast::c::loop::cond(node), scope);
        dispatch(xast::c::loop::body(node), scope);
        return node->meta;
    }

    // Composite types (wrapper for struct/union with optional temp name and where clause)
    bool visit_composite(xast::Node *node, Scope *scope) override {
        // Use the composite's scope if it has one (created by IndexingPass)
        Scope *composite_scope = node->scope ? node->scope : scope;

        // Traverse layout and namespace with the composite's scope
        dispatch(xast::c::composite::layout(node), composite_scope);
        dispatch(xast::c::composite::namespc(node), composite_scope);

        // Composite types are inherently meta
        node->meta = true;
        return true;
    }

    // Structs and unions in type definitions
    bool visit_struct_(xast::Node *node, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), scope);
        }
        // Type definitions are inherently meta
        node->meta = true;
        return true;
    }

    bool visit_union_(xast::Node *node, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), scope);
        }
        node->meta = true;
        return true;
    }

    bool visit_field(xast::Node *node, Scope *scope) override {
        dispatch(xast::c::field::type_annot(node), scope);
        node->meta = true;
        return true;
    }

    bool visit_variant(xast::Node *node, Scope *scope) override {
        dispatch(xast::c::variant::payload(node), scope);
        node->meta = true;
        return true;
    }

    bool visit_cast(xast::Node *node, Scope *scope) override {
        bool expr_meta = dispatch(xast::c::cast::expr(node), scope);
        dispatch(xast::c::cast::type(node), scope);
        node->meta = expr_meta;
        return node->meta;
    }

    bool visit_subscript(xast::Node *node, Scope *scope) override {
        bool array_meta = dispatch(xast::c::subscript::array(node), scope);
        bool index_meta = dispatch(xast::c::subscript::index(node), scope);
        node->meta = array_meta && index_meta;
        return node->meta;
    }
    
    // Program node
    bool visit_prog(xast::Node *node, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), scope);
        }
        return false;
    }

    // Helper nodes
    bool visit_params(xast::Node *node, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), scope);
        }
        return false;
    }

    bool visit_captures(xast::Node *node, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), scope);
        }
        return false;
    }
};

// ============================================================================
// Meta Dependency Collector
// ============================================================================
// Collects dependencies for meta nodes by finding all referenced symbols.
// For a meta binding, we collect all symbols referenced in its definition.

// Represents a dependency with its source location (where the dependency originates)
struct DependencyInfo {
    xast::Node *target;           // The node being depended on
    xast::Node *site;             // The site where dependency occurs (field, variant, etc.)
    bool is_self_reference;       // Whether this is a direct self-reference (type contains itself)
    bool is_through_indirection;  // Whether this dependency is through a pointer/slice (safe for layout)
};

struct MetaDependencyCollector : xast::Visitor<void, std::vector<DependencyInfo>&, Scope*> {
    // Track current composite for detecting direct self-references in fields
    xast::Node *current_composite = nullptr;
    
    // Track current field/variant for dependency site tracking
    xast::Node *current_site = nullptr;
    
    // Track whether we're inside a pointer/slice indirection (layout-safe)
    int indirection_depth = 0;
    
    // When we encounter a reference, add the declaration to dependencies
    void visit_ref(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        assert(node->data.is_ident());
        Identifier ident = node->data;
        xast::Node *decl = find_symbol_in_any(scope, *ident);
        if (decl) {
            // Don't add composite nodes as dependencies - they're not meta bindings
            // and references to them (via temp names like Self) are handled specially
            // in field/variant visitors
            if (decl->kind != xast::nk::composite) {
                deps.push_back({decl, current_site, false, indirection_depth > 0});
            }
        }
    }

    // For calls, collect the callable and all arguments
    void visit_call(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        dispatch(xast::c::call::callable(node), deps, scope);
        dispatch(xast::c::call::args(node), deps, scope);
    }

    // Binary/unary ops - traverse operands
    void visit_binary_op(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        dispatch(xast::c::binary_op::lhs(node), deps, scope);
        dispatch(xast::c::binary_op::rhs(node), deps, scope);
    }

    void visit_unary_op(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        // Check if this is a pointer or slice indirection
        bool is_indirection = false;
        if (node->data.is_op()) {
            Op op = node->data;
            if (op.kind == op::indirect || op.kind == op::slice) {
                is_indirection = true;
            }
        }
        
        // Track indirection depth
        if (is_indirection) {
            indirection_depth++;
        }
        dispatch(xast::c::unary_op::operand(node), deps, scope);
        if (is_indirection) {
            indirection_depth--;
        }
    }

    void visit_paren_expr(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        dispatch(xast::c::paren_expr::inner(node), deps, scope);
    }

    void visit_comma_expr(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), deps, scope);
        }
    }

    // For functions, we need to traverse the body with the function's scope
    void visit_func(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        Scope *func_scope = node->scope ? node->scope : scope;
        // Params are local to the function, so they shouldn't be external deps
        // But we do need to traverse captures for closures
        dispatch(xast::c::func::captures(node), deps, scope);
        // Traverse params in func_scope (they define symbols, not reference them)
        // Traverse return type
        dispatch(xast::c::func::return_ty(node), deps, scope);
        // Traverse body
        dispatch(xast::c::func::body(node), deps, func_scope);
    }

    void visit_block(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), deps, scope);
        }
    }

    void visit_bind(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        // If this is a nested meta binding, just record it as a dependency
        // and don't traverse into it - its dependencies will be collected separately
        if (node->meta) {
            deps.push_back({node, current_site, false, indirection_depth > 0});
            return;
        }
        dispatch(xast::c::bind::type_annot(node), deps, scope);
        dispatch(xast::c::bind::def(node), deps, scope);
    }

    void visit_branch(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        dispatch(xast::c::branch::cond(node), deps, scope);
        dispatch(xast::c::branch::then(node), deps, scope);
        dispatch(xast::c::branch::else_(node), deps, scope);
    }

    void visit_loop(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        dispatch(xast::c::loop::cond(node), deps, scope);
        dispatch(xast::c::loop::body(node), deps, scope);
    }

    void visit_composite(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        // Use the composite's scope if it has one (contains temp name)
        Scope *composite_scope = node->scope ? node->scope : scope;

        // Track this composite for detecting direct self-references in fields
        xast::Node *prev_composite = current_composite;
        current_composite = node;

        // Traverse layout (struct/union fields) - detect self-references here
        dispatch(xast::c::composite::layout(node), deps, composite_scope);
        
        // Clear composite tracking before traversing namespace (where clause)
        // Function signatures in where clause can reference Self without creating cycles
        current_composite = nullptr;
        dispatch(xast::c::composite::namespc(node), deps, composite_scope);

        current_composite = prev_composite;
    }

    void visit_struct_(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), deps, scope);
        }
    }

    void visit_union_(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), deps, scope);
        }
    }

    // Check if a type expression is a direct reference (not through pointer indirection)
    // to the current composite's temp name
    bool is_direct_self_reference(xast::Node *type_expr, Scope *scope) {
        if (!current_composite || !type_expr) return false;
        
        // Direct reference: just a ref node
        if (type_expr->kind == xast::nk::ref && type_expr->data.is_ident()) {
            Identifier ident = type_expr->data;
            xast::Node *decl = find_symbol_in_any(scope, *ident);
            return decl == current_composite;
        }
        
        // If it's a pointer type (*T), it's NOT a direct self-reference
        // (indirection breaks the cycle)
        if (type_expr->kind == xast::nk::unary_op && type_expr->data.is_op()) {
            Op op = type_expr->data;
            if (op.kind == op::indirect) {
                return false;  // Pointer indirection - not a direct self-ref
            }
        }
        
        return false;
    }

    void visit_field(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        xast::Node *type_annot = xast::c::field::type_annot(node);
        
        // Track current site for dependency tracking
        xast::Node *prev_site = current_site;
        current_site = node;
        
        // Check for direct self-reference (cycle)
        if (is_direct_self_reference(type_annot, scope)) {
            // Add the composite itself as a dependency - this will be detected as a self-cycle
            deps.push_back({current_composite, node, true, false});
        }
        
        dispatch(type_annot, deps, scope);
        current_site = prev_site;
    }

    void visit_variant(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        xast::Node *payload = xast::c::variant::payload(node);
        
        // Track current site for dependency tracking
        xast::Node *prev_site = current_site;
        current_site = node;
        
        // Check for direct self-reference (cycle)
        if (is_direct_self_reference(payload, scope)) {
            deps.push_back({current_composite, node, true, false});
        }
        
        dispatch(payload, deps, scope);
        current_site = prev_site;
    }

    void visit_cast(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        dispatch(xast::c::cast::expr(node), deps, scope);
        dispatch(xast::c::cast::type(node), deps, scope);
    }

    void visit_subscript(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        dispatch(xast::c::subscript::array(node), deps, scope);
        dispatch(xast::c::subscript::index(node), deps, scope);
    }

    void visit_param(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        dispatch(xast::c::param::type_annot(node), deps, scope);
    }

    void visit_params(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), deps, scope);
        }
    }

    void visit_captures(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {
        for (auto edge : node->opedges) {
            dispatch(edge->used(), deps, scope);
        }
    }

    // Literals have no dependencies
    void visit_int_lit(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {}
    void visit_str_lit(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {}
    void visit_char_lit(xast::Node *node, std::vector<DependencyInfo>& deps, Scope *scope) override {}
};

// ============================================================================
// Meta Topological Analysis
// ============================================================================
// Performs topological traversal over meta bindings, detecting:
// 1. Dependencies on runtime (non-meta) values
// 2. Cycles in meta dependencies

struct MetaTopologicalAnalysis {
    // Dependency info: maps a binding to its dependencies with source sites
    struct DepEdge {
        xast::Node *target;           // What we depend on
        xast::Node *site;             // Where the dependency comes from (field, etc.)
        bool is_self_reference;       // Is this a direct type self-containment?
        bool is_through_indirection;  // Is this through a pointer/slice? (safe for layout)
    };
    
    // Graph of meta binding dependencies
    std::unordered_map<xast::Node*, std::vector<DepEdge>> dependencies;
    
    // Track meta bindings in the order they're encountered
    std::vector<xast::Node*> meta_bindings;
    
    // For cycle detection
    enum class Color { White, Gray, Black };
    std::unordered_map<xast::Node*, Color> color;
    
    // Track the current path for cycle reporting (includes edge info)
    struct PathEdge {
        xast::Node *from;
        xast::Node *to;
        xast::Node *site;  // The site where 'from' depends on 'to'
    };
    std::vector<PathEdge> current_path;
    
    // Root scope
    Scope *global_scope = nullptr;
    
    // Dependency collector
    MetaDependencyCollector collector;
    
    // Helper to get node name for diagnostics
    std::string get_node_name(xast::Node *node) {
        if (node && node->data.is_ident()) {
            Identifier ident = node->data;
            return std::string(*ident);
        }
        return "<anonymous>";
    }
    
    // Get field/variant name
    std::string get_field_name(xast::Node *node) {
        if (!node) return "<unknown>";
        if (node->kind == xast::nk::field || node->kind == xast::nk::variant) {
            if (node->data.is_ident()) {
                Identifier ident = node->data;
                return std::string(*ident);
            }
        }
        return "<unknown>";
    }
    
    // Get the type reference name from a field/variant
    std::string get_type_ref_name(xast::Node *field_or_variant) {
        if (!field_or_variant) return "<unknown>";
        xast::Node *type_expr = nullptr;
        if (field_or_variant->kind == xast::nk::field) {
            type_expr = xast::c::field::type_annot(field_or_variant);
        } else if (field_or_variant->kind == xast::nk::variant) {
            type_expr = xast::c::variant::payload(field_or_variant);
        }
        if (type_expr && type_expr->kind == xast::nk::ref && type_expr->data.is_ident()) {
            Identifier ident = type_expr->data;
            return std::string(*ident);
        }
        return "<type>";
    }
    
    // Collect all meta bindings from the AST
    // Bindings whose definition is a meta composite are automatically treated as meta
    void collect_meta_bindings(xast::Node *node, Scope *scope) {
        if (!node) return;
        
        if (node->kind == xast::nk::bind) {
            // If the binding's definition is a meta composite, mark the binding as meta too
            xast::Node *def = xast::c::bind::def(node);
            if (!node->meta && def && def->kind == xast::nk::composite && def->meta) {
                node->meta = true;
            }
            if (node->meta) {
                meta_bindings.push_back(node);
            }
        }
        
        // Use the node's scope if it has one (for functions)
        Scope *child_scope = node->scope ? node->scope : scope;
        
        for (auto edge : node->opedges) {
            collect_meta_bindings(edge->used(), child_scope);
        }
    }
    
    // Build dependency graph for all meta bindings
    void build_dependency_graph(Scope *scope) {
        for (auto *binding : meta_bindings) {
            std::vector<DependencyInfo> raw_deps;
            
            // Get the scope for this binding
            Scope *bind_scope = scope;
            
            // Collect dependencies from the definition
            xast::Node *def = xast::c::bind::def(binding);
            if (def) {
                collector(def, raw_deps, bind_scope);
            }
            
            // Also check type annotation
            xast::Node *type_annot = xast::c::bind::type_annot(binding);
            if (type_annot) {
                collector(type_annot, raw_deps, bind_scope);
            }
            
            // Process raw dependencies
            std::vector<DepEdge> meta_deps;
            
            for (auto &dep_info : raw_deps) {
                xast::Node *dep = dep_info.target;
                
                // Check for self-references - this is a direct cycle!
                if (dep == binding) {
                    // Self-reference detected
                    continue;
                }
                
                // Check for composite self-references (type directly contains itself)
                if (dep->kind == xast::nk::composite && dep_info.is_self_reference) {
                    // This is a type that directly contains itself
                    // Find the composite's temp name and the binding's name
                    xast::Node *tmpname = xast::c::composite::tmpname(dep);
                    std::string temp_name = tmpname ? get_node_name(tmpname) : "";
                    std::string binding_name = get_node_name(binding);
                    
                    // Format: "BadNode" or "BadNode (as Me)" if temp name differs
                    std::string type_desc = binding_name;
                    if (!temp_name.empty() && temp_name != binding_name) {
                        type_desc += " (as " + temp_name + ")";
                    }
                    
                    // Error on the binding identifier
                    DiagnosticHandler::make(diag::id::meta_cycle_detected, binding->data.ident.sloc)
                        .add(type_desc)
                        .finish();
                    
                    // Note on the field/variant that causes it
                    if (dep_info.site) {
                        std::string field_name = get_field_name(dep_info.site);
                        // Get the source location of the type reference
                        xast::Node *type_ref = nullptr;
                        if (dep_info.site->kind == xast::nk::field) {
                            type_ref = xast::c::field::type_annot(dep_info.site);
                        } else if (dep_info.site->kind == xast::nk::variant) {
                            type_ref = xast::c::variant::payload(dep_info.site);
                        }
                        SourceLocation sloc = type_ref ? type_ref->sloc : dep_info.site->sloc;
                        DiagnosticHandler::make(diag::id::note_meta_cycle_node, sloc)
                            .add(*binding->data.ident)
                            .add(*binding->data.ident)
                            .finish();
                    }
                    continue;
                }
                
                // Skip parameters - they're local to the function
                if (dep->kind == xast::nk::param) continue;
                
                // Skip primitive types - they're built-in and don't need ordering
                // (Primitive types have their type field set during init)
                if (dep->type != nullptr) continue;
                
                // Only care about bind nodes for dependency tracking
                if (dep->kind != xast::nk::bind) continue;
                
                if (dep->meta) {
                    // Skip dependencies through indirection (pointers/slices)
                    // These are safe for layout and don't create cycles
                    if (dep_info.is_through_indirection) {
                        continue;
                    }
                    
                    // Avoid duplicates - only add if not already in list
                    bool found = false;
                    for (auto &existing : meta_deps) {
                        if (existing.target == dep) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        meta_deps.push_back({dep, dep_info.site, dep_info.is_self_reference, dep_info.is_through_indirection});
                    }
                } else {
                    // Non-meta binding - check if it's a top-level binding
                    // Local bindings inside functions are fine (they're not external deps)
                    bool is_top_level = false;
                    for (auto *mb : meta_bindings) {
                        if (mb == dep) {
                            is_top_level = true;
                            break;
                        }
                    }
                    
                    if (is_top_level) {
                        // This is a dependency on a top-level non-meta binding - error!
                        DiagnosticHandler::make(diag::id::meta_depends_on_runtime, binding->sloc)
                            .add(get_node_name(dep))
                            .finish();
                        DiagnosticHandler::make(diag::id::note_meta_dependency, dep->sloc)
                            .add(get_node_name(binding))
                            .finish();
                    }
                    // Local bindings (not in meta_bindings) are OK - they're function-local
                }
            }
            
            dependencies[binding] = std::move(meta_deps);
            color[binding] = Color::White;
        }
    }
    
    // DFS-based cycle detection and topological ordering
    // Returns false if a cycle is detected
    bool dfs_visit(xast::Node *node, std::vector<xast::Node*> &order, xast::Node *from_site = nullptr) {
        if (color[node] == Color::Black) {
            return true;  // Already processed
        }
        
        if (color[node] == Color::Gray) {
            // Cycle detected! 
            // Find where the cycle starts in the current path
            // The path contains edges {from, to, site}
            // We need to find the edge where 'from' == node (the start of the cycle)
            size_t cycle_start = 0;
            for (size_t i = 0; i < current_path.size(); i++) {
                if (current_path[i].from == node) {
                    cycle_start = i;
                    break;
                }
            }
            
            // Build the cycle description: node1 -> node2 -> ... -> node1
            std::stringstream cycle_str;
            cycle_str << get_node_name(node);  // Start with the cycle entry point
            for (size_t i = cycle_start; i < current_path.size(); i++) {
                cycle_str << " -> " << get_node_name(current_path[i].to);
            }
            // Note: current_path.back().to should already be pointing to node since that's how we got here
            // But we've already added it above, so no need to add again
            
            // Emit main error on the identifier of the first node in cycle
            DiagnosticHandler::make(diag::id::meta_cycle_detected, node->data.ident.sloc)
                .add(cycle_str.str())
                .finish();
            
            // Emit notes for each edge in the cycle, pointing to dependency sites
            for (size_t i = cycle_start; i < current_path.size(); i++) {
                xast::Node *site = current_path[i].site;
                xast::Node *from_node = current_path[i].from;
                xast::Node *to_node = current_path[i].to;

                SourceLocation site_loc;
                if (site == nullptr) {
                    // No specific site - use the 'to' node's location
                    site_loc = to_node->sloc;
                } else if (site->kind == xast::nk::field) {
                    site_loc = xast::c::field::type_annot(site)->sloc;
                } else if (site->kind == xast::nk::variant) {
                    site_loc = xast::c::variant::payload(site)->sloc;
                } else {
                    site_loc = site->sloc;
                }
                
                // Generic dependency note: 'from' depends on 'to'
                DiagnosticHandler::make(diag::id::note_meta_cycle_node, site_loc)
                    .add(get_node_name(from_node))
                    .add(get_node_name(to_node))
                    .finish();
            }
            
            // Mark all nodes in the cycle as Black to prevent re-reporting
            for (size_t i = cycle_start; i < current_path.size(); i++) {
                color[current_path[i].from] = Color::Black;
            }
            
            return false;
        }
        
        color[node] = Color::Gray;
        
        for (auto &dep_edge : dependencies[node]) {
            // Only visit if it's a meta binding we're tracking
            if (dependencies.find(dep_edge.target) != dependencies.end()) {
                // Record path edge before recursing
                current_path.push_back({node, dep_edge.target, dep_edge.site});
                if (!dfs_visit(dep_edge.target, order, dep_edge.site)) {
                    current_path.pop_back();
                    return false;
                }
                current_path.pop_back();
            }
        }
        
        color[node] = Color::Black;
        order.push_back(node);
        return true;
    }
    
    // Perform topological sort and return ordered list
    // Returns empty optional if cycle detected
    std::optional<std::vector<xast::Node*>> topological_sort() {
        std::vector<xast::Node*> order;
        order.reserve(meta_bindings.size());
        
        bool has_cycle = false;
        for (auto *binding : meta_bindings) {
            if (color[binding] == Color::White) {
                if (!dfs_visit(binding, order)) {
                    has_cycle = true;
                    // Continue to find all cycles, not just the first one
                }
            }
        }
        
        if (has_cycle) {
            return std::nullopt;
        }
        return order;
    }
    
    // Main entry point
    std::optional<std::vector<xast::Node*>> analyze(xast::Node *root, Scope *scope) {
        global_scope = scope;
        
        // Collect all meta bindings
        collect_meta_bindings(root, scope);
        
        std::cout << "Meta bindings found: " << meta_bindings.size() << "\n";
        for (auto *b : meta_bindings) {
            std::cout << "  - " << get_node_name(b) << "\n";
        }
        
        // Build dependency graph
        build_dependency_graph(scope);
        
        // Print dependencies for debugging
        std::cout << "Meta dependencies:\n";
        for (auto &[node, deps] : dependencies) {
            std::cout << "  " << get_node_name(node) << " depends on: ";
            for (auto &dep : deps) {
                std::cout << get_node_name(dep.target) << " ";
            }
            std::cout << "\n";
        }
        
        // Perform topological sort
        auto result = topological_sort();
        
        if (result) {
            std::cout << "Topological order:\n";
            for (auto *node : *result) {
                std::cout << "  " << get_node_name(node) << "\n";
            }
        }
        
        return result;
    }
};

// ============================================================================
// Meta Instantiation Pass
// ============================================================================
// Processes meta bindings in topological order (leaves first).
// For each binding:
// - If it's a composite type (no parameters), create the concrete Type
// - If it's a meta function (fn! with parameters), mark it as a type constructor
//   that will be instantiated when called

struct MetaInstantiator {
    Scope *global_scope;
    std::unordered_map<std::string, Type *> &type_cache;
    
    MetaInstantiator(Scope *scope, std::unordered_map<std::string, Type *> &cache)
        : global_scope(scope), type_cache(cache) {}
    
    // Get the name of a binding node
    std::string get_binding_name(xast::Node *binding) {
        if (binding->data.is_ident()) {
            return std::string(*binding->data.ident);
        }
        return "<anonymous>";
    }
    
    // Check if a meta binding is a type constructor (fn! that returns type)
    bool is_type_constructor(xast::Node *binding) {
        xast::Node *def = xast::c::bind::def(binding);
        if (!def) return false;
        
        // Check if the definition is a meta function
        if (def->kind == xast::nk::func && def->meta) {
            // Check if it has parameters (type constructors have params)
            xast::Node *params = xast::c::func::params(def);
            if (params && params->opedges.size() > 0) {
                return true;
            }
        }
        return false;
    }
    
    // Check if a binding is a direct composite type (no parameters)
    // Works for both meta and non-meta bindings
    bool is_direct_composite(xast::Node *binding) {
        xast::Node *def = xast::c::bind::def(binding);
        if (!def) return false;
        
        // Could be a composite directly: let Foo = type { ... } or let! Foo = type { ... }
        if (def->kind == xast::nk::composite) {
            return true;
        }
        
        // Could be fn!(): type { composite } - no params, returns composite (meta only)
        if (def->kind == xast::nk::func && def->meta) {
            xast::Node *params = xast::c::func::params(def);
            if (!params || params->opedges.size() == 0) {
                // No parameters - check if body contains composite
                xast::Node *body = xast::c::func::body(def);
                if (body && body->kind == xast::nk::block && body->opedges.size() == 1) {
                    xast::Node *inner = body->opedges[0]->used();
                    if (inner && inner->kind == xast::nk::composite) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
    
    // Get the composite node from a binding (handles both direct composite and fn!(): composite)
    xast::Node *get_composite_node(xast::Node *binding) {
        xast::Node *def = xast::c::bind::def(binding);
        if (!def) return nullptr;
        
        if (def->kind == xast::nk::composite) {
            return def;
        }
        
        if (def->kind == xast::nk::func && def->meta) {
            xast::Node *body = xast::c::func::body(def);
            if (body && body->kind == xast::nk::block && body->opedges.size() == 1) {
                xast::Node *inner = body->opedges[0]->used();
                if (inner && inner->kind == xast::nk::composite) {
                    return inner;
                }
            }
        }
        return nullptr;
    }
    
    // Create a Type for a direct composite definition (not a template instantiation)
    // Creates a StructType or UnionType with the binding's name
    Type *instantiate_composite(xast::Node *binding, xast::Node *composite) {
        std::string name = get_binding_name(binding);
        
        // Determine if struct or union based on layout
        xast::Node *layout = xast::c::composite::layout(composite);
        Type *ty = nullptr;
        if (layout && layout->kind == xast::nk::struct_) {
            ty = new StructType(composite, name);
        } else if (layout && layout->kind == xast::nk::union_) {
            ty = new UnionType(composite, name);
        } else {
            // Default to struct if unknown
            ty = new StructType(composite, name);
        }
        
        // Add to type cache so it can be looked up by name
        type_cache[name] = ty;
        
        // Attach type to both the binding and composite nodes
        // The composite type is needed so Self can resolve correctly
        binding->type = ty;
        composite->type = ty;
        std::cout << "  Created concrete type: " << name << "\n";
        
        return ty;
    }
    
    // Check if a binding is a type alias (call to type constructor, NOT an intrinsic)
    bool is_type_alias(xast::Node *binding) {
        xast::Node *def = xast::c::bind::def(binding);
        if (!def) return false;
        
        // let! Alias = TypeConstructor!(args)
        // The binding itself is meta, and def is a call
        if (def->kind != xast::nk::call) return false;
        
        // Check that it's not an intrinsic call
        xast::Node *callable = xast::c::call::callable(def);
        if (callable && callable->kind == xast::nk::ref && callable->data.is_ident()) {
            std::string name = std::string(*callable->data.ident);
            // Intrinsics are not type aliases
            if (name == "_is_pointer" || name == "_is_struct" || name == "_is_union" ||
                name == "_is_array" || name == "_is_function" || name == "_is_primitive" ||
                name == "_is_integral" || name == "_is_signed" || name == "_is_unsigned" ||
                name == "_sizeof" || name == "_alignof" || name == "_type_name") {
                return false;
            }
        }
        
        return true;
    }
    
    // Resolve a type expression node to a Type*
    // Handles refs (to cached types), calls (nested type constructors), etc.
    Type *resolve_type_expr(xast::Node *expr) {
        if (!expr) return nullptr;
        
        if (expr->kind == xast::nk::ref && expr->data.is_ident()) {
            // Simple reference to a type name
            std::string type_name(*expr->data.ident);
            auto it = type_cache.find(type_name);
            if (it != type_cache.end()) {
                return it->second;
            }
            // Type not found in cache - might be a forward reference or error
            return nullptr;
        }
        
        if (expr->kind == xast::nk::call) {
            // Nested type constructor call: e.g., Option!(i32)
            xast::Node *callable = xast::c::call::callable(expr);
            xast::Node *args = xast::c::call::args(expr);
            
            std::string constructor_name = "<unknown>";
            if (callable && callable->kind == xast::nk::ref && callable->data.is_ident()) {
                constructor_name = std::string(*callable->data.ident);
            }
            
            // Recursively resolve argument types
            std::vector<Type *> arg_types;
            if (args) {
                if (args->kind == xast::nk::comma_expr) {
                    for (auto edge : args->opedges) {
                        Type *arg_ty = resolve_type_expr(edge->used());
                        if (arg_ty) arg_types.push_back(arg_ty);
                    }
                } else {
                    Type *arg_ty = resolve_type_expr(args);
                    if (arg_ty) arg_types.push_back(arg_ty);
                }
            }
            
            // Create an InstantiatedType for this nested call
            return new InstantiatedType(constructor_name, arg_types, expr);
        }
        
        // Other type expressions (pointers, arrays, etc.) - not handled yet
        return nullptr;
    }
    
    // Create a placeholder type for a type alias
    // The actual instantiation happens when the type constructor is called
    Type *create_type_alias(xast::Node *binding) {
        std::string name = get_binding_name(binding);
        xast::Node *def = xast::c::bind::def(binding);
        
        // Get the call expression: TypeConstructor!(args)
        xast::Node *callable = xast::c::call::callable(def);
        xast::Node *args = xast::c::call::args(def);
        
        // Get the type constructor name
        std::string constructor_name = "<unknown>";
        if (callable && callable->kind == xast::nk::ref && callable->data.is_ident()) {
            constructor_name = std::string(*callable->data.ident);
        }
        
        // Collect argument types using recursive resolution
        std::vector<Type *> arg_types;
        if (args) {
            if (args->kind == xast::nk::comma_expr) {
                for (auto edge : args->opedges) {
                    Type *arg_ty = resolve_type_expr(edge->used());
                    if (arg_ty) arg_types.push_back(arg_ty);
                }
            } else {
                Type *arg_ty = resolve_type_expr(args);
                if (arg_ty) arg_types.push_back(arg_ty);
            }
        }
        
        // Create an InstantiatedType to represent this alias
        Type *ty = new InstantiatedType(constructor_name, arg_types, def);
        binding->type = ty;
        type_cache[name] = ty;
        
        std::cout << "  Created type alias: " << name << " = " << ty->stringify() << "\n";
        return ty;
    }
    
    // Evaluate a type alias by calling the type constructor
    // This creates a concrete type by evaluating the meta function body
    // 
    // Key concept: Nominal vs Canonical types
    // - Nominal type: The user-given name (e.g., "IntOptVec") - for diagnostics/namespaces
    // - Canonical type: Fully resolved form (e.g., "Vector!(Option!(i32))") - for type checking/mangling
    //
    // When a binding like `let! IntOptVec = Vector!(Option!(i32))` is evaluated:
    // - We first create the canonical InstantiatedType with canonical Type* args
    // - Then create an AliasType for "IntOptVec" pointing to that canonical type
    Type *evaluate_type_alias(xast::Node *binding) {
        std::string nominal_name = get_binding_name(binding);
        xast::Node *def = xast::c::bind::def(binding);
        
        // Create a constant evaluator to evaluate the call
        ConstantExprEvaluator evaluator(type_cache);
        
        // Evaluate the call expression
        ConstValue result = evaluator.dispatch(def, global_scope);
        
        if (result.is_type()) {
            // Got a concrete type - create an alias if the name differs
            Type *canonical_ty = result.type_val->get_canonical();
            std::string canonical_name = canonical_ty->stringify();
            
            if (nominal_name != canonical_name) {
                // Create an alias: nominal name -> canonical type
                Type *alias_ty = new AliasType(binding, canonical_ty, nominal_name);
                binding->type = alias_ty;
                type_cache[nominal_name] = alias_ty;
                std::cout << "  Evaluated type alias: " << nominal_name << " = " << canonical_name << "\n";
                return alias_ty;
            } else {
                binding->type = canonical_ty;
                type_cache[nominal_name] = canonical_ty;
                std::cout << "  Evaluated type alias: " << nominal_name << " = " << canonical_name << "\n";
                return canonical_ty;
            }
        } else if (result.is_node()) {
            // Got a composite node - create an InstantiatedType
            xast::Node *composite = result.node_val;
            if (composite && composite->kind == xast::nk::composite) {
                // Update the binding's definition to point to the cloned composite
                if (binding->opedges.size() > 1) {
                    binding->opedges[1]->set(composite);
                }
                
                // Create canonical InstantiatedType if we have instantiation info
                Type *canonical_ty = nullptr;
                std::string canonical_name;
                
                if (result.has_instantiation_info()) {
                    // Create InstantiatedType with canonical Type* args
                    canonical_ty = get_or_create_instantiated_type(
                        result.template_name, result.template_args, composite);
                    canonical_name = canonical_ty->stringify();
                } else {
                    // No instantiation info - this is a direct composite definition
                    // Create an InstantiatedType with empty args (or just use nominal name)
                    canonical_name = nominal_name;
                    canonical_ty = get_or_create_instantiated_type(nominal_name, {}, composite);
                }
                
                // If nominal name differs from canonical, create an alias
                if (nominal_name != canonical_name) {
                    Type *alias_ty = new AliasType(binding, canonical_ty, nominal_name);
                    binding->type = alias_ty;
                    type_cache[nominal_name] = alias_ty;
                    std::cout << "  Instantiated type alias: " << nominal_name << " -> " << canonical_name << "\n";
                    return alias_ty;
                } else {
                    binding->type = canonical_ty;
                    type_cache[nominal_name] = canonical_ty;
                    std::cout << "  Instantiated type: " << canonical_name << "\n";
                    return canonical_ty;
                }
            }
        }
        
        // Fall back to creating an InstantiatedType placeholder
        return create_type_alias(binding);
    }
    
    // Cache for interned InstantiatedTypes
    std::unordered_map<std::string, InstantiatedType *> instantiated_type_cache;
    
    // Get or create an InstantiatedType with canonical args, ensuring interning
    // This is the primary way to create canonical composite types
    Type *get_or_create_instantiated_type(const std::string &template_name,
                                           const std::vector<Type *> &args,
                                           xast::Node *composite) {
        // Build canonical key with canonical arg types
        std::string key = template_name;
        if (!args.empty()) {
            key += "!(";
            for (size_t i = 0; i < args.size(); ++i) {
                Type *arg = args[i];
                Type *canonical_arg = arg ? arg->get_canonical() : nullptr;
                if (i > 0) key += ", ";
                key += canonical_arg ? canonical_arg->stringify() : "?";
            }
            key += ")";
        }
        
        // Check if already interned
        auto it = instantiated_type_cache.find(key);
        if (it != instantiated_type_cache.end()) {
            return it->second;
        }
        
        // Build canonical args vector
        std::vector<Type *> canonical_args;
        for (auto *arg : args) {
            canonical_args.push_back(arg ? arg->get_canonical() : nullptr);
        }
        
        // Create new InstantiatedType
        InstantiatedType *ty = new InstantiatedType(template_name, canonical_args, composite);
        instantiated_type_cache[key] = ty;
        type_cache[key] = ty;
        
        return ty;
    }
    
    // Instantiate a composite node to create a concrete type
    // This is for direct composite definitions (not template instantiations)
    Type *instantiate_composite_node(xast::Node *binding, xast::Node *composite, const std::string &name) {
        // For direct composites, create an InstantiatedType with no template args
        Type *ty = get_or_create_instantiated_type(name, {}, composite);
        
        // Attach type to both the binding and composite nodes
        // The composite type is needed so Self can resolve correctly
        binding->type = ty;
        composite->type = ty;
        std::cout << "  Instantiated composite type: " << name << "\n";
        
        return ty;
    }
    
    // Check if a meta binding is a constant value (not a type)
    bool is_constant_binding(xast::Node *binding) {
        xast::Node *def = xast::c::bind::def(binding);
        if (!def) return false;
        
        // If it's a literal, it's a constant
        if (def->kind == xast::nk::int_lit || 
            def->kind == xast::nk::char_lit || 
            def->kind == xast::nk::str_lit) {
            return true;
        }
        
        // If it's a binary/unary op that's not a type constructor, it might be a constant
        if (def->kind == xast::nk::binary_op || def->kind == xast::nk::unary_op) {
            // Check if it looks like a type expression (pointer, slice types)
            if (def->kind == xast::nk::unary_op && def->data.is_op()) {
                Op op = def->data;
                if (op.kind == op::indirect || op.kind == op::slice) {
                    return false;  // Probably a type expression
                }
            }
            return true;
        }
        
        // Check for intrinsic calls that return constants
        if (def->kind == xast::nk::call) {
            xast::Node *callable = xast::c::call::callable(def);
            if (callable && callable->kind == xast::nk::ref && callable->data.is_ident()) {
                std::string name = std::string(*callable->data.ident);
                // These intrinsics return integers, not types
                if (name == "_is_pointer" || name == "_is_struct" || name == "_is_union" ||
                    name == "_is_array" || name == "_is_function" || name == "_is_primitive" ||
                    name == "_is_integral" || name == "_is_signed" || name == "_is_unsigned" ||
                    name == "_sizeof" || name == "_alignof") {
                    return true;
                }
            }
        }
        
        return false;
    }
    
    // Evaluate a constant binding and set its type
    void evaluate_constant(xast::Node *binding) {
        std::string name = get_binding_name(binding);
        
        ConstantExprEvaluator evaluator(type_cache);
        ConstValue result = evaluator.dispatch(binding, global_scope);
        
        if (result.is_integer()) {
            std::cout << "  Evaluated constant: " << name << " = " << result.int_val << "\n";
            // Set the type to i64 for integer constants (or infer based on value range)
            Type *int_ty = type_cache.count("i64") ? type_cache["i64"] : 
                          (type_cache.count("i32") ? type_cache["i32"] : nullptr);
            if (int_ty) {
                binding->type = int_ty;
            }
        } else if (result.is_string()) {
            std::cout << "  Evaluated constant: " << name << " = \"" << result.str_val << "\"\n";
            // String type - leave as is for now
        }
    }
    
    // Process a single meta binding
    void process_binding(xast::Node *binding) {
        std::string name = get_binding_name(binding);
        
        // Skip if already has a type (e.g., primitive types)
        if (binding->type != nullptr) {
            std::cout << "  Skipping " << name << " (already has type)\n";
            return;
        }
        
        if (is_type_constructor(binding)) {
            // This is a type constructor (fn! with parameters)
            // Mark it with MetaType to indicate it produces types when called
            binding->type = MetaType::get();
            std::cout << "  Registered type constructor: " << name << "\n";
        } else if (is_direct_composite(binding)) {
            // Direct composite - instantiate it
            xast::Node *composite = get_composite_node(binding);
            if (composite) {
                instantiate_composite(binding, composite);
            }
        } else if (is_type_alias(binding)) {
            // Type alias - evaluate and create concrete type
            evaluate_type_alias(binding);
        } else if (is_constant_binding(binding)) {
            // Constant value - evaluate it
            evaluate_constant(binding);
        } else {
            // Other meta binding
            std::cout << "  Processing other meta binding: " << name << "\n";
            // Try to evaluate it
            ConstantExprEvaluator evaluator(type_cache);
            ConstValue result = evaluator.dispatch(binding, global_scope);
            if (result.is_type()) {
                binding->type = result.type_val;
                type_cache[name] = result.type_val;
                std::cout << "    Resolved to type: " << result.type_val->stringify() << "\n";
            }
        }
    }
    
    // Process all bindings in topological order
    void instantiate(const std::vector<xast::Node*> &topo_order) {
        std::cout << "\nInstantiating meta bindings (leaves first):\n";
        
        // The topological order is already leaves-first (dependencies before dependents)
        for (xast::Node *binding : topo_order) {
            process_binding(binding);
        }
        
        std::cout << "\nInstantiation complete.\n";
    }
    
    // Lift composite names: for type aliases like `let! Foo = Bar!(i32)`,
    // update the composite's tmpname to reference "Foo" instead of "Self"
    void lift_composite_names(const std::vector<xast::Node*> &topo_order) {
        std::cout << "\n=== Lifting Composite Names ===\n";
        
        for (xast::Node *binding : topo_order) {
            if (!binding || binding->kind != xast::nk::bind) continue;
            if (!binding->data.is_ident()) continue;
            
            std::string name(*binding->data.ident);
            
            // Get the definition (second operand of bind)
            xast::Node *def = xast::c::bind::def(binding);
            if (!def) continue;
            
            // If the definition is a composite, update its tmpname
            if (def->kind == xast::nk::composite) {
                xast::Node *tmpname = xast::c::composite::tmpname(def);
                
                // Check if tmpname is different from binding name
                std::string current_name = "";
                if (tmpname && tmpname->kind == xast::nk::ref && tmpname->data.is_ident()) {
                    current_name = std::string(*tmpname->data.ident);
                }
                
                if (current_name != name) {
                    // Create a new ref node with the binding's name
                    // Re-use the tmpname node if it exists, just update its identifier
                    if (tmpname && tmpname->kind == xast::nk::ref) {
                        // Copy the identifier from the binding to the tmpname
                        tmpname->data = binding->data;
                        std::cout << "  Lifted " << (current_name.empty() ? "<anon>" : current_name) 
                                  << " -> " << name << "\n";
                    }
                }
            }
        }
        
        std::cout << "Name lifting complete.\n";
    }
};

// ============================================================================
// AST Simplification Pass
// ============================================================================
// Simplifies the AST by:
// 1. Replacing meta if!/while! with their evaluated branches
// 2. Replacing meta function calls with their results
// 3. Removing meta bindings that have been fully evaluated
// 4. Converting type expressions to concrete type references

struct ASTSimplifier {
    std::unordered_map<std::string, Type *> &type_cache;
    ConstantExprEvaluator &evaluator;
    // Store strings to keep them alive for string_view Identifiers
    std::vector<std::unique_ptr<std::string>> string_pool;
    
    ASTSimplifier(std::unordered_map<std::string, Type *> &cache, ConstantExprEvaluator &eval)
        : type_cache(cache), evaluator(eval) {}
    
    // Allocate a string that will live as long as this simplifier
    std::string_view intern_string(std::string s) {
        auto ptr = std::make_unique<std::string>(std::move(s));
        std::string_view view = *ptr;
        string_pool.push_back(std::move(ptr));
        return view;
    }
    
    // Simplify a node and return the simplified node (or nullptr if should be removed)
    xast::Node *simplify(xast::Node *node, Scope *scope) {
        if (!node) return nullptr;
        
        // Handle meta branches (if!)
        if (node->kind == xast::nk::branch && node->meta) {
            return simplify_meta_branch(node, scope);
        }
        
        // Handle meta loops (while!)
        if (node->kind == xast::nk::loop && node->meta) {
            return simplify_meta_loop(node, scope);
        }
        
        // Handle meta calls
        if (node->kind == xast::nk::call && node->meta) {
            return simplify_meta_call(node, scope);
        }
        
        // Handle intrinsic calls (even without meta flag)
        if (node->kind == xast::nk::call) {
            xast::Node *simplified = try_simplify_intrinsic_call(node, scope);
            if (simplified != node) {
                return simplified;
            }
        }
        
        // Recursively simplify children
        for (auto edge : node->opedges) {
            xast::Node *child = edge->used();
            if (child) {
                xast::Node *simplified = simplify(child, scope);
                if (simplified != child) {
                    edge->set(simplified);
                }
            }
        }
        
        return node;
    }
    
    // Try to simplify an intrinsic call
    xast::Node *try_simplify_intrinsic_call(xast::Node *node, Scope *scope) {
        xast::Node *callable = xast::c::call::callable(node);
        if (!callable || callable->kind != xast::nk::ref || !callable->data.is_ident()) {
            return node;
        }
        
        std::string name = std::string(*callable->data.ident);
        
        // Check if this is an intrinsic
        if (name == "_is_pointer" || name == "_is_struct" || name == "_is_union" ||
            name == "_is_array" || name == "_is_function" || name == "_is_primitive" ||
            name == "_is_integral" || name == "_is_signed" || name == "_is_unsigned" ||
            name == "_sizeof" || name == "_alignof" || name == "_type_name") {
            
            ConstValue result = evaluator.dispatch(node, scope);
            
            if (result.is_integer()) {
                xast::Node *lit = new xast::Node(xast::nk::int_lit);
                lit->data = static_cast<uint64_t>(result.int_val);
                lit->meta = true;
                lit->sloc = node->sloc;
                return lit;
            }
            
            if (result.is_string()) {
                xast::Node *lit = new xast::Node(xast::nk::str_lit);
                std::string_view str = intern_string(result.str_val);
                lit->data = Identifier{ str, node->sloc };
                lit->meta = true;
                lit->sloc = node->sloc;
                return lit;
            }
        }
        
        return node;
    }
    
    xast::Node *simplify_meta_branch(xast::Node *node, Scope *scope) {
        // Evaluate the condition
        ConstValue cond = evaluator.dispatch(xast::c::branch::cond(node), scope);
        
        if (cond.is_none() || cond.is_error()) {
            // Can't evaluate - leave as is
            return node;
        }
        
        // Select the appropriate branch
        xast::Node *selected;
        if (cond.is_truthy()) {
            selected = xast::c::branch::then(node);
        } else {
            selected = xast::c::branch::else_(node);
        }
        
        if (selected) {
            // Recursively simplify the selected branch
            return simplify(selected, scope);
        }
        
        // No selected branch (e.g., false condition with no else)
        // Return a null node
        return new xast::Node(xast::nk::null);
    }
    
    xast::Node *simplify_meta_loop(xast::Node *node, Scope *scope) {
        // For meta loops, we'd need to unroll them
        // For now, just evaluate and check if we can simplify
        ConstValue result = evaluator.dispatch(node, scope);
        
        if (result.is_node()) {
            return result.node_val;
        }
        
        // Can't simplify - leave as is (or could unroll)
        return node;
    }
    
    xast::Node *simplify_meta_call(xast::Node *node, Scope *scope) {
        // Evaluate the call
        ConstValue result = evaluator.dispatch(node, scope);
        
        if (result.is_type()) {
            // The call produced a type - create an internalref node with the type set
            xast::Node *internal_ref = new xast::Node(xast::nk::internalref);
            internal_ref->meta = true;
            internal_ref->sloc = node->sloc;
            internal_ref->type = result.type_val;
            return internal_ref;
        }
        
        if (result.is_node()) {
            // If the node has an InstantiatedType, create an internalref instead
            // This handles type constructor calls like Array!(i32, 6)
            if (result.node_val && result.node_val->type && 
                result.node_val->type->get_kind() == typekind::instantiated_t) {
                xast::Node *internal_ref = new xast::Node(xast::nk::internalref);
                internal_ref->meta = true;
                internal_ref->sloc = node->sloc;
                internal_ref->type = result.node_val->type;
                return internal_ref;
            }
            return result.node_val;
        }
        
        if (result.is_integer() || result.is_string()) {
            // Create a literal node
            xast::Node *lit = new xast::Node(
                result.is_integer() ? xast::nk::int_lit : xast::nk::str_lit);
            if (result.is_integer()) {
                lit->data = static_cast<uint64_t>(result.int_val);
            } else {
                std::string_view str = intern_string(result.str_val);
                lit->data = Identifier{ str, node->sloc };
            }
            lit->meta = true;
            return lit;
        }
        
        return node;
    }
    
    // Check if a meta binding should be removed from the AST
    // Pure type constructors (fn! returning type) should be removed - they're compile-time only
    // Concrete type definitions (struct/union) should be kept
    bool should_remove_meta_binding(xast::Node *binding) {
        if (!binding || !binding->meta) return false;
        
        xast::Node *def = xast::c::bind::def(binding);
        if (!def) return false;
        
        // Type constructors (fn! with type return) should be removed
        // They have MetaType as their type and are only used during meta evaluation
        if (def->kind == xast::nk::func && def->meta) {
            // Check if this is a pure type constructor (returns type, has no runtime effect)
            // These have MetaType as their binding type
            if (binding->type == MetaType::get()) {
                return true;  // Remove pure type constructors
            }
        }
        
        // Fully evaluated type aliases (now have a concrete type) can be removed
        if (binding->type != nullptr && binding->type != MetaType::get()) {
            // Check if it's an instantiated type like IntOptVec
            if (binding->type->get_kind() == typekind::struct_t ||
                binding->type->get_kind() == typekind::union_t) {
                return false; // Keep concrete type definitions
            }
        }
        
        return false; // Default: don't remove
    }
    
    // Clean up the prog node by processing meta bindings
    void cleanup_meta_bindings(xast::Node *prog, Scope *scope) {
        if (!prog || prog->kind != xast::nk::prog) return;
        
        // Collect bindings to remove
        std::vector<size_t> to_remove;
        
        for (size_t i = 0; i < prog->opedges.size(); i++) {
            xast::Node *child = prog->opedges[i]->used();
            if (child && child->kind == xast::nk::bind && child->meta) {
                if (should_remove_meta_binding(child)) {
                    to_remove.push_back(i);
                } else {
                    // Simplify the definition of meta bindings we're keeping
                    xast::Node *def = xast::c::bind::def(child);
                    if (def) {
                        xast::Node *simplified_def = simplify(def, scope);
                        if (simplified_def != def) {
                            // Update the binding's definition
                            // The def is at opedges[1] for bind nodes
                            if (child->opedges.size() > 1) {
                                child->opedges[1]->set(simplified_def);
                            }
                        }
                    }
                }
            }
        }
        
        // Remove bindings in reverse order to keep indices valid
        for (auto it = to_remove.rbegin(); it != to_remove.rend(); ++it) {
            xast::Node *binding = prog->opedges[*it]->used();
            std::string name = "<unknown>";
            if (binding && binding->data.is_ident()) {
                name = std::string(*binding->data.ident);
            }
            std::cout << "  Removing type constructor: " << name << "\n";
            prog->opedges.erase(prog->opedges.begin() + *it);
        }
    }
    
    // Run simplification on the entire AST
    void simplify_ast(xast::Node *root, Scope *scope) {
        std::cout << "\n=== AST Simplification ===\n";
        
        // First, resolve type annotations that are meta calls
        // This must happen before we remove meta functions
        resolve_type_annotation_calls(root, scope);
        
        simplify(root, scope);
        cleanup_meta_bindings(root, scope);
        
        // Inline top-level meta expressions that simplified to blocks
        inline_blocks_in_container(root, scope);
        
        std::cout << "AST simplification complete.\n";
    }
    
    // Resolve type annotation calls (like `specialized!(u16, dispatch_fn)`)
    // This must happen before meta functions are removed
    void resolve_type_annotation_calls(xast::Node *root, Scope *scope) {
        if (!root || root->kind != xast::nk::prog) return;
        
        for (auto edge : root->opedges) {
            xast::Node *child = edge->used();
            if (child && child->kind == xast::nk::bind) {
                resolve_binding_type_annotation(child, scope);
            }
        }
    }
    
    // Resolve a binding's type annotation if it's a meta call
    void resolve_binding_type_annotation(xast::Node *binding, Scope *scope) {
        // Only process non-meta bindings - meta bindings are already processed
        if (binding->meta) return;
        
        xast::Node *type_annot = xast::c::bind::type_annot(binding);
        if (!type_annot) return;
        
        // Check if the type annotation is a call (meta call)
        if (type_annot->kind == xast::nk::call) {
            // Evaluate the call to get the type
            ConstValue result = evaluator.dispatch(type_annot, scope);
            
            if (result.is_type()) {
                // Replace the type annotation with a reference to the resolved type
                xast::Node *type_ref = new xast::Node(xast::nk::ref);
                std::string_view name = intern_string(result.type_val->stringify());
                type_ref->data = Identifier{ name, type_annot->sloc };
                type_ref->meta = true;
                type_ref->type = result.type_val;
                type_ref->sloc = type_annot->sloc;
                
                // Replace the type annotation (opedges[0] for bind nodes)
                if (binding->opedges.size() > 0) {
                    binding->opedges[0]->set(type_ref);
                }
            } else if (result.is_node() && result.node_val && result.node_val->kind == xast::nk::composite) {
                // Got a composite - create an InstantiatedType for it
                xast::Node *composite = result.node_val;
                
                Type *ty = nullptr;
                if (result.has_instantiation_info()) {
                    // Create InstantiatedType with proper template args
                    ty = evaluator.get_or_create_instantiated_type(
                        result.template_name, result.template_args, composite);
                } else {
                    // Anonymous composite - create InstantiatedType with no args
                    ty = evaluator.get_or_create_instantiated_type("<anonymous>", {}, composite);
                }
                
                if (ty) {
                    // Replace the type annotation with a composite reference
                    composite->type = ty;
                    if (binding->opedges.size() > 0) {
                        binding->opedges[0]->set(composite);
                    }
                }
            }
        }
    }
    
    // Inline blocks that appear directly in a non-execution scope container
    // This handles cases like top-level if!/while! that evaluate to blocks containing bindings
    void inline_blocks_in_container(xast::Node *container, Scope *scope) {
        if (!container) return;
        
        // Only process prog nodes and blocks (for where-clauses)
        if (container->kind != xast::nk::prog && container->kind != xast::nk::block) return;
        
        // We need to rebuild the edges list since we might be expanding some items
        std::vector<xast::Node *> new_children;
        bool changed = false;
        
        for (auto edge : container->opedges) {
            xast::Node *child = edge->used();
            if (!child) continue;
            
            // If the child is a block that's been simplified from a meta expression,
            // inline its contents
            if (child->kind == xast::nk::block) {
                // Inline the block's children
                for (auto block_edge : child->opedges) {
                    xast::Node *block_child = block_edge->used();
                    if (block_child) {
                        new_children.push_back(block_child);
                        // Index any new bindings
                        if (block_child->kind == xast::nk::bind && block_child->data.is_ident()) {
                            maybe_insert_symbol(scope, block_child);
                        }
                    }
                }
                changed = true;
            } else {
                new_children.push_back(child);
            }
            
            // Also recursively check composites for where-clauses
            if (child->kind == xast::nk::bind) {
                xast::Node *def = xast::c::bind::def(child);
                if (def && def->kind == xast::nk::composite) {
                    xast::Node *namespc = xast::c::composite::namespc(def);
                    if (namespc) {
                        inline_blocks_in_container(namespc, scope);
                    }
                }
            }
        }
        
        if (changed) {
            // Clear and rebuild edges
            container->opedges.clear();
            for (xast::Node *child : new_children) {
                container->add(child);
            }
        }
    }
};

// ============================================================================
// Type Checker Pass
// ============================================================================
// Performs concrete type checking on value expressions after meta simplification.
// This pass uses bidirectional type inference:
// 1. Infers types for expressions (bottom-up)
// 2. Propagates expected types downward (top-down)
// 3. Validates operator operand types
// 4. Checks function call argument types
// 5. Validates assignments and type annotations
// 6. Sets the `type` field on all nodes

struct TypeChecker : xast::Visitor<Type *, Type *, Scope *> {
    std::unordered_map<std::string, Type *> &type_cache;
    bool has_errors = false;
    
    TypeChecker(std::unordered_map<std::string, Type *> &cache) : type_cache(cache) {}
    
    // Intern a type to ensure canonical representation
    Type *intern_type(Type *ty) {
        if (!ty) return nullptr;
        std::string key = ty->stringify();
        auto it = type_cache.find(key);
        if (it != type_cache.end()) {
            return it->second;
        }
        type_cache[key] = ty;
        return ty;
    }
    
    // Check if src type can be assigned to dst type
    bool can_assign(Type *src, Type *dst) {
        if (!src || !dst) return false;
        if (src == dst) return true;
        if (src->get_kind() == typekind::error_t || dst->get_kind() == typekind::error_t) return true;
        
        // Same canonical type
        if (src->get_canonical() == dst->get_canonical()) return true;
        
        // Numeric conversions - allow implicit widening and narrowing between integrals
        // In practice, many languages allow this for literals
        if (src->is_integral() && dst->is_integral()) {
            // Allow all integral conversions
            // (A more sophisticated approach would check if the value fits)
            return true;
        }
        
        // Allow fp to fp conversions
        if (src->is_fp() && dst->is_fp()) {
            return true;
        }
        
        // Pointer to pointer - allow with warning
        if (src->get_kind() == typekind::pointer_t && dst->get_kind() == typekind::pointer_t) {
            return true;
        }
        
        return false;
    }
    
    // Compute the common type between two types for branch unification
    // Returns nullptr if no common type exists
    Type *common_type(Type *a, Type *b) {
        if (!a || !b) return a ? a : b;
        if (a == b) return a;
        if (a->get_kind() == typekind::error_t) return b;
        if (b->get_kind() == typekind::error_t) return a;
        
        // Same canonical type
        if (a->get_canonical() == b->get_canonical()) return a;
        
        // Both integral - use the larger type
        if (a->is_integral() && b->is_integral()) {
            unsigned a_size = a->get_size();
            unsigned b_size = b->get_size();
            if (a_size >= b_size) return a;
            return b;
        }
        
        // Both floating point - use the larger type
        if (a->is_fp() && b->is_fp()) {
            unsigned a_size = a->get_size();
            unsigned b_size = b->get_size();
            if (a_size >= b_size) return a;
            return b;
        }
        
        // Integral and floating point - use the float
        if ((a->is_integral() && b->is_fp()) || (a->is_fp() && b->is_integral())) {
            return a->is_fp() ? a : b;
        }
        
        // Both pointers - if compatible, use one of them
        if (a->get_kind() == typekind::pointer_t && b->get_kind() == typekind::pointer_t) {
            return a; // Could be smarter here
        }
        
        // No common type
        return nullptr;
    }
    
    // Resolve a type expression to a Type*
    Type *resolve_type_expr(xast::Node *node, Scope *scope) {
        if (!node) return nullptr;
        
        // If already has a type, return it
        if (node->type && node->type != MetaType::get()) {
            return node->type;
        }
        
        switch (node->kind) {
        case xast::nk::ref: {
            if (!node->data.is_ident()) return ErrorType::get();
            std::string_view name = *node->data.ident;
            
            // Look up in type cache first
            auto it = type_cache.find(std::string(name));
            if (it != type_cache.end()) {
                return it->second;
            }
            
            // Look up symbol
            xast::Node *decl = find_symbol_in_any(scope, name);
            if (decl && decl->type) {
                return decl->type;
            }
            return ErrorType::get();
        }
        case xast::nk::unary_op: {
            Op op = node->data;
            if (op.kind == op::indirect) {
                // Pointer type: *T
                Type *pointee = resolve_type_expr(xast::c::unary_op::operand(node), scope);
                if (!pointee) return ErrorType::get();
                return intern_type(new PointerType(nullptr, pointee));
            }
            if (op.kind == op::slice) {
                // Array/slice type: []T
                Type *element = resolve_type_expr(xast::c::unary_op::operand(node), scope);
                if (!element) return ErrorType::get();
                return intern_type(new ArrayType(nullptr, element, 0));
            }
            return ErrorType::get();
        }
        case xast::nk::binary_op: {
            Op op = node->data;
            if (op.kind == op::arrow) {
                // Function type: (params) -> return
                xast::Node *params_node = xast::c::binary_op::lhs(node);
                xast::Node *return_node = xast::c::binary_op::rhs(node);
                
                std::vector<Type *> param_types;
                if (params_node && params_node->kind == xast::nk::paren_expr) {
                    xast::Node *inner = xast::c::paren_expr::inner(params_node);
                    if (inner) {
                        if (inner->kind == xast::nk::comma_expr) {
                            for (auto edge : inner->opedges) {
                                Type *pt = resolve_type_expr(edge->used(), scope);
                                param_types.push_back(pt ? pt : ErrorType::get());
                            }
                        } else {
                            Type *pt = resolve_type_expr(inner, scope);
                            param_types.push_back(pt ? pt : ErrorType::get());
                        }
                    }
                }
                
                Type *return_type = resolve_type_expr(return_node, scope);
                if (!return_type) return_type = ErrorType::get();
                
                return intern_type(new FunctionType(nullptr, return_type, param_types));
            }
            return ErrorType::get();
        }
        case xast::nk::composite: {
            // Struct or union type
            xast::Node *layout = xast::c::composite::layout(node);
            if (layout) {
                if (layout->kind == xast::nk::struct_) {
                    return intern_type(new StructType(node, ""));
                }
                if (layout->kind == xast::nk::union_) {
                    return intern_type(new UnionType(node, ""));
                }
            }
            return ErrorType::get();
        }
        default:
            return ErrorType::get();
        }
    }
    
    // Main dispatch for type checking expressions
    Type *visit_prog(xast::Node *node, Type *expected, Scope *scope) override {
        for (auto edge : node->opedges) {
            xast::Node *child = edge->used();
            if (child) {
                dispatch(child, nullptr, scope);
            }
        }
        return nullptr;
    }
    
    Type *visit_bind(xast::Node *node, Type *expected, Scope *scope) override {
        // Skip meta bindings - they're already processed
        if (node->meta) {
            return node->type;
        }
        
        xast::Node *type_annot = xast::c::bind::type_annot(node);
        xast::Node *def = xast::c::bind::def(node);
        
        Type *annotated_ty = nullptr;
        Type *value_ty = nullptr;
        
        if (type_annot) {
            annotated_ty = resolve_type_expr(type_annot, scope);
        }
        
        if (def) {
            // Pass annotated type as expected type to the initializer
            value_ty = dispatch(def, annotated_ty, scope);
        }
        
        // Determine the binding's type
        if (annotated_ty && value_ty) {
            // Check assignment compatibility
            if (!can_assign(value_ty, annotated_ty)) {
                DiagnosticHandler::make(diag::id::call_expr_typecheck_argument, def->sloc)
                    .add(annotated_ty->stringify())
                    .add(value_ty->stringify())
                    .finish();
                has_errors = true;
            }
            node->type = annotated_ty;
        } else if (annotated_ty) {
            node->type = annotated_ty;
        } else if (value_ty) {
            node->type = value_ty;
        } else {
            node->type = ErrorType::get();
        }
        
        return node->type;
    }
    
    Type *visit_func(xast::Node *node, Type *expected, Scope *scope) override {
        // Skip meta functions
        if (node->meta) {
            return node->type;
        }
        
        Scope *func_scope = node->scope ? node->scope : scope;
        
        // Resolve return type
        xast::Node *return_ty_node = xast::c::func::return_ty(node);
        Type *return_ty = return_ty_node ? resolve_type_expr(return_ty_node, scope) : ErrorType::get();
        if (!return_ty) return_ty = ErrorType::get();
        
        // Process parameters
        std::vector<Type *> param_types;
        xast::Node *params = xast::c::func::params(node);
        if (params) {
            for (auto edge : params->opedges) {
                xast::Node *param = edge->used();
                if (param && param->kind == xast::nk::param) {
                    xast::Node *param_type_annot = xast::c::param::type_annot(param);
                    Type *param_ty = param_type_annot ? resolve_type_expr(param_type_annot, scope) : ErrorType::get();
                    if (!param_ty) param_ty = ErrorType::get();
                    param->type = param_ty;
                    param_types.push_back(param_ty);
                }
            }
        }
        
        // Create function type
        Type *func_type = intern_type(new FunctionType(nullptr, return_ty, param_types));
        node->type = func_type;
        
        // Type check the body - pass return type as expected
        xast::Node *body = xast::c::func::body(node);
        if (body) {
            Type *body_ty = dispatch(body, return_ty, func_scope);
            
            // Check that body type matches declared return type
            if (body_ty && return_ty && return_ty->get_kind() != typekind::error_t) {
                if (!can_assign(body_ty, return_ty)) {
                    DiagnosticHandler::make(diag::id::return_type_mismatch, body->sloc)
                        .add(body_ty->stringify())
                        .add(return_ty->stringify())
                        .finish();
                    has_errors = true;
                }
            }
        }
        
        return func_type;
    }
    
    Type *visit_block(xast::Node *node, Type *expected, Scope *scope) override {
        // Use the block's own scope if it has one, otherwise use parent scope
        Scope *block_scope = node->scope ? node->scope : scope;
        
        // Process all statements except the last one without expected type
        Type *last_type = nullptr;
        size_t count = node->opedges.size();
        size_t idx = 0;
        for (auto edge : node->opedges) {
            xast::Node *stmt = edge->used();
            if (stmt) {
                // Only pass expected type to last statement (the "return" value)
                bool is_last = (idx == count - 1);
                last_type = dispatch(stmt, is_last ? expected : nullptr, block_scope);
            }
            ++idx;
        }
        node->type = last_type;
        return last_type;
    }
    
    Type *visit_branch(xast::Node *node, Type *expected, Scope *scope) override {
        // Skip meta branches
        if (node->meta) {
            return node->type;
        }
        
        // Check condition is integral (no expected type for condition)
        xast::Node *cond = xast::c::branch::cond(node);
        if (cond) {
            Type *cond_ty = dispatch(cond, nullptr, scope);
            if (cond_ty && !cond_ty->is_integral() && cond_ty->get_kind() != typekind::error_t) {
                DiagnosticHandler::make(diag::id::logical_not_typecheck, cond->sloc)
                    .add(cond_ty->stringify())
                    .finish();
                has_errors = true;
            }
        }
        
        // Check branches - pass expected type to both
        xast::Node *then_block = xast::c::branch::then(node);
        xast::Node *else_block = xast::c::branch::else_(node);
        
        // First pass: type check then branch with expected type
        Type *then_ty = then_block ? dispatch(then_block, expected, scope) : nullptr;
        
        // Second pass: type check else branch with expected type, or with then_ty if no expected
        Type *else_expected = expected ? expected : then_ty;
        Type *else_ty = else_block ? dispatch(else_block, else_expected, scope) : nullptr;
        
        // Determine result type using expected type or common type
        Type *result_ty = then_ty;
        if (expected && expected->get_kind() != typekind::error_t) {
            // If we have expected type, use it (types should match due to inference)
            result_ty = expected;
        } else if (then_ty && else_ty) {
            result_ty = common_type(then_ty, else_ty);
            if (!result_ty) {
                // No common type - report error
                DiagnosticHandler::make(diag::id::branch_type_mismatch, node->sloc)
                    .add(then_ty->stringify())
                    .add(else_ty->stringify())
                    .finish();
                has_errors = true;
                result_ty = ErrorType::get();
            }
        }
        
        node->type = result_ty;
        return result_ty;
    }
    
    Type *visit_loop(xast::Node *node, Type *expected, Scope *scope) override {
        // Skip meta loops
        if (node->meta) {
            return node->type;
        }
        
        // Check condition is integral
        xast::Node *cond = xast::c::loop::cond(node);
        if (cond) {
            Type *cond_ty = dispatch(cond, nullptr, scope);
            if (cond_ty && !cond_ty->is_integral() && cond_ty->get_kind() != typekind::error_t) {
                DiagnosticHandler::make(diag::id::logical_not_typecheck, cond->sloc)
                    .add(cond_ty->stringify())
                    .finish();
                has_errors = true;
            }
        }
        
        // Check body (loops don't produce a value, so no expected type)
        xast::Node *body = xast::c::loop::body(node);
        if (body) {
            dispatch(body, nullptr, scope);
        }
        
        node->type = nullptr; // Loops don't have a value
        return nullptr;
    }
    
    Type *visit_int_lit(xast::Node *node, Type *expected, Scope *scope) override {
        // Integer literals adopt expected type if it's integral
        if (expected && expected->is_integral()) {
            node->type = expected;
        } else {
            // Default to i64
            node->type = PrimitiveType::get_i64_type();
        }
        return node->type;
    }
    
    Type *visit_char_lit(xast::Node *node, Type *expected, Scope *scope) override {
        // Character literals are u8
        node->type = PrimitiveType::get_u8_type();
        return node->type;
    }
    
    Type *visit_str_lit(xast::Node *node, Type *expected, Scope *scope) override {
        // String literals are *u8
        node->type = intern_type(new PointerType(nullptr, PrimitiveType::get_u8_type()));
        return node->type;
    }
    
    Type *visit_ref(xast::Node *node, Type *expected, Scope *scope) override {
        // Skip type references (meta)
        if (node->meta) {
            return node->type;
        }
        
        if (!node->data.is_ident()) {
            node->type = ErrorType::get();
            return node->type;
        }
        
        std::string_view name = *node->data.ident;
        xast::Node *decl = find_symbol_in_any(scope, name);
        
        if (!decl) {
            DiagnosticHandler::make(diag::id::use_of_undeclared_symbol, node->sloc)
                .add(std::string(name))
                .finish();
            has_errors = true;
            node->type = ErrorType::get();
            return node->type;
        }
        
        // Get type from declaration
        if (decl->type) {
            node->type = decl->type;
        } else {
            node->type = ErrorType::get();
        }
        
        return node->type;
    }
    
    // Internal ref - type is already set, just return it
    Type *visit_internalref(xast::Node *node, Type *expected, Scope *scope) override {
        // Type is already set during AST simplification
        return node->type ? node->type : ErrorType::get();
    }
    
    Type *visit_unary_op(xast::Node *node, Type *expected, Scope *scope) override {
        // Skip type operators
        if (node->meta) {
            return node->type;
        }
        
        xast::Node *operand = xast::c::unary_op::operand(node);
        Type *operand_ty = dispatch(operand, nullptr, scope);
        if (!operand_ty) operand_ty = ErrorType::get();
        
        // Error propagation
        if (operand_ty->get_kind() == typekind::error_t) {
            node->type = ErrorType::get();
            return node->type;
        }
        
        Op op = node->data;
        switch (op.kind) {
        case op::indirect: {
            // Dereference: *ptr -> pointee
            if (operand_ty->get_kind() == typekind::pointer_t) {
                auto ptr_ty = static_cast<PointerType *>(operand_ty);
                node->type = ptr_ty->get_pointee();
            } else {
                DiagnosticHandler::make(diag::id::indirection_typecheck, operand->sloc)
                    .add(operand_ty->stringify())
                    .finish();
                has_errors = true;
                node->type = ErrorType::get();
            }
            break;
        }
        case op::addr: {
            // Address-of: &val -> *val_ty
            node->type = intern_type(new PointerType(nullptr, operand_ty));
            break;
        }
        case op::neg: {
            // Negation: requires numeric
            if (!operand_ty->is_numeric()) {
                DiagnosticHandler::make(diag::id::unary_negate_typecheck, operand->sloc)
                    .add(operand_ty->stringify())
                    .finish();
                has_errors = true;
                node->type = ErrorType::get();
            } else {
                node->type = operand_ty;
            }
            break;
        }
        case op::lnot: {
            // Logical not: requires integral, returns i32
            if (!operand_ty->is_integral()) {
                DiagnosticHandler::make(diag::id::logical_not_typecheck, operand->sloc)
                    .add(operand_ty->stringify())
                    .finish();
                has_errors = true;
                node->type = ErrorType::get();
            } else {
                node->type = PrimitiveType::get_i32_type();
            }
            break;
        }
        case op::preincr:
        case op::predecr:
        case op::postincr:
        case op::postdecr: {
            // Inc/dec: requires integral
            if (!operand_ty->is_integral()) {
                DiagnosticHandler::make(diag::id::side_effecting_unary_typecheck_integral, operand->sloc)
                    .add(operand_ty->stringify())
                    .finish();
                has_errors = true;
                node->type = ErrorType::get();
            } else {
                node->type = operand_ty;
            }
            break;
        }
        default:
            node->type = operand_ty;
            break;
        }
        
        return node->type;
    }
    
    Type *visit_binary_op(xast::Node *node, Type *expected, Scope *scope) override {
        // Skip type operators
        if (node->meta) {
            return node->type;
        }
        
        Op op = node->data;
        xast::Node *lhs = xast::c::binary_op::lhs(node);
        xast::Node *rhs = xast::c::binary_op::rhs(node);
        
        // Handle dot operator (field access) specially
        if (op.kind == op::dot) {
            Type *lhs_ty = dispatch(lhs, nullptr, scope);
            
            // For meta refs (type names), we need to look up the binding manually
            // since TypeChecker skips meta refs
            xast::Node *composite_node = nullptr;
            bool is_type_namespace_access = false;
            
            if (lhs->kind == xast::nk::ref && lhs->meta && lhs->data.is_ident()) {
                // This is a type name like IntOptVec.new() - look up the binding
                std::string_view name = *lhs->data.ident;
                xast::Node *decl = find_symbol_in_any(scope, name);
                if (decl && decl->kind == xast::nk::bind) {
                    xast::Node *def = xast::c::bind::def(decl);
                    if (def && def->kind == xast::nk::composite) {
                        composite_node = def;
                        is_type_namespace_access = true;
                    }
                    // Check if the binding's type is a StructType/UnionType (direct composite)
                    if (!composite_node && decl->type && 
                        (decl->type->get_kind() == typekind::struct_t || decl->type->get_kind() == typekind::union_t)) {
                        composite_node = static_cast<DeclType *>(decl->type)->decl;
                        is_type_namespace_access = true;
                    }
                    // Also check if the binding's type is an InstantiatedType
                    if (!composite_node && decl->type && decl->type->get_kind() == typekind::instantiated_t) {
                        composite_node = static_cast<InstantiatedType *>(decl->type)->instantiation_node;
                        is_type_namespace_access = true;
                    }
                    if (!composite_node && decl->type && decl->type->get_kind() == typekind::alias_t) {
                        Type *canonical = decl->type->get_canonical();
                        if (canonical && canonical->get_kind() == typekind::instantiated_t) {
                            composite_node = static_cast<InstantiatedType *>(canonical)->instantiation_node;
                            is_type_namespace_access = true;
                        }
                        // Also handle alias to struct/union
                        if (!composite_node && canonical && 
                            (canonical->get_kind() == typekind::struct_t || canonical->get_kind() == typekind::union_t)) {
                            composite_node = static_cast<DeclType *>(canonical)->decl;
                            is_type_namespace_access = true;
                        }
                    }
                    // Set lhs type for consistency  
                    if (decl->type) {
                        lhs->type = decl->type;
                        lhs_ty = decl->type;
                    }
                }
            }
            
            if (!lhs_ty || lhs_ty->get_kind() == typekind::error_t) {
                // Only error if we couldn't resolve this via meta ref lookup either
                if (!is_type_namespace_access) {
                    node->type = ErrorType::get();
                    return node->type;
                }
            }
            
            // Unwrap pointer for -> like access (auto-deref)
            Type *base_ty = lhs_ty ? lhs_ty : ErrorType::get();
            if (base_ty->get_kind() == typekind::pointer_t) {
                base_ty = static_cast<PointerType *>(base_ty)->get_pointee();
            }
            
            // Check if LHS is a type expression (for namespace access)
            // This handles: TypeName.member, internalref.member, or TypeConstructor!(args).member
            // BUT NOT value.field where value has an instantiated type
            bool is_type_expr = is_type_namespace_access ||
                lhs->kind == xast::nk::internalref || 
                (lhs_ty && lhs_ty->get_kind() == typekind::meta_t) || 
                lhs->kind == xast::nk::composite ||
                (lhs->kind == xast::nk::ref && lhs->meta);
            
            if (is_type_expr) {
                // Namespace access on a type - look up member in the type's where clause
                // composite_node may already be set for meta refs above
                
                // For internalref, get composite from the InstantiatedType
                if (!composite_node && lhs->kind == xast::nk::internalref && lhs->type && 
                    lhs->type->get_kind() == typekind::instantiated_t) {
                    composite_node = static_cast<InstantiatedType *>(lhs->type)->instantiation_node;
                }
                // Also handle internalref with struct/union type
                if (!composite_node && lhs->kind == xast::nk::internalref && lhs->type && 
                    (lhs->type->get_kind() == typekind::struct_t || lhs->type->get_kind() == typekind::union_t)) {
                    composite_node = static_cast<DeclType *>(lhs->type)->decl;
                }
                // If LHS is directly a composite node
                if (!composite_node && lhs->kind == xast::nk::composite) {
                    composite_node = lhs;
                }
                // If LHS type is an InstantiatedType, get the composite from it
                if (!composite_node && lhs_ty && lhs_ty->get_kind() == typekind::instantiated_t) {
                    composite_node = static_cast<InstantiatedType *>(lhs_ty)->instantiation_node;
                }
                // Also handle struct/union types
                if (!composite_node && lhs_ty && 
                    (lhs_ty->get_kind() == typekind::struct_t || lhs_ty->get_kind() == typekind::union_t)) {
                    composite_node = static_cast<DeclType *>(lhs_ty)->decl;
                }
                
                if (composite_node) {
                    // Get the member name from RHS - could be a ref or a call
                    xast::Node *member_ref = nullptr;
                    xast::Node *call_node = nullptr;  // If RHS is a call
                    
                    if (rhs->kind == xast::nk::ref && rhs->data.is_ident()) {
                        member_ref = rhs;
                    } else if (rhs->kind == xast::nk::call) {
                        // For Type.method() syntax, RHS is a call node
                        call_node = rhs;
                        xast::Node *callee = xast::c::call::callable(rhs);
                        if (callee && callee->kind == xast::nk::ref && callee->data.is_ident()) {
                            member_ref = callee;
                        }
                    }
                    
                    if (member_ref && member_ref->data.is_ident()) {
                        std::string member_name = std::string(*member_ref->data.ident);
                    
                        // Look up in the composite's namespace (where clause)
                        xast::Node *namespc = xast::c::composite::namespc(composite_node);
                        if (namespc && namespc->kind == xast::nk::block) {
                            for (auto edge : namespc->opedges) {
                                xast::Node *item = edge->used();
                                if (item && item->kind == xast::nk::bind && item->data.is_ident()) {
                                    if (*item->data.ident == member_name) {
                                        // Found the member - type check it
                                        Scope *composite_scope = composite_node->scope ? composite_node->scope : scope;
                                        Type *member_ty = dispatch(item, nullptr, composite_scope);
                                        member_ref->type = member_ty;
                                        
                                        // If RHS was a call, manually type-check the call
                                        if (call_node) {
                                            // member_ty should be a function type
                                            if (member_ty && member_ty->get_kind() == typekind::function_t) {
                                                auto fn_ty = static_cast<FunctionType *>(member_ty);
                                                // Type-check arguments
                                                xast::Node *args = xast::c::call::args(call_node);
                                                if (args) {
                                                    dispatch(args, nullptr, scope);
                                                }
                                                // Return type is the function's return type
                                                Type *ret_ty = fn_ty->get_return_ty();
                                                call_node->type = ret_ty;
                                                node->type = ret_ty;
                                            } else {
                                                // Not callable
                                                node->type = ErrorType::get();
                                            }
                                        } else {
                                            node->type = member_ty;
                                        }
                                        return node->type;
                                    }
                                }
                            }
                        }
                    
                        // Member not found in namespace
                        DiagnosticHandler::make(diag::id::ref_undeclared_symbol, member_ref->sloc)
                            .add(member_name)
                            .finish();
                        has_errors = true;
                    }
                }
                
                node->type = ErrorType::get();
                return node->type;
            }
            
            // Check field access on struct/union (value.field)
            // Also handle InstantiatedType which wraps struct types
            xast::Node *decl_node = nullptr;
            if (base_ty->get_kind() == typekind::struct_t || base_ty->get_kind() == typekind::union_t) {
                auto decl_ty = static_cast<DeclType *>(base_ty);
                decl_node = decl_ty->decl;
            } else if (base_ty->get_kind() == typekind::instantiated_t) {
                // InstantiatedType - get composite from instantiation_node
                decl_node = static_cast<InstantiatedType *>(base_ty)->instantiation_node;
            }
            
            if (decl_node && decl_node->kind == xast::nk::composite) {
                if (rhs->kind == xast::nk::ref && rhs->data.is_ident()) {
                    std::string field_name = std::string(*rhs->data.ident);
                    
                    // Find field in struct/union (layout)
                    xast::Node *layout = xast::c::composite::layout(decl_node);
                    
                    if (layout) {
                        for (auto edge : layout->opedges) {
                            xast::Node *field = edge->used();
                            if (field && (field->kind == xast::nk::field || field->kind == xast::nk::variant)) {
                                if (field->data.is_ident() && *field->data.ident == field_name) {
                                    // Found the field
                                    xast::Node *field_type_annot = xast::c::field::type_annot(field);
                                    Type *field_ty = field_type_annot ? 
                                        resolve_type_expr(field_type_annot, scope) : ErrorType::get();
                                    node->type = field_ty;
                                    rhs->type = field_ty;
                                    return node->type;
                                }
                            }
                        }
                    }
                    
                    // Field not found
                    DiagnosticHandler::make(diag::id::ref_undeclared_symbol, rhs->sloc)
                        .add(field_name)
                        .finish();
                    has_errors = true;
                }
            } else {
                DiagnosticHandler::make(diag::id::dot_on_non_field_accessible_type, lhs->sloc)
                    .add(lhs_ty->stringify())
                    .finish();
                has_errors = true;
            }
            
            node->type = ErrorType::get();
            return node->type;
        }
        
        // Handle assignment specially - rhs gets lhs type as expected
        if (op.kind == op::assign) {
            Type *lhs_ty = dispatch(lhs, nullptr, scope);
            Type *rhs_ty = dispatch(rhs, lhs_ty, scope);  // Pass lhs type as expected
            
            if (!lhs_ty) lhs_ty = ErrorType::get();
            if (!rhs_ty) rhs_ty = ErrorType::get();
            
            // Check assignment compatibility
            if (!can_assign(rhs_ty, lhs_ty)) {
                DiagnosticHandler::make(diag::id::assignment_requires_explicit_downcast, node->sloc)
                    .add(lhs_ty->stringify())
                    .add(rhs_ty->stringify())
                    .finish();
                has_errors = true;
            }
            
            node->type = lhs_ty;
            return node->type;
        }
        
        // General binary operators - propagate expected to operands for arithmetic
        Type *lhs_ty = dispatch(lhs, expected, scope);
        Type *rhs_ty = dispatch(rhs, lhs_ty ? lhs_ty : expected, scope);
        
        if (!lhs_ty) lhs_ty = ErrorType::get();
        if (!rhs_ty) rhs_ty = ErrorType::get();
        
        // Error propagation
        if (lhs_ty->get_kind() == typekind::error_t || rhs_ty->get_kind() == typekind::error_t) {
            node->type = ErrorType::get();
            return node->type;
        }
        
        // Arithmetic operators require numeric types
        bool is_arithmetic = op.kind == op::add || op.kind == op::sub || 
                            op.kind == op::mult || op.kind == op::div || op.kind == op::mod;
        
        // Comparison operators
        bool is_comparison = op.kind == op::eq || op.kind == op::neq ||
                            op.kind == op::lt || op.kind == op::gt ||
                            op.kind == op::lte || op.kind == op::gte;
        
        // Logical operators
        bool is_logical = op.kind == op::land || op.kind == op::lor;
        
        if (is_arithmetic) {
            if (!lhs_ty->is_numeric() || !rhs_ty->is_numeric()) {
                DiagnosticHandler::make(diag::id::binary_op_typecheck, node->sloc)
                    .add(token::get_operator_string(op.tok))
                    .add(lhs_ty->stringify())
                    .add(rhs_ty->stringify())
                    .finish();
                has_errors = true;
                node->type = ErrorType::get();
            } else {
                // Result is the larger type
                node->type = lhs_ty->get_size() >= rhs_ty->get_size() ? lhs_ty : rhs_ty;
            }
        } else if (is_comparison) {
            // Comparisons return i32 (bool-like)
            // Types must be compatible
            if (!can_assign(rhs_ty, lhs_ty) && !can_assign(lhs_ty, rhs_ty)) {
                DiagnosticHandler::make(diag::id::binary_op_typecheck, node->sloc)
                    .add(token::get_operator_string(op.tok))
                    .add(lhs_ty->stringify())
                    .add(rhs_ty->stringify())
                    .finish();
                has_errors = true;
            }
            node->type = PrimitiveType::get_i32_type();
        } else if (is_logical) {
            // Logical operators require integral types, return i32
            if (!lhs_ty->is_integral() || !rhs_ty->is_integral()) {
                DiagnosticHandler::make(diag::id::binary_op_typecheck, node->sloc)
                    .add(token::get_operator_string(op.tok))
                    .add(lhs_ty->stringify())
                    .add(rhs_ty->stringify())
                    .finish();
                has_errors = true;
                node->type = ErrorType::get();
            } else {
                node->type = PrimitiveType::get_i32_type();
            }
        } else {
            // Default: use lhs type
            node->type = lhs_ty;
        }
        
        return node->type;
    }
    
    Type *visit_call(xast::Node *node, Type *expected, Scope *scope) override {
        // Skip meta calls
        if (node->meta) {
            return node->type;
        }
        
        xast::Node *callable = xast::c::call::callable(node);
        Type *callable_ty = dispatch(callable, nullptr, scope);
        
        if (!callable_ty || callable_ty->get_kind() == typekind::error_t) {
            node->type = ErrorType::get();
            return node->type;
        }
        
        if (callable_ty->get_kind() != typekind::function_t) {
            DiagnosticHandler::make(diag::id::noncallable_expression, callable->sloc)
                .add(callable_ty->stringify())
                .finish();
            has_errors = true;
            node->type = ErrorType::get();
            return node->type;
        }
        
        auto fn_ty = static_cast<FunctionType *>(callable_ty);
        auto param_types = fn_ty->get_params();
        
        // Check arguments - pass parameter types as expected types
        xast::Node *args = xast::c::call::args(node);
        std::vector<Type *> arg_types;
        
        if (args) {
            if (args->kind == xast::nk::comma_expr) {
                size_t idx = 0;
                for (auto edge : args->opedges) {
                    Type *expected_ty = idx < param_types.size() ? param_types[idx] : nullptr;
                    Type *arg_ty = dispatch(edge->used(), expected_ty, scope);
                    arg_types.push_back(arg_ty ? arg_ty : ErrorType::get());
                    ++idx;
                }
            } else {
                Type *expected_ty = param_types.size() > 0 ? param_types[0] : nullptr;
                Type *arg_ty = dispatch(args, expected_ty, scope);
                arg_types.push_back(arg_ty ? arg_ty : ErrorType::get());
            }
        }
        
        // Check argument count
        if (arg_types.size() != param_types.size()) {
            DiagnosticHandler::make(
                arg_types.size() < param_types.size() ? 
                    diag::id::call_expr_too_few_arguments : 
                    diag::id::call_expr_too_many_arguments,
                node->sloc)
                .add(std::to_string(param_types.size()))
                .add(std::to_string(arg_types.size()))
                .add(callable_ty->stringify())
                .finish();
            has_errors = true;
        } else {
            // Check argument types
            for (size_t i = 0; i < arg_types.size(); ++i) {
                if (!can_assign(arg_types[i], param_types[i])) {
                    DiagnosticHandler::make(diag::id::call_expr_typecheck_argument, node->sloc)
                        .add(param_types[i]->stringify())
                        .add(arg_types[i]->stringify())
                        .finish();
                    has_errors = true;
                }
            }
        }
        
        node->type = fn_ty->get_return_ty();
        return node->type;
    }
    
    Type *visit_paren_expr(xast::Node *node, Type *expected, Scope *scope) override {
        xast::Node *inner = xast::c::paren_expr::inner(node);
        Type *inner_ty = inner ? dispatch(inner, expected, scope) : nullptr;
        node->type = inner_ty;
        return inner_ty;
    }
    
    Type *visit_comma_expr(xast::Node *node, Type *expected, Scope *scope) override {
        Type *last_ty = nullptr;
        for (auto edge : node->opedges) {
            last_ty = dispatch(edge->used(), nullptr, scope);
        }
        node->type = last_ty;
        return last_ty;
    }
    
    Type *visit_cast(xast::Node *node, Type *expected, Scope *scope) override {
        xast::Node *expr = xast::c::cast::expr(node);
        xast::Node *type_expr = xast::c::cast::type(node);
        
        // Type check the expression (no expected type - cast specifies the type)
        dispatch(expr, nullptr, scope);
        
        // Resolve the target type
        Type *target_ty = resolve_type_expr(type_expr, scope);
        node->type = target_ty ? target_ty : ErrorType::get();
        return node->type;
    }
    
    Type *visit_subscript(xast::Node *node, Type *expected, Scope *scope) override {
        xast::Node *array = xast::c::subscript::array(node);
        xast::Node *index = xast::c::subscript::index(node);
        
        Type *array_ty = dispatch(array, nullptr, scope);
        Type *index_ty = dispatch(index, nullptr, scope);
        
        if (!array_ty || array_ty->get_kind() == typekind::error_t) {
            node->type = ErrorType::get();
            return node->type;
        }
        
        // Check index is integral
        if (index_ty && !index_ty->is_integral()) {
            DiagnosticHandler::make(diag::id::unary_negate_typecheck, index->sloc)
                .add(index_ty->stringify())
                .finish();
            has_errors = true;
        }
        
        // Get element type
        if (array_ty->get_kind() == typekind::pointer_t) {
            node->type = static_cast<PointerType *>(array_ty)->get_pointee();
        } else if (array_ty->get_kind() == typekind::array_t) {
            node->type = static_cast<ArrayType *>(array_ty)->get_element_ty();
        } else {
            DiagnosticHandler::make(diag::id::subscript_on_type, array->sloc)
                .finish();
            has_errors = true;
            node->type = ErrorType::get();
        }
        
        return node->type;
    }
    
    Type *visit_composite(xast::Node *node, Type *expected, Scope *scope) override {
        // Use the composite's own scope (which contains Self -> composite mapping)
        Scope *composite_scope = node->scope ? node->scope : scope;
        
        // Don't traverse the tmpname - it's just an identifier, not an expression to type check
        // Traverse the layout (struct/union) with the composite's scope
        xast::Node *layout = xast::c::composite::layout(node);
        if (layout) {
            dispatch(layout, nullptr, composite_scope);
        }
        
        // Traverse the namespace (where clause) with the composite's scope
        xast::Node *namespc = xast::c::composite::namespc(node);
        if (namespc) {
            dispatch(namespc, nullptr, composite_scope);
        }
        
        // Composite doesn't have a traditional "type" - it IS a type
        // Return the type if already assigned, otherwise MetaType
        if (!node->type) {
            node->type = MetaType::get();
        }
        return node->type;
    }
    
    // Run type checking on the entire AST
    void check(xast::Node *root, Scope *scope) {
        std::cout << "\n=== Pass 9: Type Checking ===\n";
        dispatch(root, nullptr, scope);
        if (has_errors) {
            std::cout << "Type checking found errors.\n";
        } else {
            std::cout << "Type checking complete.\n";
        }
    }
};

// ============================================================================
// Non-Execution Scope Validation Pass
// ============================================================================
// After AST simplification, validates that only let bindings remain in 
// non-execution scopes (top-level prog and where-clauses). All meta expressions
// like if!, while!, call! should have been resolved to bindings by now.

struct NonExecutionScopeValidator {
    bool has_errors = false;
    
    // Get a human-readable description of a node kind for error messages
    const char *describe_node_kind(xast::nk kind) {
        switch (kind) {
            case xast::nk::bind: return "let binding";
            case xast::nk::func: return "function expression";
            case xast::nk::call: return "function call";
            case xast::nk::branch: return "if expression";
            case xast::nk::loop: return "loop expression";
            case xast::nk::ref: return "reference";
            case xast::nk::int_lit: return "integer literal";
            case xast::nk::char_lit: return "character literal";
            case xast::nk::str_lit: return "string literal";
            case xast::nk::unary_op: return "unary operator";
            case xast::nk::binary_op: return "binary operator";
            case xast::nk::block: return "block";
            case xast::nk::paren_expr: return "parenthesized expression";
            case xast::nk::comma_expr: return "comma expression";
            case xast::nk::subscript: return "subscript";
            case xast::nk::cast: return "cast expression";
            case xast::nk::composite: return "composite type";
            default: return "expression";
        }
    }
    
    // Check if a node is a valid declaration for non-execution scope
    bool is_valid_declaration(xast::Node *node) {
        // Only let bindings are valid in non-execution scopes
        return node && node->kind == xast::nk::bind;
    }
    
    // Validate children of a non-execution scope container
    void validate_non_execution_scope(xast::Node *container, const char *scope_name) {
        for (auto edge : container->opedges) {
            xast::Node *child = edge->used();
            if (!child) continue;
            
            if (!is_valid_declaration(child)) {
                DiagnosticHandler::make(
                    diag::id::non_declaration_in_non_execution_scope,
                    child->sloc
                ).add(describe_node_kind(child->kind)).finish();
                has_errors = true;
            } else {
                // For bindings, recursively check any where-clauses in composite types
                check_binding_for_where_clauses(child);
            }
        }
    }
    
    // Check if a binding contains composite types with where-clauses
    void check_binding_for_where_clauses(xast::Node *binding) {
        xast::Node *def = xast::c::bind::def(binding);
        if (!def) return;
        
        // Traverse the definition looking for composite types with where-clauses
        check_node_for_where_clauses(def);
    }
    
    // Recursively check a node for composite types with where-clauses
    void check_node_for_where_clauses(xast::Node *node) {
        if (!node) return;
        
        if (node->kind == xast::nk::composite) {
            xast::Node *namespc = xast::c::composite::namespc(node);
            if (namespc && namespc->kind == xast::nk::block) {
                validate_non_execution_scope(namespc, "where-clause");
            }
        }
        
        // Recurse into children
        for (auto edge : node->opedges) {
            check_node_for_where_clauses(edge->used());
        }
    }
    
    // Run validation on the entire AST
    bool validate(xast::Node *root) {
        if (!root || root->kind != xast::nk::prog) return true;
        
        std::cout << "\n=== Pass 8: Non-Execution Scope Validation ===\n";
        
        validate_non_execution_scope(root, "top-level");
        
        if (has_errors) {
            std::cout << "Non-execution scope validation found errors.\n";
        } else {
            std::cout << "Non-execution scope validation complete.\n";
        }
        
        return !has_errors;
    }
};


void analyze(xast::Node *root, std::vector<PrimitiveType *> const &primitives, StringPool &strings) {

    assert(root && root->kind == xast::nk::prog);
    
    // Initialize the global string pool pointer
    g_string_pool = &strings;

    std::unordered_map<std::string, Type *> type_cache;

    // create global scope
    auto gscope = new Scope;
    for (auto ty : primitives) {
        std::cout << ty->stringify() << "\n";
        // Create a synthetic typebind node for the primitive type
        // TODO: alloc these in the node pool
        // TODO: maybe make these actual code in a std lib?
        xast::Node *tynode = new xast::Node(xast::nk::bind);
        tynode->data = Identifier{ ty->stringify(), {} };
        tynode->type = ty;
        tynode->meta = true;  // Primitive types are meta
        gscope->symbols[ty->stringify()] = tynode;
        type_cache[ty->stringify()] = ty;
    }
    root->scope = gscope;

    // Pass 1: Index all symbols
    std::cout << "\n=== Pass 1: Indexing ===\n";
    IndexingPass()(root, gscope);

    // Pass 2: Validate expression universes (type vs value)
    std::cout << "\n=== Pass 2: Expression Universe Validation ===\n";
    ExpressionUniverseValidator()(root, ExprUniverse::Unknown, gscope);
    std::cout << "Universe validation complete.\n";

    // Pass 3: Propagate meta flag through expressions
    std::cout << "\n=== Pass 3: Meta Propagation ===\n";
    MetaPropagationPass()(root, gscope);

    // Pass 4: Evaluate constant expressions
    std::cout << "\n=== Pass 4: Constant Expression Evaluation ===\n";
    ConstantExprEvaluator const_eval(type_cache);
    // Evaluate all meta bindings
    for (auto &[name, node] : gscope->symbols) {
        if (node->meta && node->kind == xast::nk::bind) {
            ConstValue val = const_eval.dispatch(node, gscope);
            if (!val.is_none() && !val.is_error()) {
                std::cout << "  Constant " << name << " = " << val.stringify() << "\n";
            }
        }
    }
    std::cout << "Constant evaluation complete.\n";

    // Pass 5: Analyze meta dependencies and perform topological sort
    std::cout << "\n=== Pass 5: Meta Dependency Analysis ===\n";
    MetaTopologicalAnalysis meta_analysis;
    auto topo_order = meta_analysis.analyze(root, gscope);
    
    if (!topo_order) {
        std::cout << "Meta analysis failed due to cycles\n";
    } else {
        std::cout << "Meta analysis succeeded, " << topo_order->size() << " meta bindings in order\n";
        
        // Pass 6: Instantiate meta bindings in topological order
        std::cout << "\n=== Pass 6: Meta Instantiation ===\n";
        MetaInstantiator instantiator(gscope, type_cache);
        instantiator.instantiate(*topo_order);
        
        // Pass 6b: Lift composite names to their binding names
        instantiator.lift_composite_names(*topo_order);
        
        // Pass 7: Simplify AST by evaluating meta expressions
        std::cout << "\n=== Pass 7: AST Simplification ===\n";
        ASTSimplifier simplifier(type_cache, const_eval);
        simplifier.simplify_ast(root, gscope);
        
        // Pass 8: Validate non-execution scopes
        // After simplification, only let bindings should remain at top-level and in where-clauses
        NonExecutionScopeValidator scope_validator;
        scope_validator.validate(root);
        
        // Pass 9: Type checking for value expressions
        TypeChecker type_checker(type_cache);
        type_checker.check(root, gscope);
    }

    xast::dump(root);
}

} // namespace fe
