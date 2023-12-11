#ifndef OP_H
#define OP_H

namespace op {

    enum kind {

        __lvalue_low_bound,

        // lvalue ops
        //  prefix unary increment
        preincr,
        //  postfix unary increment
        postincr,
        //  prefix unary decrement
        predecr,
        //  postfix unary decrement
        postdecr,
        //  assignment
        assign,
        //  address of
        addr,

        __lvalue_high_bound,
        __logical_low_bound,

        // logical ops
        //  not
        lnot,
        //  and
        land,
        //  or
        lor,

        __logical_high_bound,
        __arithmetic_low_bound,

        // value ops
        //  addition
        add,
        //  subtraction
        sub,
        //  multiplication
        mult,
        //  division
        div,
        //  modulo
        mod,
        // unary negate
        neg,

        __arithmetic_high_bound,
        __relational_low_bound,

        // relational ops
        //  greater than
        gt,
        //  less than
        lt,
        //  greater than or equal to
        gte,
        //  less than or equal to
        lte,
        //  equal to
        eq,
        //  not equal to
        neq,

        __relational_high_bound,
        __misc_low_bound,

        // misc
        //  grouping
        group,
        // indirection
        deref,

        __misc_high_bound,
    };

    inline bool is_lval_op(kind o) { return o > __lvalue_low_bound && o < __lvalue_high_bound; }
    inline bool is_log_op(kind o) { return o > __logical_low_bound && o < __logical_high_bound; }
    inline bool is_val_op(kind o) { return o > __arithmetic_low_bound && o < __arithmetic_high_bound; }
    inline bool is_rel_op(kind o) { return o > __relational_low_bound && o < __relational_high_bound; }

}

#endif