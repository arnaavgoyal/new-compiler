#ifndef UTILS_ILIST_H
#define UTILS_ILIST_H

#include <type_traits>
#include "utils/iterator.h"

/**
 * Defines a simple implementation of an Intrusive Linked List Node (IListNode).
 * Functions exactly like a regular linked list node, except that Inner must derive from IListNode<Inner> for the IList to work properly.
 * @param Inner the contained class, that derives from this class
*/
template <typename Inner>
class IListNode {
private:
    Inner *_next;
    Inner *_prev;

public:
    IListNode(Inner *prev, Inner *next) : _next(next), _prev(prev) { }
    IListNode() : IListNode(nullptr, nullptr) { }
    void set_next(Inner *n) { _next = n; }
    Inner *get_next() { return _next; }
    Inner *&next() { return _next; }
    void set_prev(Inner *p) { _prev = p; }
    Inner *get_prev() { return _prev; }
    Inner *&prev() { return _prev; }
};

/**
 * Defines an Intrusive Linked List (IList).
 * For this list to work properly, Inner must derive from IListNode<Inner>.
 * Otherwise, functions identically to a regular linked list.
 * Implements all bidirectional iterator functionality, so this can used with <algorithm> and range-based for loops.
 * @param Inner the contained type, which is itself an IListNode<Inner>
*/
template <typename Inner>
class IList {
public:
    using node_type = IListNode<Inner>;
private:
    Inner *_head = nullptr;
    Inner *_tail = nullptr;
    unsigned int _size = 0;

public:
    IList() {
        static_assert(
            std::is_base_of<node_type, Inner>::value,
            "Inner must inherit from IListNode<Inner>"
        );

        _head = static_cast<Inner *>(new node_type);
        _tail = static_cast<Inner *>(new node_type);
        _head->set_next(_tail);
        _tail->set_prev(_head);
    }
    Inner *head() {
        return _head->get_next() == _tail ? nullptr : _head->get_next();
    }
    Inner *tail() {
        return _tail->get_prev() == _head ? nullptr : _tail->get_prev();
    }
    unsigned int size() { return _size; }
    void append(Inner *e) {
        e->set_prev(_tail->get_prev());
        e->set_next(_tail);
        _tail->get_prev()->set_next(e);
        _tail->set_prev(e);
        _size++;
    }
    void remove(Inner *e) {
        assert(_size > 0 && "remove() called on empty list");
        e->get_next()->set_prev(e->get_prev());
        e->get_prev()->set_next(e->get_next());
        e->set_next(nullptr);
        e->set_prev(nullptr);
        _size--;
    }

private:
    class ilist_forward_iterator : public bidirectional_iterator<Inner *, ilist_forward_iterator> {
    private:
        using bidirectional_iterator<Inner *, ilist_forward_iterator>::curr;
        
        void go_forward() override {
            curr = curr->get_next();
        }
        void go_backward() override {
            curr = curr->get_prev();
        }

    public:
        ilist_forward_iterator() { curr = nullptr; }
        ilist_forward_iterator(Inner *ptr) { curr = ptr; }
    };
    class ilist_reverse_iterator : public bidirectional_iterator<Inner *, ilist_reverse_iterator> {
    private:
        using bidirectional_iterator<Inner *, ilist_reverse_iterator>::curr;

        void go_forward() override {
            curr = curr->get_prev();
        }
        void go_backward() override {
            curr = curr->get_next();
        }

    public:
        ilist_reverse_iterator() { curr = nullptr; }
        ilist_reverse_iterator(Inner *ptr) { curr = ptr; }
    };
    class ilist_const_forward_iterator : public bidirectional_iterator<Inner *, ilist_const_forward_iterator> {
    private:
        using bidirectional_iterator<Inner *, ilist_const_forward_iterator>::curr;
        
        void go_forward() override {
            curr = curr->get_next();
        }
        void go_backward() override {
            curr = curr->get_prev();
        }

    public:
        ilist_const_forward_iterator() { curr = nullptr; }
        ilist_const_forward_iterator(Inner *ptr) { curr = ptr; }
    };
    class ilist_const_reverse_iterator : public bidirectional_iterator<Inner *, ilist_const_reverse_iterator> {
    private:
        using bidirectional_iterator<Inner *, ilist_const_reverse_iterator>::curr;
        
        void go_forward() override {
            curr = curr->get_prev();
        }
        void go_backward() override {
            curr = curr->get_next();
        }

    public:
        ilist_const_reverse_iterator() { curr = nullptr; }
        ilist_const_reverse_iterator(Inner *ptr) { curr = ptr; }
    };

public:
    using forward_iterator = ilist_forward_iterator;
    forward_iterator begin() { return forward_iterator(_head->get_next()); }
    forward_iterator end() { return forward_iterator(_tail); }

    using reverse_iterator = ilist_reverse_iterator;
    reverse_iterator rbegin() { return reverse_iterator(_tail->get_prev()); }
    reverse_iterator rend() { return reverse_iterator(_head); }

    using const_forward_iterator = ilist_const_forward_iterator;
    const_forward_iterator begin() const { return const_forward_iterator(_head->get_next()); }
    const_forward_iterator end() const { return const_forward_iterator(_tail); }

    using const_reverse_iterator = ilist_const_reverse_iterator;
    const_reverse_iterator rbegin() const { return const_reverse_iterator(_tail->get_prev()); }
    const_reverse_iterator rend() const { return const_reverse_iterator(_head); }
};

#endif
