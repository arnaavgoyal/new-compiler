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
protected:
    Inner *_head = nullptr;
    Inner *_tail = nullptr;
    unsigned int _size = 0;

    IList(node_type *_h, node_type *_t)
        : _head(static_cast<Inner *>(_h)), _tail(static_cast<Inner *>(_t)) {

        static_assert(
            std::is_base_of<node_type, Inner>::value,
            "Inner must inherit from IListNode<Inner>"
        );

        if (!_h) {
            _head = static_cast<Inner *>(new node_type);
        }
        if (!_t) {
            _tail = static_cast<Inner *>(new node_type);
        }
        _head->set_next(_tail);
        _tail->set_prev(_head);
    }

public:
    IList() : IList(nullptr, nullptr) { }
    Inner *first() {
        return _head->get_next() == _tail ? nullptr : _head->get_next();
    }
    Inner *last() {
        return _tail->get_prev() == _head ? nullptr : _tail->get_prev();
    }
    unsigned int size() { return _size; }
    void append_before(Inner *e, Inner *before) {
        assert(e && "element to append cannot be null");
        assert(before && "element to append before cannot be null");
        e->set_prev(before->get_prev());
        e->set_next(before);
        before->get_prev()->set_next(e);
        before->set_prev(e);
        _size++;
    }
    void append(Inner *e) {
        append_before(e, _tail);
    }
    void remove(Inner *e) {
        assert(_size > 0 && "cannot remove from empty list");
        assert(e && "element to remove cannot be null");
        e->get_next()->set_prev(e->get_prev());
        e->get_prev()->set_next(e->get_next());
        // commented this out because it leads to unintended
        // iterator invalidation when remove is called on the current element
        // of the iterator
        //e->set_next(nullptr);
        //e->set_prev(nullptr);
        _size--;
    }


private:
    class fwiter : public bidirectional_iterator<Inner *, fwiter> {
    private:
        using bidirectional_iterator<Inner *, fwiter>::curr;

        IList<Inner> *list = nullptr;
        Inner *cache_next = nullptr;
        Inner *cache_prev = nullptr;
        void cache() { cache_next = curr->get_next(); cache_prev = curr->get_prev(); }

        void go_forward() override { curr = cache_next; cache(); }
        void go_backward() override { curr = cache_prev; cache(); }

    public:
        fwiter() { curr = nullptr; }
        fwiter(Inner *ptr, IList<Inner> *list) : list(list) { curr = ptr; cache(); }
    };
    class bwiter : public bidirectional_iterator<Inner *, bwiter> {
    private:
        using bidirectional_iterator<Inner *, bwiter>::curr;

        Inner *cache_next = nullptr;
        Inner *cache_prev = nullptr;
        void cache() { cache_next = curr->get_next(); cache_prev = curr->get_prev(); }

        void go_forward() override { curr = cache_prev; cache(); }
        void go_backward() override { curr = cache_next; cache(); }

    public:
        bwiter() { curr = nullptr; }
        bwiter(Inner *ptr) { curr = ptr; cache(); }
    };

public:
    using iterator = fwiter;
    iterator begin() { return iterator(_head->get_next(), this); }
    iterator end() { return iterator(_tail, this); }

    using reverse_iterator = bwiter;
    reverse_iterator rbegin() { return reverse_iterator(_tail->get_prev()); }
    reverse_iterator rend() { return reverse_iterator(_head); }
};

#endif
