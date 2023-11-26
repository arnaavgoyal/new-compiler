#ifndef UTILS_ILIST_H
#define UTILS_ILIST_H

#include <type_traits>

template <typename Inner>
class IListNode {
private:
    Inner *_next;
    Inner *_prev;

public:
    IListNode(Inner *next, Inner *prev) : _next(next), _prev(prev) { }
    IListNode(Inner *next) : IListNode(next, nullptr) { }
    IListNode() : IListNode(nullptr, nullptr) { }
    void set_next(Inner *n) { _next = n; }
    Inner *get_next() { return _next; }
    Inner *&next() { return _next; }
    void set_prev(Inner *p) { _prev = p; }
    Inner *get_prev() { return _prev; }
    Inner *&prev() { return _prev; }
};

template <typename Inner>
class IList {
private:
    Inner *_head = nullptr;
    Inner *_tail = nullptr;
    unsigned int _size = 0;

public:
    IList() {
        static_assert(
            std::is_base_of<IListNode<Inner>, Inner>::value,
            "Inner must inherit from IListNode<Inner>"
        );
    }
    Inner *head() { return _head; }
    Inner *tail() { return _tail; }
    unsigned int size() { return _size; }
    void append(Inner *e) {
        if (_tail) {
            e->set_prev(_tail);
            _tail->set_next(e);
        }
        else {
            e->set_prev(nullptr);
            _head = e;
            _tail = e;
        }
        e->set_next(nullptr);
        _size++;
    }
    void remove(Inner *e) {
        if (e->next()) {
            e->next()->prev() = e->prev();
        }
        else {
            // tail
            _tail = e->prev();
        }
        if (e->prev()) {
            e->prev()->next() = e->next();
        }
        else {
            // head
            _head = e->next();
        }
        _size--;
    }
};

#endif
