#ifndef LL_H
#define LL_H

#include <type_traits>
#include <cassert>

template <typename T>
class ilist_node {
private:
    T *_next;
    T *_prev;

public:
    ilist_node(T *next, T *prev) : _next(next), _prev(prev) { }
    ilist_node(T *next) : ilist_node(next, nullptr) { }
    ilist_node() : ilist_node(nullptr, nullptr) { }
    void set_next(T *n) { _next = n; }
    T *get_next() { return _next; }
    T *&next() { return _next; }
    void set_prev(T *p) { _prev = p; }
    T *get_prev() { return _prev; }
    T *&prev() { return _prev; }
};

template <typename T, typename = std::enable_if_t<std::is_base_of_v<ilist_node<T>, T>>>
class ilist {
private:
    T *_head = nullptr;
    T *_tail = nullptr;
    unsigned int _size = 0;

public:
    T *head() { return _head; }
    T *tail() { return _tail; }
    unsigned int size() { return _size; }
    void append(T *e) {
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
    void remove(T *e) {
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