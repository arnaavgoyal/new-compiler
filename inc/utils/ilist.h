#ifndef LL_H
#define LL_H

#include <type_traits>
#include <cassert>
#include "ir/symtable.h"

namespace ir {

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

template <typename Inner, typename Outer>
class STPPIListUser;

template <typename Inner, typename Outer>
class STPPIListNode;

template <typename Inner, typename Outer>
class STPPIList;

template <typename Inner, typename Outer>
class STPPIListUser {
private:
    friend class STPPIListNode<Inner, Outer>;
    int random = 0;

public:
    virtual STPPIList<Inner, Outer> &get_inner_list(Inner *) = 0;
};

template <typename Inner, typename Outer>
class STPPIListNode : public IListNode<Inner> {
private:
    friend class STPPIList<Inner, Outer>;
    bool named = false;
    std::string name;
    STPPIListUser<Inner, Outer> *parent;

public:
    bool has_name() { return named; }
    std::string get_name() {
        assert(has_name());
        return name;
    }
    void set_parent(Outer *p) {
        if (parent) {
            parent->get_inner_list(static_cast<Inner *>(this)).remove(static_cast<Inner *>(this));
        }
        parent = static_cast<STPPIListUser<Inner, Outer> *>(p);
        parent->get_inner_list(static_cast<Inner *>(this)).append(static_cast<Inner *>(this), p);
    }
    void set_name(std::string &n) {
        if (parent) {
            parent->get_inner_list(static_cast<Inner *>(this)).rename(name, static_cast<Inner *>(this));
        }
        else {
            name = n;
            named = true;
        }
    }
};

template <typename Inner, typename Outer>
class STPPIList : public IList<Inner> {
private:
    ir::SymbolTable<Inner> symtable;

    static void set_parent_fields(Inner *i, STPPIListUser<Inner, Outer> *p) {
        reinterpret_cast<STPPIListNode<Inner, Outer> *>(i)->parent = p;
    }
    static void set_name_fields(Inner *i, std::string name) {
        STPPIListNode<Inner, Outer> *si = static_cast<STPPIListNode<Inner, Outer> *>(i);
        si->name = name;
        si->named = true;
    }
    void maybe_remove_name_from_symtable(Inner *i, std::string name) {
        STPPIListNode<Inner, Outer> *si = static_cast<STPPIListNode<Inner, Outer> *>(i);
        if (si->has_name()) {
            symtable.remove(si->get_name());
        }
    }

public:
    STPPIList() {
        static_assert(
            std::is_base_of<STPPIListNode<Inner, Outer>, Inner>::value,
            "Inner must inherit from STPPIListNode<Inner>"
        );
    }
    void append(Inner *i, STPPIListUser<Inner, Outer> *that) {
        IList<Inner>::append(i);
        STPPIListNode<Inner, Outer> *si = static_cast<STPPIListNode<Inner, Outer> *>(i);
        if (si->has_name()) {
            symtable.insert(si->get_name(), i);
        }
        set_parent_fields(i, that);
    }
    void remove(Inner *i) {
        IList<Inner>::remove(i);
        STPPIListNode<Inner, Outer> *si = static_cast<STPPIListNode<Inner, Outer> *>(i);
        maybe_remove_name_from_symtable(i, si->get_name());
        set_parent_fields(i, nullptr);
    }
    void rename(Inner *i, std::string name) {
        STPPIListNode<Inner, Outer> *si = static_cast<STPPIListNode<Inner, Outer> *>(i);
        maybe_remove_name_from_symtable(i, name);
        symtable.insert(name, i);
        set_name_fields(i, name);
    }
    void remove_by_name(std::string name) {
        Inner *i = symtable.remove(name);
        IList<Inner>::remove(i);
        set_parent_fields(i, nullptr);
    }
    Inner *get_by_name(std::string name) {
        return symtable.get(name);
    }
    void append_and_rename(Inner *i, std::string name, STPPIListUser<Inner, Outer> *that) {
        IList<Inner>::append(i);
        set_parent_fields(i, that);
        symtable.insert(name, i);
        set_name_fields(i, name);
    }
};

}

#endif