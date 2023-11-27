#ifndef UTILS_STPPILIST_H
#define UTILS_STPPILIST_H

#include <type_traits>
#include <cassert>
#include "utils/ilist.h"
#include "utils/symtable.h"

template <typename, typename>
class STPPIListNode;

template <typename, typename>
class STPPIList;

/** ------------------- Ensure get_inner_list is member ------------------- */
// THIS DOES NOT WORK -- DO NOT USE
template<typename Class, typename MemberType>
struct determine_get_inner_list_impl { };

template <typename Class, typename FuncRetType, typename... FuncArgTypes>
struct determine_get_inner_list_impl<Class, FuncRetType(FuncArgTypes...)> {

    template <typename ClassToCheck>
    static constexpr auto determine(ClassToCheck *) -> typename
        std::is_same<
            decltype(
                std::declval<ClassToCheck>().get_inner_type(std::declval<FuncArgTypes>()...)
            ),
            FuncRetType
        >::type
    ;

    template <typename ClassToCheck>
    static constexpr std::false_type determine(ClassToCheck *);

public:
    using type = decltype(determine<Class>(nullptr));
    static constexpr bool value = type::value;
};

template <typename Inner, typename Outer>
struct determine_get_inner_list {
    static constexpr bool value
        = determine_get_inner_list_impl<Outer, STPPIList<Inner, Outer> &(Inner *)>::value;
};

/** ------------------- STPPIListNode decl ------------------- */

/**
 * Defines a Node of the Symbol Table and Parent Pointer Intrusive List (STPPIListNode).
 * Adds a parent pointer and a string name to the IListNode implementation.
 * Inner must derive from this class.
 * Outer is the class that will contain the STPPIList in which Inner will be a node.
 * STPPIList<Inner, Outer> &Outer::get_inner_list(Inner *) must be defined and accessible by STPPIListNode<Inner, Outer>.
 * STPPIListNode relies on its corresponding STPPIList for all of its operations, thus it will not function without get_inner_list().
 * @param Inner the stored class that derives from this class
 * @param Outer the parent class that will contain this list
*/
template <typename Inner, typename Outer>
class STPPIListNode : public IListNode<Inner> {
private:
    friend class STPPIList<Inner, Outer>;
    Outer *parent = nullptr;
    std::string name;
    bool named = false;

public:
    STPPIListNode() {
        // static_assert(
        //     determine_get_inner_list<Inner, Outer>::value,
        //     "Outer must define 'STPPIList<Inner, Outer> &get_inner_list(Inner *)'"
        // );
    }
    bool has_name() { return named; }
    std::string get_name();
    Outer *get_parent() { return parent; }
    void set_parent(Outer *p);
    void set_name(std::string n);
};

/** ------------------- STPPIList decl ------------------- */

/**
 * Defines the Symbol Table and Parent Pointer Intrusive List (STPPIList).
 * Inner must derive from STPPIListNode<Inner, Outer> to work with the STPPIList defined inside Outer.
 * @param Inner the stored class that derives from STPPIListNode<Inner, Outer>
 * @param Outer the parent class that will contain this list
*/
template <typename Inner, typename Outer>
class STPPIList : public IList<Inner> {
public:
    using node_type = STPPIListNode<Inner, Outer>;
private:
    SymbolTable<Inner> symtable;
    Outer *container;

    void set_parent_fields(Inner *i, Outer *p);
    void set_name_fields(Inner *i, std::string name);
    void maybe_remove_name_from_symtable(Inner *i);

public:
    /**
     * Pass 'this' as the parameter.
     * This is the way STPPIList gets a pointer to the parent
     * (the class containing this object).
    */
    STPPIList(Outer *you) : container(you) {
        static_assert(
            std::is_base_of<STPPIListNode<Inner, Outer>, Inner>::value,
            "Inner must inherit from STPPIListNode<Inner>"
        );
        // static_assert(
        //     determine_get_inner_list<Inner, Outer>::value,
        //     "Outer must define 'STPPIList<Inner, Outer> &get_inner_list(Inner *)'"
        // );
    }
    void append(Inner *i);
    void remove(Inner *i);
    void rename(Inner *i, std::string name);
    void remove(std::string name);
    Inner *get(std::string name);
    void append_and_rename(Inner *i, std::string name);
};

/** ------------------- STPPIListNode impl ------------------- */

template <typename Inner, typename Outer>
std::string STPPIListNode<Inner, Outer>::get_name() {
    assert(has_name());
    return name;
}
template <typename Inner, typename Outer>
void STPPIListNode<Inner, Outer>::set_parent(Outer *p) {
    if (parent) {
        parent->get_inner_list(static_cast<Inner *>(nullptr)).remove(static_cast<Inner *>(this));
    }
    parent = p;
    parent->get_inner_list(static_cast<Inner *>(nullptr)).append(static_cast<Inner *>(this));
}
template <typename Inner, typename Outer>
void STPPIListNode<Inner, Outer>::set_name(std::string n) {
    if (parent) {
        parent->get_inner_list(static_cast<Inner *>(this)).rename(static_cast<Inner *>(this), n);
    }
    else {
        name = n;
        named = true;
    }
}

/** ------------------- STPPIList impl ------------------- */

template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::set_parent_fields(Inner *i, Outer *p) {
    static_cast<node_type *>(i)->parent = p;
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::set_name_fields(Inner *i, std::string name) {
    node_type *si = static_cast<node_type *>(i);
    si->name = name;
    si->named = true;
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::maybe_remove_name_from_symtable(Inner *i) {
    node_type *si = static_cast<node_type *>(i);
    if (si->has_name()) {
        symtable.remove(si->get_name());
    }
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::append(Inner *i) {
    IList<Inner>::append(i);
    node_type *si = static_cast<node_type *>(i);
    if (si->has_name()) {
        symtable.insert(si->get_name(), i);
    }
    set_parent_fields(i, container);
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::remove(Inner *i) {
    IList<Inner>::remove(i);
    maybe_remove_name_from_symtable(i);
    set_parent_fields(i, nullptr);
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::rename(Inner *i, std::string name) {
    maybe_remove_name_from_symtable(i);
    symtable.insert(name, i);
    set_name_fields(i, name);
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::remove(std::string name) {
    Inner *i = symtable.remove(name);
    IList<Inner>::remove(i);
    set_parent_fields(i, nullptr);
}
template <typename Inner, typename Outer>
Inner *STPPIList<Inner, Outer>::get(std::string name) {
    return symtable.get(name);
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::append_and_rename(Inner *i, std::string name) {
    IList<Inner>::append(i);
    set_parent_fields(i, container);
    symtable.insert(name, i);
    set_name_fields(i, name);
}

#endif
