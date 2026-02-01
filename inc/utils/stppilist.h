#ifndef UTILS_STPPILIST_H
#define UTILS_STPPILIST_H

#include <cassert>
#include <iostream>
#include <type_traits>

#include "utils/ilist.h"
#include "utils/symtable.h"

template <typename, typename>
class STPPIListNode;

template <typename, typename>
class STPPIList;

/** ------------------- STPPIListNode decl ------------------- */

/**
 * Defines a Node of the Symbol Table and Parent Pointer Intrusive List (STPPIListNode).
 * All STPPIListNodes have a name, and might have a parent.
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
    std::string name = "";
    std::string name_hint = "";
    bool hinted = false;

    // sentinel constructor
    STPPIListNode(Outer *parent) : parent(parent) { }

public:
    STPPIListNode() = default;
    bool has_name() { return hinted && parent; }
    bool has_name_hint() { return hinted; }
    std::string get_name() {
        assert(has_name() && "does not have a name");
        return name;
    }
    std::string get_name_hint() {
        //assert(has_name_hint() && "does not have a name hint");
        return name_hint;
    }
    Outer *get_parent() { return parent; }
    std::string set_parent(Outer *new_parent);
    std::string set_name(std::string name_hint);
    std::string insert_before(Inner *i);
    void remove_from_parent();
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
    Outer *container;

    AutoRenamingSymbolTable<Inner *> &symtable();
    void set_parent_fields(Inner *i, Outer *p);
    void set_name_fields(Inner *i, std::string &name);
    void set_name_fields(Inner *i, std::string &name, std::string &name_hint);
    void maybe_remove_name_from_symtable(Inner *i);

public:
    /**
     * Pass 'this' as the parameter.
     * This is the way STPPIList gets a pointer to the parent
     * (the class containing this object).
    */
    STPPIList(Outer *you)
        : IList<Inner>(new node_type(you), new node_type(you)),
        container(you) {
        static_assert(
            std::is_base_of<STPPIListNode<Inner, Outer>, Inner>::value,
            "Inner must inherit from STPPIListNode<Inner>"
        );
    }
    std::string append(Inner *i);
    std::string append(Inner *i, std::string name_hint);
    std::string append_before(Inner *i, Inner *before);
    std::string append_before(Inner *i, Inner *before, std::string name_hint);
    void remove(Inner *i);
    Inner *remove(std::string name);
    std::string rename(Inner *i, std::string name_hint);
    Inner *get(std::string name);
};

/** ------------------- STPPIListNode impl ------------------- */

template <typename Inner, typename Outer>
std::string STPPIListNode<Inner, Outer>::set_parent(Outer *new_parent) {

    if (parent) {

        // we must remove this node from the existing parent
        parent->get_inner_list(static_cast<Inner *>(nullptr)).remove(static_cast<Inner *>(this));
    }

    if (new_parent) {

        // let the new parent do everything else
        return new_parent->get_inner_list(static_cast<Inner *>(nullptr)).append(static_cast<Inner *>(this));
    }

    return name_hint;
}
template <typename Inner, typename Outer>
std::string STPPIListNode<Inner, Outer>::set_name(std::string name_hint) {

    if (!parent) {

        // no parent so we must set these fields manually 
        this->name_hint = name_hint;
        hinted = true;

        // just return the name hint
        return name_hint;
    }

    // we have a parent, let it do all the work
    return parent->get_inner_list(static_cast<Inner *>(this)).rename(static_cast<Inner *>(this), name_hint);
}
template <typename Inner, typename Outer>
std::string STPPIListNode<Inner, Outer>::insert_before(Inner *i) {

    if (parent) {

        // we must remove this node from the existing parent
        parent->get_inner_list(static_cast<Inner *>(nullptr)).remove(static_cast<Inner *>(this));
    }

    Outer *new_parent = i->parent;
    assert(new_parent && "cannot insert before a parent-less node");

    // let the new parent do everything else
    return new_parent->get_inner_list(static_cast<Inner *>(nullptr)).append_before(static_cast<Inner *>(this), i);
}
template <typename Inner, typename Outer>
void STPPIListNode<Inner, Outer>::remove_from_parent() {

    assert(parent && "cannot remove a parent-less node from parent");

    // let parent do the rest
    parent->get_inner_list(static_cast<Inner *>(nullptr)).remove(static_cast<Inner *>(this));
}

/** ------------------- STPPIList impl ------------------- */

template <typename Inner, typename Outer>
AutoRenamingSymbolTable<Inner *> &STPPIList<Inner, Outer>::symtable() {
    return container->get_symtable(static_cast<Inner *>(nullptr));
}

template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::set_parent_fields(Inner *i, Outer *p) {

    // set its parent
    i->parent = p;
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::set_name_fields(Inner *i, std::string &name) {

    // set the actual name we gave it
    i->name = name;
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::set_name_fields(Inner *i, std::string &name, std::string &name_hint) {

    // set the name we gave it
    set_name_fields(i, name);

    // set the name hint
    i->name_hint = name_hint;

    // since we gave it a new name hint, we also need to set hinted (just in case it wasn't before)
    i->hinted = true;
}
template <typename Inner, typename Outer>
std::string STPPIList<Inner, Outer>::append(Inner *i) {

    // append to the ilist
    IList<Inner>::append(i);

    // set the parent
    set_parent_fields(i, container);

    // maybe set name ...
    std::string name;
    if (i->hinted) {

        // this node needs a name
        // insert into symtable and get rename
        name = symtable().insert(i->name_hint, i);

        // set name to the rename
        set_name_fields(i, name);
    }
    
    return name;
}
template <typename Inner, typename Outer>
std::string STPPIList<Inner, Outer>::append(Inner *i, std::string name_hint) {

    // append to ilist
    IList<Inner>::append(i);

    // set parent
    set_parent_fields(i, container);

    // needs a name ...
    // insert into symtable and get rename
    std::string name = symtable().insert(name_hint, i);

    // set name hint and name
    set_name_fields(i, name, name_hint);
    
    return name;
}
template <typename Inner, typename Outer>
std::string STPPIList<Inner, Outer>::append_before(Inner *i, Inner *before) {

    // append to the ilist
    IList<Inner>::append_before(i, before);

    // set the parent
    set_parent_fields(i, container);

    // maybe set name ...
    std::string name;
    if (i->hinted) {

        // this node needs a name
        // insert into symtable and get rename
        name = symtable().insert(i->name_hint, i);

        // set name to the rename
        set_name_fields(i, name);
    }
    
    return name;
}
template <typename Inner, typename Outer>
std::string STPPIList<Inner, Outer>::append_before(Inner *i, Inner *before, std::string name_hint) {

    // append to ilist
    IList<Inner>::append_before(i, before);

    // set parent
    set_parent_fields(i, container);

    // needs a name ...
    // insert into symtable and get rename
    std::string name = symtable().insert(name_hint, i);

    // set name hint and name
    set_name_fields(i, name, name_hint);
    
    return name;
}
template <typename Inner, typename Outer>
void STPPIList<Inner, Outer>::remove(Inner *i) {

    // remove from ilist
    IList<Inner>::remove(i);

    // maybe remove from symtable ...
    if (i->has_name()) {

        // has a name, so have to remove
        symtable().remove(i->name);
    }
    
    // unset parent
    set_parent_fields(i, nullptr);
}
template <typename Inner, typename Outer>
Inner *STPPIList<Inner, Outer>::remove(std::string name) {

    // remove from symtable, also get the corresponding node
    Inner *i = symtable().remove(name);

    assert(i && "no node with given name");

    // remove from ilist
    IList<Inner>::remove(i);

    // unset parent
    set_parent_fields(i, nullptr);

    return i;
}
template <typename Inner, typename Outer>
std::string STPPIList<Inner, Outer>::rename(Inner *i, std::string name_hint) {

    if (i->has_name()) {

        // already has a name ...
        // remove the node from symtable
        Inner *removed = symtable().remove(i->name);

        assert(removed == i && "removed Inner does not match given Inner");
    }

    // insert into symtable and get actual name
    std::string name = symtable().insert(name_hint, i);

    // set name and name_hint
    set_name_fields(i, name, name_hint);

    return name;
}
template <typename Inner, typename Outer>
Inner *STPPIList<Inner, Outer>::get(std::string name) {
    return symtable().get(name);
}

#endif
