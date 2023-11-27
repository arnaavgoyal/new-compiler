#ifndef UTILS_ITERATOR_H
#define UTILS_ITERATOR_H

/**
 * Defines a simple bidirectional iterator template.
 * This allows clients to implement the full functionality of a bidirectional iterator much easier than if doing so manually.
 * Simply subclass this class and override go_forward() and go_backward().
 * @param ValueType the type of the values that will be iterated over
 * @param ChildIteratorClass your iterator class that derives from this class
*/
template <typename ValueType, typename ChildIteratorClass>
class bidirectional_iterator {
public:
    using iterator_category = std::bidirectional_iterator_tag;
    using difference_type = int;
    using value_type = ValueType;
    using pointer = ValueType *;
    using reference = ValueType &;

protected:
    value_type curr;

    virtual void go_forward() = 0;
    virtual void go_backward() = 0;

private:
    using this_type = bidirectional_iterator<ValueType, ChildIteratorClass>;

public:
    reference operator*() { return curr; }
    pointer operator->() { return &curr; }
    ChildIteratorClass &operator++() {
        go_forward();
        return static_cast<ChildIteratorClass &>(*this);
    }
    ChildIteratorClass operator++(int) {
        this_type it = *this;
        go_forward();
        return static_cast<ChildIteratorClass>(it);
    }
    ChildIteratorClass &operator--() {
        go_backward();
        return static_cast<ChildIteratorClass &>(*this);
    }
    ChildIteratorClass operator--(int) {
        this_type it = *this;
        go_backward();
        return static_cast<ChildIteratorClass>(it);
    }
    friend bool operator==(ChildIteratorClass const &l, ChildIteratorClass const &r)
        { return static_cast<this_type const &>(l).curr == static_cast<this_type const &>(r).curr; }
    friend bool operator!=(ChildIteratorClass const &l, ChildIteratorClass const &r)
        { return static_cast<this_type const &>(l).curr != static_cast<this_type const &>(r).curr; }
};

#endif