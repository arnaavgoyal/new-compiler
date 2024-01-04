#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include <vector>
#include <iostream>
#include <memory>
#include <concepts>
#include <cassert>

template <typename T>
class Allocator {
private:

    /** vars for debugging purposes */
    static int count;
    int id;

    /** size of allocated buffers */
    enum { buf_size = 25 };

    /** list of allocated buffers */
    std::vector<T *> buf_list;

    /** the buffer offset into the active buffer */
    int buf_offset;

    /**
     * Allocates a new buffer and appends it to the buffer list.
    */
    void alloc_new_buf();

public:

    /**
     * Constructs a new, empty allocator.
    */
    Allocator();

    /**
     * Allocates a new object of appropriate type.
     * 
     * @return pointer to the new object
    */
    T *alloc();

    /**
     * Destructor.
    */
    ~Allocator();

    /**
     * Prevent copying to maintain uniqueness of allocators.
    */
    Allocator(Allocator const &other) = delete;
    Allocator &operator=(Allocator const &other) = delete;

};

/** Must define in header so template funcs will work */

template <typename T>
int Allocator<T>::count = 0;

template <typename T>
Allocator<T>::Allocator() {
    buf_offset = 0;
    count++;
    id = count;
    //std::cout << "alloced new Allocator<" << typeid(T).name() << "> (id " << id << ")" << std::endl;
}

template <typename T>
void Allocator<T>::alloc_new_buf() {

    // dynamically allocate new buffer of Ts
    T *buf = new T[buf_size];

    // add buffer to list
    buf_list.push_back(buf);

    // reset buf offset to 0
    buf_offset = 0;

    //std::cout << "Allocator<" << typeid(T).name() << ">(id " << id << ") alloced new buf: " << buf << std::endl;
}

template <typename T>
T *Allocator<T>::alloc() {

    // check if at capacity and handle accordingly
    if (buf_offset == buf_size || buf_list.size() == 0) {
        alloc_new_buf();
    }

    // get pointer into next open "slot" in current buffer
    T *ret = buf_list.back() + buf_offset;

    //std::cout << "Allocator<" << typeid(T).name() << ">(id " << id << ") (size " << sizeof(T) << ") alloced: " << ret << " (offset " << buf_offset << ")" << std::endl;

    // increment offset
    buf_offset++;

    return ret;
}

template <typename T>
Allocator<T>::~Allocator() {
    for (
        typename std::vector<T *>::size_type i = 0;
        i < buf_list.size();
        i++
    ) {
        delete[] buf_list[i];
        buf_list[i] = nullptr;
    }
    //std::cout << "Allocator<" << typeid(T).name() << ">(id " << id << ") freed." << std::endl;
}

class RawRegionAllocatorImpl {
private:
    friend class RawRegionAllocator;

    std::vector<std::byte *> bufs;
    std::size_t allocsz;
    std::byte *curr;

    std::byte *alloc_buf(std::size_t sz) {
        return static_cast<std::byte *>(::operator new(sz));
    }

    RawRegionAllocatorImpl(std::size_t initsize, std::size_t allocsize)
        : allocsz(allocsize) {
        curr = alloc_buf(initsize);
        bufs.push_back(curr);
    }

    template <typename AllocTy>
    AllocTy *allocate(unsigned num) {
        static_assert(std::is_trivially_destructible<AllocTy>::value,
            "objects allocated with this allocator do not get destructed");
        assert(sizeof(AllocTy) <= allocsz &&
            "size of object is too big for this allocator");
        if (num == 0) { return nullptr; }
        std::size_t totalsz = sizeof(AllocTy) * num;
        std::size_t rem = bufs.back() - curr;
        if (totalsz > rem) {
            curr = alloc_buf(allocsz);
        }
        AllocTy *retptr = reinterpret_cast<AllocTy *>(curr);
        curr += totalsz;
        return retptr;
    }

public:
    ~RawRegionAllocatorImpl() {
        for (auto buf : bufs) {
            ::operator delete(buf);
        }
    }
};

class RawRegionAllocator {
protected:
    std::shared_ptr<RawRegionAllocatorImpl> impl;

public:
    enum { DEFAULT_ALLOCSZ = 1024 };

    // regular ctors
    RawRegionAllocator(std::size_t initsize, std::size_t allocsize)
        : impl(new RawRegionAllocatorImpl(initsize, allocsize)) { }
    RawRegionAllocator(std::size_t allocsize)
        : RawRegionAllocator(allocsize, allocsize) { }
    RawRegionAllocator()
        : RawRegionAllocator(DEFAULT_ALLOCSZ) { }

    // copy ctor
    RawRegionAllocator(RawRegionAllocator const &other)
        { impl = other.impl; }

    // copy assign
    RawRegionAllocator &operator=(RawRegionAllocator const &rhs) {
        if (this == &rhs) return *this;
        impl = rhs.impl;
    }

    template <typename AllocTy>
    AllocTy *allocate(unsigned num = 1) { return impl->allocate<AllocTy>(num); }
};
static_assert(std::copy_constructible<RawRegionAllocator>);

#endif