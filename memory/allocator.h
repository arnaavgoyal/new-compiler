#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include <vector>
#include <iostream>
#include <stdlib.h>

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

#endif