#ifndef HASHTABLE_H
#define HASHTABLE_H

#include <string>
#include <map>
#include <stack>
#include <vector>

template <typename T>
class HashTable {
private:

    enum scope_status { open, closed };
    using hash_table = std::map<std::string, T>;

    hash_table table;

public:

    HashTable();
    std::vector<T> const &lookup(std::string const &key);
    void insert(std::string key, T element);

    /**
     * Prevent copying to maintain uniqueness of symbol tables.
    */
    HashTable(HashTable const &other) = delete;
    HashTable &operator=(HashTable &other) = delete;

};

/** Must define in header so template funcs will work */

template <typename T>
HashTable<T>::HashTable() { }

template <typename T>
std::vector<T> const &HashTable<T>::lookup(std::string const &key) {
    return table[key];
}

template <typename T>
void HashTable<T>::insert(std::string key, T value) {
    table[key].push_back(value);
}

#endif