#ifndef IR_SYMTABLE_H
#define IR_SYMTABLE_H

#include <map>
#include <string>
#include <cassert>
#include <iostream>

template <typename SymbolValueType>
struct _ty_determiner;

template <typename SymbolValueType>
class AutoRenamingSymbolTable {
private:
    typedef struct {
        SymbolValueType val;
        unsigned collisions;
        bool populated;
    } map_entry_type;

    using map_type = std::map<std::string, map_entry_type>;
    map_type map;

public:
    std::string insert(std::string key, SymbolValueType val) {

        //std::cout << "trying '" << key << "' ..." << std::endl;

        // try insert
        auto res = map.emplace(key, map_entry_type{val, 0, true});
        // pair<
        //   iterator<             =  .first
        //     pair<               =  .first.operator*()
        //       string,           =  .first.operator*().first
        //       map_entry_type>>, =  .first.operator*().second
        //   bool                  =  .second
        // >
        //_ty_determiner<decltype(res)> td;

        // insertion occurred
        if (res.second) {
            // this means key was unique and all is good
            //std::cout << "'" << key << "' never inserted before, done!" << std::endl;
            return key;
        }

        // insertion did not occur
        // this means key is maybe not unique

        // get entry
        map_entry_type &entry = res.first.operator*().second;

        // entry is not populated
        if (!entry.populated) {
            // this entry was previously removed, so the key is valid as-is
            entry.val = val;
            entry.populated = true;
            //std::cout << "'" << key << "' inserted before but removed, done!" << std::endl;
            return key;
        }

        // entry is populated
        // means this key is invalid, and must be renamed

        // get counter
        unsigned &counter = entry.collisions;

        // until a unique rename is found
        while (true) {

            // increment due to the collision
            counter++;

            // use collision counter to rename
            // and attempt to insert now
            auto res = map.emplace(
                key + std::to_string(counter),
                map_entry_type{val, 0, true}
            );

            // check if insertion was done
            if (res.second) {
                //std::cout << "'" << key << "' populated, renamed to '" << key + std::to_string(counter) << "'" << std::endl;
                return key + std::to_string(counter);
            }
        }
    }
    SymbolValueType remove(std::string key) {

        // find entry
        auto it = map.find(key);

        assert(it != map.end() && "entry with given key was never inserted");

        // get ref to entry
        map_entry_type &entry = it.operator*().second;

        assert(entry.populated && "entry with given key is already removed");

        // set entry as unpopulated
        entry.populated = false;

        return entry.val;
    }
    SymbolValueType get(std::string key) {

        // find entry
        auto it = map.find(key);

        assert(it != map.end() && "entry with given key was never inserted");

        // get entry
        map_entry_type entry = it.operator*().second;

        assert(entry.populated && "entry with key is not populated");

        return entry.val;
    }
};

#endif