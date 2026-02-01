#ifndef UTILS_MEMORY_H
#define UTILS_MEMORY_H

#include <cassert>
#include <string_view>
#include <unordered_map>
#include <vector>

struct RawRegionAllocator {

    enum { REGION_SIZE = 1 << 16 };

    struct Region {
        char *base;
        uint32_t size;
        uint32_t offset;
    };

    std::vector<Region> regions;

    char *operator()(uint32_t sz) {
        if (sz > REGION_SIZE / 4) {
            // very large alloc, so do it specially
            regions.push_back({ new char[sz], sz, sz });
            if (regions.size() > 1)
                std::swap(regions.back(), regions[regions.size()-2]);
            return regions.back().base;
        }
        if (regions.size() == 0) {
            regions.push_back({ new char[REGION_SIZE], REGION_SIZE, 0 });
        }
        auto &r = regions.back();
        if (r.size - r.offset < sz) {
            regions.push_back({ new char[REGION_SIZE], REGION_SIZE, sz });
            return regions.back().base;
        }
        auto ptr = r.base + r.offset;
        r.offset += sz;
        return ptr;
    }

    RawRegionAllocator() = default;
    RawRegionAllocator(RawRegionAllocator const &) = delete;
    RawRegionAllocator &operator=(RawRegionAllocator const &) = delete;

    ~RawRegionAllocator() {
        for (auto r : regions) {
            delete[] r.base;
        }
    }
};


template <typename T>
class Allocator {
private:

    /** vars for debugging purposes */
    static int count;
    int id;

    RawRegionAllocator &a;
    std::vector<T *> objs;

public:

    Allocator(RawRegionAllocator &a) : a(a) {
        count++;
        id = count;
    }

    template <typename... Args>
    T *operator()(Args &&... args) {
        auto buf = a(sizeof(T));
        auto obj = new (buf) T(std::forward<Args>(args)...);
        objs.push_back(obj);
        return obj;
    }

    /**
     * Destructor.
    */
    ~Allocator() {
        for (auto obj : objs) {
            obj->~T();
        }
    }

    /**
     * Prevent copying to maintain uniqueness of allocators.
    */
    Allocator(Allocator const &) = delete;
    Allocator &operator=(Allocator const &) = delete;

};

/** Must define in header so template funcs will work */
template <typename T>
int Allocator<T>::count = 0;


struct StringPool {
    RawRegionAllocator &a;
    std::unordered_map<std::string_view, std::string_view> m;

    StringPool(RawRegionAllocator &a) : a(a) { }
    std::string_view add(char const *literal) {
        // since the string is a literal, we don't need to alloc
        std::string_view sv(literal);
        m[sv] = sv;
        return sv;
    }
    std::string_view get(std::string_view s) {
        auto it = m.find(s);
        if (it != m.end()) { return it->second; }
        auto str = (char *)std::memcpy(a(s.size()), s.data(), s.size());
        std::string_view sv = { str, s.size() };
        m[sv] = sv;
        return sv;
    }
};

#endif
