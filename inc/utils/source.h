#ifndef UTILS_SOURCE_H
#define UTILS_SOURCE_H

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <string>
#include <vector>

#include "utils/iterator.h"
#include "utils/memory.h"

struct SourceFile {
    uint32_t base;
    std::string path;
    std::string_view content;
    std::vector<uint32_t> lines;
};

struct SourceLocation {

    uint32_t start, len;

    friend SourceLocation operator>>(SourceLocation x, SourceLocation y);
    SourceLocation &operator>>=(SourceLocation y);

    friend SourceLocation operator^(SourceLocation x, SourceLocation y) {
        auto start = x.start + x.len;
        assert(start <= y.start);
        return { start, y.start - start };
    }
};

struct ExpandedSourceLocation {

    using byte_offset_t = uint32_t;
    using rowcol_offset_t = uint32_t;

    SourceFile *src;

    byte_offset_t start_offset;
    byte_offset_t end_offset;

    rowcol_offset_t start_row;
    rowcol_offset_t start_col;

    rowcol_offset_t end_row;
    rowcol_offset_t end_col;
};


struct SourceManager {
private:
    uint32_t back = 0;
    RawRegionAllocator &a;
    std::vector<SourceFile *> files;
    std::unordered_map<std::string, unsigned> lookup;

    SourceManager(RawRegionAllocator &a) : a(a) { }
    ~SourceManager() {
        for (auto file : files) {
            delete file;
        }
    }

    SourceFile *_get(std::string path) {
        auto it = lookup.find(path);
        if (it != lookup.end()) return files[it->second];

        auto filesz = std::filesystem::file_size(path);
        auto bufsz = filesz + 1; // for EOF
        auto buf = a(bufsz);
        auto file = fopen(path.c_str(), "rb");
        auto nread = fread(buf, sizeof(char), filesz, file);
        assert(nread == filesz);
        buf[filesz] = END_OF_FILE;

        lookup[path] = files.size();
        files.push_back(new SourceFile{ back, path, { buf, bufsz }, { 0 } });
        back += bufsz;
        char const *chr = buf, *end = buf + filesz;
        while ((chr = (char *)std::memchr(chr, '\n', end - chr))) {
            files.back()->lines.push_back(chr - buf + 1);
            chr++;
        }

        return files.back();
    }

    ExpandedSourceLocation _expand(SourceLocation sloc) {
        auto it = std::upper_bound
            ( files.begin()
            , files.end()
            , sloc.start
            , [](uint32_t const &offset, SourceFile *const &src){
                return offset < src->base;
            }
        );
        assert(it != files.begin());
        SourceFile *src = *std::prev(it);
        
        assert(sloc.start >= src->base);
        auto offset_into_file = sloc.start - src->base;
        auto it2 = std::upper_bound
            ( src->lines.begin()
            , src->lines.end()
            , offset_into_file
        );
        assert(it2 != src->lines.begin());

        auto line_start_it = std::prev(it2);
        auto start_row = std::distance(src->lines.begin(), line_start_it) + 1;

        auto end_offset_into_file = offset_into_file + sloc.len;
        auto it3 = std::upper_bound(it2, src->lines.end(), end_offset_into_file);
        assert(it3 != src->lines.begin());

        auto line_end_it = std::prev(it3);
        auto end_row = std::distance(src->lines.begin(), line_end_it) + 1;

        ExpandedSourceLocation esl
            { src
            , offset_into_file
            , offset_into_file + sloc.len - 1 // we dont want one-past-the-end
            , (uint32_t)start_row
            , offset_into_file - *line_start_it + 1
            , (uint32_t)end_row
            , end_offset_into_file - *line_end_it
            }
        ;
        return esl;
    }


    static SourceManager *me;

public:
    enum { END_OF_FILE = '\0' };

    static void init(RawRegionAllocator &a) {
        assert(!me && "already init'd");
        me = new SourceManager(a);
    }
    static SourceFile *get(std::string path) {
        assert(me && "not init'd yet");
        return me->_get(path);
    }
    static ExpandedSourceLocation expand(SourceLocation sloc) {
        assert(me && "not init'd yet");
        return me->_expand(sloc);
    }
};

#endif
