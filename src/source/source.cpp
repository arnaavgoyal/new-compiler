#include <iostream>
#include <fstream>

#include "source/source.h"

/** ------------------- SourceLocation ------------------- */

SourceLocation::SourceLocation(
    SourceID si,
    byte_offset_t so,
    byte_offset_t eo,
    rowcol_offset_t sr,
    rowcol_offset_t sc,
    rowcol_offset_t er,
    rowcol_offset_t ec
) { set(si, so, eo, sr, sc, er, ec); }

void SourceLocation::set(
    SourceID si,
    byte_offset_t so,
    byte_offset_t eo,
    rowcol_offset_t sr,
    rowcol_offset_t sc,
    rowcol_offset_t er,
    rowcol_offset_t ec
) {
    src_id       = si;
    start_offset = so;
    end_offset   = eo;
    start_row    = sr;
    start_col    = sc;
    end_row      = er;
    end_col      = ec;
}

void SourceLocation::set_start(
    byte_offset_t so,
    rowcol_offset_t sr,
    rowcol_offset_t sc
) {
    start_offset = so;
    start_row    = sr;
    start_col    = sc;
}

void SourceLocation::set_end(
    byte_offset_t eo,
    rowcol_offset_t er,
    rowcol_offset_t ec
) {
    end_offset   = eo;
    end_row      = er;
    end_col      = ec;
}

SourceLocation const &SourceLocation::operator=(SourceLocation const &other) {
    src_id       = other.src_id;
    start_offset = other.start_offset;
    start_row    = other.start_row;
    start_col    = other.start_col;
    end_offset   = other.end_offset;
    end_row      = other.end_row;
    end_col      = other.end_col;
    return *this;
}

void SourceLocation::copy_src(SourceLocation const &other) {
    src_id = other.src_id;
}

void SourceLocation::copy_start(SourceLocation const &other) {
    start_offset = other.start_offset;
    start_row    = other.start_row;
    start_col    = other.start_col;
}

void SourceLocation::copy_end(SourceLocation const &other) {
    end_offset   = other.end_offset;
    end_row      = other.end_row;
    end_col      = other.end_col;
}

SourceLocation::byte_offset_t SourceLocation::length_in_bytes() {
    return end_offset - start_offset + 1;
}

void SourceLocation::print() {
    std::cout
        << SourceManager::get_source_path(src_id) << " @ "
        << start_row  << ":" << start_col  << "::"
        << end_row    << ":" << end_col;
}

/** ------------------- SourceManager ------------------- */

std::vector<std::string> SourceManager::source_paths = std::vector<std::string>();

SourceID SourceManager::add_source(std::string const &path) {
    source_paths.push_back(path);
    return (source_paths.size());
}

std::ifstream *SourceManager::open_source(SourceID const &src_id) {
    if (src_id.id == 0) {
        return nullptr;
    }
    else if (src_id.id > source_paths.size()) {
        printf("SourceManager --- FATAL ERROR\n");
        exit(EXIT_FAILURE);
    }
    return new std::ifstream(source_paths[src_id.id - 1], std::ios::binary);
}

std::string SourceManager::get_source_path(SourceID const &src_id) {
    if (src_id.id == 0) {
        return std::string("invalid");
    }
    else if (src_id.id > source_paths.size()) {
        printf("SourceManager --- FATAL ERROR\n");
        exit(EXIT_FAILURE);
    }
    return source_paths[src_id.id - 1];
}