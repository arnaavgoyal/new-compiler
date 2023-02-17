#include <iostream>
#include <fstream>

#include "source.h"

SourceFileManager::SourceFileManager() {
    this->source_paths = std::vector<std::string>();
    this->sources = std::vector<std::ifstream *>();
}

SourceFileID SourceFileManager::add_source(std::string const &path) {
    this->source_paths.push_back(path);
    this->sources.push_back(new std::ifstream(path));
    return SourceFileID(sources.size() - 1);
}

std::string const SourceFileManager::get_source_path(SourceFileID const &src_id) {
    return source_paths[src_id.id];
}

std::ifstream *SourceFileManager::get_source(SourceFileID const &src_id) {
    return sources[src_id.id];
}