#ifndef SOURCE_H
#define SOURCE_H

#include <vector>
#include <string>
#include <fstream>

class SourceFileManager;

class SourceFileID {
    friend SourceFileManager;
private:
    int id;
public:
    SourceFileID() { this->id = -1; }
    SourceFileID(int id) { this->id = id; }
};

class SourceFileManager {
private:
    std::vector<std::string> source_paths;
    std::vector<std::ifstream *> sources;
public:
    SourceFileManager();
    SourceFileID add_source(std::string const &path);
    std::string const get_source_path(SourceFileID const &src_id);
    std::ifstream *get_source(SourceFileID const &src_id);
};

#endif