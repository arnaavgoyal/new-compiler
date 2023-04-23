#ifndef SOURCE_H
#define SOURCE_H

#include <vector>
#include <string>
#include <fstream>

/** ------------------- DECLARATIONS ------------------- */

class SourceID;
class SourceManager;
class SourceLocation;

/** ------------------- CLASS DEFINITIONS ------------------- */

class SourceID {

    // SourceManager creates SourceIDs as an opaque object
    friend class SourceManager;

private:

    int id;
    SourceID(int id) { this->id = id; }

public:

    SourceID() { this->id = -1; }

};

class SourceLocation {
public:

    using byte_offset_t = unsigned long int;
    using rowcol_offset_t = unsigned int;

    /** source file */
    SourceID src_id;

    /** byte offset of first byte */
    byte_offset_t start_offset;

    /** byte offset of last byte */
    byte_offset_t end_offset;

    /** FOR USER OUTPUT */

    /** row of first character */
    rowcol_offset_t start_row;
    /** col of first character */
    rowcol_offset_t start_col;

    /** row of last character */
    rowcol_offset_t end_row;
    /** col of last character */
    rowcol_offset_t end_col;

    /**
     * Constructs empty (invalid) SourceLocation.
    */
    SourceLocation() :
        src_id(),
        start_offset(0),
        end_offset(0),
        start_row(0),
        start_col(0),
        end_row(0),
        end_col(0)
    { }

    /**
     * Constructor for all fields.
    */
    SourceLocation(
        SourceID si,
        byte_offset_t so,
        byte_offset_t eo,
        rowcol_offset_t sr,
        rowcol_offset_t sc,
        rowcol_offset_t er,
        rowcol_offset_t ec
    );

    /**
     * Set all fields.
    */
    void set(
        SourceID si,
        byte_offset_t so,
        byte_offset_t eo,
        rowcol_offset_t sr,
        rowcol_offset_t sc,
        rowcol_offset_t er,
        rowcol_offset_t ec
    );

    /**
     * Set start fields.
    */
    void set_start(
        byte_offset_t so,
        rowcol_offset_t sr,
        rowcol_offset_t sc
    );

    /**
     * Set end fields.
    */
    void set_end(
        byte_offset_t eo,
        rowcol_offset_t er,
        rowcol_offset_t ec
    );

    /**
     * Copy all fields from another src loc.
    */
    SourceLocation const &operator=(SourceLocation const &other);

    /**
     * Copy source from another src loc.
    */
    void copy_src(SourceLocation const &other);

    /**
     * Copy start fields from another src loc.
    */
    void copy_start(SourceLocation const &other);

    /**
     * Copy end fields from another src loc.
    */
    void copy_end(SourceLocation const &other);

    /**
     * Get byte length of source loc.
     * 
     * @return the length in bytes
    */
    byte_offset_t length_in_bytes();

    /**
     * Print the src loc.
    */
    void print();

};

class SourceManager {
private:
    
    /** list of paths of all sources added */
    static std::vector<std::string> source_paths;

public:
    
    /**
     * Adds a source with given path and returns the corresponding source ID.
     * 
     * @param path the path
     * @return the source ID
    */
    static SourceID add_source(std::string const &path);

    /**
     * Dynamically allocates a stream and opens it on the given source.
     * 
     * The caller of this function takes responsibility of the stream and
     * must deallocate it when done.
     * 
     * @param src_id the ID of the source to open
     * @return pointer to opened stream
    */
    static std::ifstream *open_source(SourceID const &src_id);

    /**
     * Gets the path of the source with the given source ID.
     * 
     * @param src_id the ID of the source
     * @return the path
    */
    static std::string get_source_path(SourceID const &src_id);

};

#endif