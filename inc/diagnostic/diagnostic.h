#ifndef ERROR_H
#define ERROR_H

#include <stdlib.h>
#include "source/source.h"
#include <vector>
#include <cassert>
#include <iostream>
#include <string.h>
#include <string>

struct RawDiagnostic;

namespace diag {

enum class severity {

    fatal,
    error,

    _uncompilable,

    warning,
    note
};

enum class id {

#define DIAGNOSTIC(name, severity, str) name,
#include "diagnostic/diagdefs"

    __end
};

RawDiagnostic get(id diag_id);

}

struct RawDiagnostic {
    diag::severity sev;
    char const *str;
};

class Diagnostic {
private:
    friend class DiagnosticBuilder;
    friend class DiagnosticHandler;
    diag::severity sev;
    char const *formatstr = nullptr;
    std::string finalstr;
    std::vector<std::string> args;
    std::vector<SourceLocation> locs;

public:
    Diagnostic() = default;
    void fmt();
};

class DiagnosticBuilder {
private:
    Diagnostic d;
    bool valid = true;

public:
    DiagnosticBuilder(diag::id id) {
        auto rd = diag::get(id);
        d.sev = rd.sev;
        d.formatstr = rd.str;
    }
    DiagnosticBuilder &add(SourceLocation loc) {
        assert(valid);
        d.locs.push_back(loc);
        return *this;
    }
    DiagnosticBuilder &add(std::string str) {
        assert(valid);
        d.args.push_back(str);
        return *this;
    }
    DiagnosticBuilder &add(char const *str) {
        assert(valid);
        d.args.push_back(str);
        return *this;
    }
    void finish();
};

class DiagnosticHandler {
private:
    static std::vector<Diagnostic> diags;

    friend DiagnosticBuilder;
    static void handle(Diagnostic &&d) {
        diags.push_back(std::move(d));
        // if (d.sev == diag::severity::fatal) {
        //     prog_exit();
        // }
    }

public:
    static DiagnosticBuilder make(diag::id id, SourceLocation caretloc) {
        auto db = DiagnosticBuilder(id);
        db.add(caretloc);
        return db;
    }
    static void print_diag(Diagnostic &diag);
    static unsigned dump();
    static void prog_exit();
};

#endif
