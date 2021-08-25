/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "binIO.hpp"

// random comment

void initializeModule_binio() {
}

void BinIO::out(std::ostream &out, const int &val) {
    out.write((const char*)&val, sizeof(int));
}

void BinIO::out(std::ostream &out, const double &val) {
    out.write((const char*)&val, sizeof(double));
}

void BinIO::out(std::ostream &out, const bool &val) {
    out.put((val ? 'T' : 'F'));
}

void BinIO::out(std::ostream &out, const std::string &s) {
    out.write(s.c_str(), s.size() + 1);
}

void BinIO::out(std::ostream &out, const ISerializable &obj) {
    obj.output(out);
}

void BinIO::outIdent(std::ostream &out, const IEnvironmental &obj) {
    BinIO::out(out, obj.getID());
}

void BinIO::in(std::istream &in, int &val) {
    in.read((char*)&val, sizeof(int));
}

void BinIO::in(std::istream &in, double &val) {
    in.read((char*)&val, sizeof(double));
}

void BinIO::in(std::istream &in, bool &val) {
    char c;
    in.get(c);
    if(c == 'T') { val = true; } else { val = false; }
}

void BinIO::in(std::istream &in, std::string &s) {
    char c;
    s = "";
    while(true) {
        in.get(c);
        if(c == '\0') { return; }
        s += c;
    }
}

void BinIO::in(std::istream &in, ISerializable &obj) {
    obj.input(in);
}

