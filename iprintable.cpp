/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/

#include "iprintable.hpp"

void initializeModule_iprintable() {
}

std::ostream& operator<<(std::ostream& os, const IPrintable& val) {
    val.printSimp(os);
    return os;
}

std::ostream& operator<<(std::ostream& os, const IPrintable *val) {
    val->printSimp(os);
    return os;
}

