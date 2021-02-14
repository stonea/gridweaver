/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/

#include "dataObject.hpp"
#include <iostream>
#include <stdlib.h>
using namespace std;

void DataObject::print(ostream &out) const {
    out << "DataObject " << mName << " {" << endl;
    out << "           grid = ";
        if(mGrid == NULL) {
            out << "NULL";
        } else {
            mGrid->printSimp(out);
        }
        out << endl;
    out << "       schedule = ";
        if(mSchedule == NULL) {
            out << "NULL";
        } else {
            mSchedule->printSimp(out);
        }
        out << endl;
    out << "   distribution = ";
        if(mDistribution == NULL) {
            out << "NULL";
        } else {
            mDistribution->printSimp(out);
        }
        out << endl;
    out << "}" << endl;
}

void DataObject::printSimp(ostream &out) const {
    out << mName;
}

