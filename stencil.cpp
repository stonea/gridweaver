/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "stencil.hpp"
#include <iostream>
using namespace std;

Neighbor::Neighbor() {
    mName = "UNKNOWN";
}

Neighbor::Neighbor(string name) {
    setName(name);
}

void Neighbor::setName(string name) {
    mName = name;
}

void Neighbor::addCondition(Exp *condition, Exp **result) {
    mConditions.push_back(condition);
    mResults.push_back(result);
}

void Neighbor::setDefault(Exp **dflt) {
    mDefault = dflt;
}

void Neighbor::print() {
    //cout << "Neighbor: " << mName << endl;
    //for(int i = 0; i < numConditions(); i++) {
    //    cout << mConditions[i] << ": " << mResults[i];
    //}
    //cout << "Default: " << mDefault << endl;
}

Edge::Edge(Neighbor offset, double coefficient) {
    mNeighbor = offset;
    mCoefficient = coefficient;
}

