/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#ifndef STENCIL_HPP_
#define STENCIL_HPP_

#include <string>
#include <vector>
#include "expressions.hpp"
using namespace std;


class Edge {
  public:
    Edge(Neighbor offset, double coefficient);

    Neighbor getNeighbor() const { return mNeighbor; }
    double getCoefficient() const { return mCoefficient; }

  private:
    Neighbor mNeighbor;
    double mCoefficient;
};

#endif
