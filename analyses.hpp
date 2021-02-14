/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#ifndef ANALYSES_HPP_
#define ANALYSES_HPP_

#include "gridLibCall.hpp"

#include <rose.h>
#include <iostream>
using namespace std;

/**
 * Finds expressions in the source code that the code generator needs to
 * somehow address.  Also fills the environment with needed information.
 **/
class CallsAnalysis {
  public:
    CallsVectorT analyze(SgProject *proj);
};

#endif
