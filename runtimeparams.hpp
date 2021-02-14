/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#ifndef RUNTIMEPARAM_HPP_
#define RUNTIMEPARAM_HPP_

/**
 * This class is a singleton that contains parameters specified when
 * the GridGen code generator is invoked.
 */
class RuntimeParams {
  public:
    static int nProcs;          // Number of processors to generate code for
};

#endif

