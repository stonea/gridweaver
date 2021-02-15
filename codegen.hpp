/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2012 Colorado State University
 *****************************************************************************/
#ifndef CODEGEN_HPP_
#define CODEGEN_HPP_

#include "analyses.hpp"

#include <iostream>
#include <map>
#include <set>
using namespace std;

/**
 * Generate code
 */
class CodeGen {
  public:
    CodeGen(CallsAnalysis *analysis);

    /**
     * Apply optimizations using the GridWeaver code-generator
     */
    void apply();

  private:
    CallsAnalysis *mpCalls;
};



class DataApplyGenerator : public CallVisitor {
  private:
    CallsAnalysis *mpCalls;

  public:
    DataApplyGenerator(CallsAnalysis *calls) :
        mpCalls(calls)
    { }

    virtual void visit__data_apply1(Call__data_apply1 *call);
    virtual void visit__data_apply2(Call__data_apply2 *call);
    virtual void visit__data_apply3(Call__data_apply3 *call);
    virtual void visit__data_apply4(Call__data_apply4 *call);
    virtual void visit__data_apply5(Call__data_apply5 *call);
    virtual void visit__data_apply6(Call__data_apply6 *call);
    virtual void visit__data_apply7(Call__data_apply7 *call);
    virtual void visit__data_apply8(Call__data_apply8 *call);
    virtual void visit__data_apply9(Call__data_apply9 *call);
    virtual void visit__data_apply10(Call__data_apply10 *call);
};

#endif
