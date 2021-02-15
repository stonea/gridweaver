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
class CallsAnalysis : public IPrintable {
  public:
    void analyze(SgProject *proj);

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<CallsAnalysis>";
    }

    CallsVectorT& calls() { return mCalls; }

    Neighbor*     getNeighborForFortranVar(const std::string &ftnIdent) const;
    Subgrid*      getSubgridForFortranVar(const std::string &ftnIdent) const;
    Grid*         getGridForFortranVar(const std::string &ftnIdent) const;
    Distribution* getDistributionForFortranVar(const std::string &ftnIdent) const;
    Schedule*     getScheduleForFortranVar(const std::string &ftnIdent) const;
    DataObject*   getDataObjectForFortranVar(const std::string &ftnIdent) const;

  private:
    //Environment mEnvironment;
    CallsVectorT mCalls;
};

class EnvironmentConstructionVisitor : public CallVisitor {
  public:
    virtual void visit__subgrid_new(Call__subgrid_new *call);
    virtual void visit__grid_new(Call__grid_new *call);
    virtual void visit__grid_addSubgrid(Call__grid_addSubgrid *call);
    virtual void visit__grid_addBorder(Call__grid_addBorder *call);
    virtual void visit__distribution_new(Call__distribution_new *call);
    virtual void visit__schedule_new(Call__schedule_new *call);
    virtual void visit__data_new(Call__data_new *call);
};

#endif
