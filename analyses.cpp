/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "analyses.hpp"
#include "utils.hpp"
#include <string>
#include <iostream>
using namespace std;

// Maps symbols to environmental objects
std::map<std::string, IEnvironmental*> gSymbolTable;




/**
 * This visitor finds nodes that mark calls to subroutines and functions in the
 * GridLib library.
 */
class FindFunctionCalls : public AstSimpleProcessing {
  public:
    virtual void visit(SgNode *n);
    int numCalls() const { return mCalls.size(); }
    string getCall(int num) const { return mCalls[num]; }
    SgFunctionCallExp* getCallNode(int num) const { return mNodes[num]; }
    
  private:
    vector<string> mCalls;
    vector<SgFunctionCallExp*> mNodes;
};

void FindFunctionCalls::visit(SgNode *n) {
    string name;

    // Find all function calls that begin with environment_,
    // schedule__, or data__
    SgFunctionCallExp* call = isSgFunctionCallExp(n);
    if(call != NULL) {
        SgFunctionSymbol* func = call->getAssociatedFunctionSymbol();
        name = func->get_name().getString();
        trim(name);
        if(name.find("neighbor_", 0) == 0 ||
           name.find("subgrid_", 0) == 0 ||
           name.find("grid_", 0) == 0 ||
           name.find("distribution_", 0) == 0 ||
           name.find("schedule_", 0) == 0 ||
           name.find("data_", 0) == 0)
        {
            mCalls.push_back(name);
            mNodes.push_back(call);
        }
    }
}

void CallsAnalysis::analyze(SgProject *proj) {
    // Find calls to GridGen tool
    FindFunctionCalls v;
    v.traverseInputFiles(proj, preorder);
    
    // Iterate through these calls and construct objects that wrap them.
    for(int i = 0; i < v.numCalls(); i++) {
        string call = v.getCall(i);
        SgFunctionCallExp *node = v.getCallNode(i);

        int pos = find(&CALL_IDENT[0], &CALL_IDENT[CALL__NUM_CALLS],
                       call) - &CALL_IDENT[0];
        if(pos != CALL__NUM_CALLS) {
            mCalls.push_back((*CALL_CONSTRUCTOR[pos])(node));
        }
    }

    // Construct environmental objects based on call
    EnvironmentConstructionVisitor envBuilder;
    envBuilder.accept(mCalls);
}

void CallsAnalysis::print(std::ostream &out) const {
    // Iterate through calls
    for(vector<GridLibCall*>::const_iterator i = mCalls.begin();
        i != mCalls.end(); i++)
    {
        (*i)->print(out);
    }
}

Neighbor* CallsAnalysis::getNeighborForFortranVar(const std::string &ftnIdent) const {
    return dynamic_cast<Neighbor*>(gSymbolTable.find(ftnIdent)->second);
}

Subgrid* CallsAnalysis::getSubgridForFortranVar(const std::string &ftnIdent) const {
    return dynamic_cast<Subgrid*>(gSymbolTable.find(ftnIdent)->second);
}

Grid* CallsAnalysis::getGridForFortranVar(const std::string &ftnIdent) const {
    return dynamic_cast<Grid*>(gSymbolTable.find(ftnIdent)->second);
}

Distribution* CallsAnalysis::getDistributionForFortranVar(const std::string &ftnIdent) const {
    return dynamic_cast<Distribution*>(gSymbolTable.find(ftnIdent)->second);
}

Schedule* CallsAnalysis::getScheduleForFortranVar(const std::string &ftnIdent) const {
    return dynamic_cast<Schedule*>(gSymbolTable.find(ftnIdent)->second);
}

DataObject* CallsAnalysis::getDataObjectForFortranVar(const std::string &ftnIdent) const {
    return dynamic_cast<DataObject*>(gSymbolTable.find(ftnIdent)->second);
}


void EnvironmentConstructionVisitor::visit__subgrid_new(
    Call__subgrid_new *call)
{
    Subgrid *sg = Environment::newSubgrid(call->envName());
    sg->setExtents(100, 100);
    sg->setSymbolicExtents(call->width(), call->height());
    gSymbolTable.insert(make_pair(call->lhs(), sg));
}

void EnvironmentConstructionVisitor::visit__grid_new(
    Call__grid_new *call)
{
    Grid *g = Environment::newGrid(call->envName());
    gSymbolTable.insert(make_pair(call->lhs(), g));
}

void EnvironmentConstructionVisitor::visit__grid_addSubgrid(
    Call__grid_addSubgrid *call)
{
    Grid     *g = dynamic_cast<Grid*>(gSymbolTable.find(call->grid())->second);
    Subgrid *sg = dynamic_cast<Subgrid*>(gSymbolTable.find(call->subgrid())->second);
    g->addSubgrid(sg);
}

void EnvironmentConstructionVisitor::visit__grid_addBorder(
    Call__grid_addBorder *call)
{
}

void EnvironmentConstructionVisitor::visit__distribution_new(
    Call__distribution_new *call)
{
    Distribution *dist = Environment::newDistribution(call->envName());
    gSymbolTable.insert(make_pair(call->lhs(), dist));
}

void EnvironmentConstructionVisitor::visit__schedule_new(
    Call__schedule_new *call)
{
    Schedule *sched = Environment::newSchedule(call->envName());
    gSymbolTable.insert(make_pair(call->lhs(), sched));
}

void EnvironmentConstructionVisitor::visit__data_new(
    Call__data_new *call)
{
    DataObject *data = Environment::newDataObject(call->lhs());
    gSymbolTable.insert(make_pair(call->lhs(), data));
}

