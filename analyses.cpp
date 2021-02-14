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
        if(name.find("environment_", 0) == 0 ||
           name.find("schedule_", 0) == 0 ||
           name.find("data_", 0) == 0 ||
           name.find("file_", 0) == 0 ||
           name.find("string_", 0) == 0)
        {
            mCalls.push_back(name);
            mNodes.push_back(call);
        }
    }
}



CallsVectorT  CallsAnalysis::analyze(SgProject *proj) {
    CallsVectorT results;

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
            results.push_back((*CALL_CONSTRUCTOR[pos])(node));
        }
    }

    return results;
}
