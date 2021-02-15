/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "analyzer.hpp"
#include <string>
#include <iostream>
using namespace std;

void trim(string& str) {
    str.erase(str.begin(), find_if(str.begin(), str.end(),
        not1(ptr_fun<int, int>(isspace))));
    str.erase(find_if(str.rbegin(), str.rend(),
        not1(ptr_fun<int, int>(isspace))).base(), str.end());
}


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

    SgFunctionCallExp* call = isSgFunctionCallExp(n);
    if(call != NULL) {
        SgFunctionSymbol* func = call->getAssociatedFunctionSymbol();
        name = func->get_name().getString();
        trim(name);
        if(name.find("environment", 0) == 0 ||
           name.find("schedule", 0) == 0 ||
           name.find("data_", 0) == 0)
        {
            mCalls.push_back(name);
            mNodes.push_back(call);
        }
    }
}



#if 0
void Analyzer::analyze(SgProject *proj) {
    // Find calls to GridGen tool
    FindFunctionCalls v;
    v.traverseInputFiles(proj, preorder);
    
    // Handle calls
    for(int i = 0; i < v.numCalls(); i++) {
        string call = v.getCall(i);
        SgFunctionCallExp *node = v.getCallNode(i);

        /*
        if(call == "neighbor_new") { call_neighborNew(call, node); }
        else if(call == "neighbor_setOffset") {
            call_neighborSetOffset(call, node); }
        else if(call == "subgrid_new") { call_subgridNew(call, node); }
        else if(call == "subgrid_setBound") {
            call_subgridSetBound(call, node); }
        else if(call == "subgrid_addEdges") { call_subgridAddEdge(call, node); }
        else if(call == "data_new") { call_dataNew(call, node); }
        else if(call == "data_applyGrid") { call_dataApplyGrid(call, node); }
        else if(call == "data_add") { call_dataAdd(call, node); }
        else if(call == "data_sub") { call_dataSub(call, node); }
        else if(call == "data_mult") { call_dataMult(call, node); }
        else if(call == "data_pow") { call_dataPow(call, node); } 
        else if(call == "data_sum") { call_dataSum(call, node); }
        else if(call == "data_print") { call_dataPrint(call, node); }
        */
    }

    //return mAnalysis;
}

void Analyzer::call_neighborNew(string call, SgFunctionCallExp* node) {
    cout << "call_neighborNew" << endl;

    /* SgVarRefExp  SgStringVal  SgIntVal  SgIntVal */
    SgExprListExp *args = node->get_args();
    SgVarRefExp *node1 = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));
    SgStringVal *node2 = isSgStringVal(args->get_traversalSuccessorByIndex(1));
    SgIntVal *node3 = isSgIntVal(args->get_traversalSuccessorByIndex(2));

    assert(node1 != NULL && node2 != NULL && node3 != NULL);

    cout << "\t\t ****: " << node1->get_symbol()->get_name().getString()
         << endl;
    cout << "\t\t ****: " << node2->get_value() << endl;
    cout << "\t\t ****: " << node3->get_value() << endl;

    string neighborName = node1->get_symbol()->get_name().getString();

    mAnalysis.mEnvironment.newNeighbor(neighborName);
    //Neighbor &off = mEnvironment.getNeighbor(neighborName);
}

void Analyzer::call_neighborSetOffset(string call, SgFunctionCallExp* node) {
    cout << "call_neighborSetDefault" << endl;

    /* SgVarRefExp  SgAggregateInitializer */
    SgExprListExp *args = node->get_args();
    SgVarRefExp *node1 = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));
    SgAggregateInitializer *node2 =
        isSgAggregateInitializer(args->get_traversalSuccessorByIndex(1));

    assert(node1 != NULL && node2 != NULL);

    cout << "\t\t ****: " << node1->get_symbol()->get_name().getString()
         << endl;
    cout << "\t\t ****: " << node2->unparseToString() << endl;

    string neighborName = node1->get_symbol()->get_name().getString();
    Neighbor &off = mAnalysis.mEnvironment.getNeighbor(neighborName);
    off.setDefault(makeExpressions(node2));
}

void Analyzer::call_subgridNew(string call, SgFunctionCallExp* node) {
    cout << "call_subgridNew" << endl;

    /* SgVarRefExp  SgIntVal  SgIntVal */
    SgExprListExp *args = node->get_args();
    SgVarRefExp *node1 = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));
    SgIntVal *node2 = isSgIntVal(args->get_traversalSuccessorByIndex(1));
    SgIntVal *node3 = isSgIntVal(args->get_traversalSuccessorByIndex(2));

    assert(node1 != NULL && node2 != NULL && node3 != NULL);

    cout << "\t\t ****: " << node1->get_symbol()->get_name().getString()
         << endl;
    cout << "\t\t ****: " << node2->get_value() << endl;
    cout << "\t\t ****: " << node3->get_value() << endl;

    string gridName = node1->get_symbol()->get_name().getString();
    int nDims = node2->get_value();
    double coefficient = 1.0;

    mAnalysis.mEnvironment.newGrid(gridName, node, nDims, coefficient);
}

void Analyzer::call_subgridSetBound(string call, SgFunctionCallExp* node) {
    cout << "call_subgridSetBound" << endl;

    /* SgVarRefExp  SgIntVal  SgVarRefExp */
    SgExprListExp *args = node->get_args();
    SgVarRefExp *node1 = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));
    SgIntVal    *node2 = isSgIntVal(args->get_traversalSuccessorByIndex(1));
    SgVarRefExp *node3 = isSgVarRefExp(args->get_traversalSuccessorByIndex(2));

    assert(node1 != NULL && node2 != NULL && node3 != NULL);

    cout << "\t\t ****: " << node1->get_symbol()->get_name().getString()
         << endl;
    cout << "\t\t ****: " << node2->get_value() << endl;
    cout << "\t\t ****: " << node3->get_symbol()->get_name().getString()
         << endl;

    string gridName = node1->get_symbol()->get_name().getString();
    Grid *g = mAnalysis.mEnvironment.getGrid(gridName);
    int dim = node2->get_value() - 1;
    string val = node3->get_symbol()->get_name().getString();

    g->setBound(dim, val);
}

void Analyzer::call_subgridAddEdge(string call, SgFunctionCallExp* node) {
    cout << "call_subgridAddEdge" << endl;

    /* SgVarRefExp  SgVarRefExp  SgIntVal */
    SgExprListExp *args = node->get_args();
    SgVarRefExp *node1 = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));
    SgVarRefExp *node2 = isSgVarRefExp(args->get_traversalSuccessorByIndex(1));
    SgFunctionCallExp *node3 = isSgFunctionCallExp(args->get_traversalSuccessorByIndex(2));
    Exp *node3Exp = makeExpression(node3);

    assert(node1 != NULL && node2 != NULL && node3 != NULL);

    cout << "\t\t ****: " << node1->get_symbol()->get_name().getString()
         << endl;
    cout << "\t\t ****: " << node2->get_symbol()->get_name().getString()
         << endl;
    cout << "\t\t ****: " << node3->unparseToString() << endl;

    string gridName = node1->get_symbol()->get_name().getString();
    Grid *g = mAnalysis.mEnvironment.getGrid(gridName);
    Neighbor &off = mAnalysis.mEnvironment.getNeighbor(
        node2->get_symbol()->get_name().getString());
    double coefficient = 1; //node3->get_value(); TODO: UPDATE

    g->addEdge(off, coefficient);
}

void Analyzer::call_dataNew(string call, SgFunctionCallExp* node) {
    mAnalysis.mCalls.push_back(GridLibCall(GRIDLIB_DATA_NEW, node));
}

void Analyzer::call_dataApplyGrid(string call, SgFunctionCallExp* node) {
    mAnalysis.mCalls.push_back(GridLibCall(GRIDLIB_DATA_APPLY_GRID, node));
}

void Analyzer::call_dataAdd(string call, SgFunctionCallExp* node) {
    mAnalysis.mCalls.push_back(GridLibCall(GRIDLIB_DATA_ADD, node));
}

void Analyzer::call_dataSub(string call, SgFunctionCallExp* node) {
    mAnalysis.mCalls.push_back(GridLibCall(GRIDLIB_DATA_SUB, node));
}

void Analyzer::call_dataMult(string call, SgFunctionCallExp* node) {
    mAnalysis.mCalls.push_back(GridLibCall(GRIDLIB_DATA_MULT, node));
}

void Analyzer::call_dataPow(string call, SgFunctionCallExp* node) {
    mAnalysis.mCalls.push_back(GridLibCall(GRIDLIB_DATA_POW, node));
}

void Analyzer::call_dataSum(string call, SgFunctionCallExp* node) {
    mAnalysis.mCalls.push_back(GridLibCall(GRIDLIB_DATA_SUM, node));
}

void Analyzer::call_dataPrint(string call, SgFunctionCallExp* node) {
    cout << "call_dataPrint" << endl;

    /* SgVarRefExp  SgVarRefExp  SgIntVal */
    SgExprListExp *args = node->get_args();
    SgVarRefExp *node1 = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));

    assert(node1 != NULL);

    cout << "\t\t ****: " << node1->get_symbol()->get_name().getString()
         << endl;

    mAnalysis.mCalls.push_back(GridLibCall(GRIDLIB_DATA_PRINT, node));
}

/* TODO: Simplify this function, I shouldn't have to hardcode all of this */
Exp *Analyzer::makeExpression(SgFunctionCallExp* node) {
    Exp *exp = new Exp();
    SgFunctionCallExp *left, *right;
    SgIntVal *leftVal;
    string func;
    
    func = node->getAssociatedFunctionSymbol()->get_name().getString();
    
    if(func == "if_") {
        exp->setCommand(CMD_IF);
        left = isSgFunctionCallExp(
            node->get_args()->get_traversalSuccessorByIndex(0));
        assert(left != NULL);
        exp->setLeft(makeExpression(left));
    }
    else if(func == "and_") {
        exp->setCommand(CMD_AND);
        left = isSgFunctionCallExp(
            node->get_args()->get_traversalSuccessorByIndex(0));
        right = isSgFunctionCallExp(
            node->get_args()->get_traversalSuccessorByIndex(1));
        assert(left != NULL && right != NULL);
        exp->setLeft(makeExpression(left));
        exp->setRight(makeExpression(right));
    }
    else if(func == "eq_") {
        exp->setCommand(CMD_EQ);
        left = isSgFunctionCallExp(
            node->get_args()->get_traversalSuccessorByIndex(0));
        right = isSgFunctionCallExp(
            node->get_args()->get_traversalSuccessorByIndex(1));
        assert(left != NULL && right != NULL);
        exp->setLeft(makeExpression(left));
        exp->setRight(makeExpression(right));
    }
    else if(func == "idx_") {
        exp->setCommand(CMD_IDX);
        leftVal = isSgIntVal(
            node->get_args()->get_traversalSuccessorByIndex(0));
        assert(leftVal != NULL);
        exp->setLeft(makeExpression(leftVal->get_value()));
    }
    else if(func == "const_") {
        exp->setCommand(CMD_CONST);
        leftVal = isSgIntVal(
            node->get_args()->get_traversalSuccessorByIndex(0));
        assert(leftVal != NULL);
        exp->setLeft(makeExpression(leftVal->get_value()));
    }
    else if(func == "param_") {
        exp->setCommand(CMD_PARAM);
        leftVal = isSgIntVal(
            node->get_args()->get_traversalSuccessorByIndex(0));
        assert(leftVal != NULL);
        exp->setLeft(makeExpression(leftVal->get_value()));
    }
    else if(func == "minus_") {
        exp->setCommand(CMD_MINUS);
        left = isSgFunctionCallExp(
            node->get_args()->get_traversalSuccessorByIndex(0));
        right = isSgFunctionCallExp(
            node->get_args()->get_traversalSuccessorByIndex(1));
        assert(left != NULL && right != NULL);
        exp->setLeft(makeExpression(left));
        exp->setRight(makeExpression(right));
    }
    else if(func == "plus_") {
        exp->setCommand(CMD_PLUS);
        left = isSgFunctionCallExp(
            node->get_args()->get_traversalSuccessorByIndex(0));
        right = isSgFunctionCallExp(
            node->get_args()->get_traversalSuccessorByIndex(1));
        assert(left != NULL && right != NULL);
        exp->setLeft(makeExpression(left));
        exp->setRight(makeExpression(right));
    }

    return exp;
}

Exp *Analyzer::makeExpression(int value) {
    Exp *exp = new Exp();
    exp->setCommand(value);
}

Exp **Analyzer::makeExpressions(SgAggregateInitializer* node) {
    SgExprListExp *sublist = isSgExprListExp(node->get_initializers());
    assert(sublist != NULL);
    SgExprListExp *list = isSgExprListExp(
        sublist->get_traversalSuccessorByIndex(0));
    assert(list != NULL);

    Exp **exps = new Exp*[list->get_numberOfTraversalSuccessors()];
    for(int i = 0; i < list->get_numberOfTraversalSuccessors(); i++) {
        exps[i] = makeExpression(isSgFunctionCallExp(
            list->get_traversalSuccessorByIndex(i)));
    }

    return exps;
}
#endif
