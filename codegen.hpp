/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2012 Colorado State University
 *****************************************************************************/
#ifndef CODEGEN_HPP_
#define CODEGEN_HPP_

#include <iostream>
#include <map>
#include <set>
using namespace std;

/**
 * Generate code
 */
class CodeGen {
  public:
    CodeGen();

    void generateApply();

  private:
    void outputPrintCall(GridLibCall call);
    void outputNewDataObject(GridLibCall call);
    void outputGridApplication(GridLibCall call);
    void outputNeighbor(ostream *out, Neighbor off, GridLibCall call);
    void outputNewVariableDeclarations();
    SgNode *getTopOfFunction(SgFunctionDefinition *func);
    void addDeclaration(SgNode *at, string var);
    void unparse(Exp &exp, ostream &out);

    Grid *mpGrid;
    Analysis mAnalysis;
    VarDeclsMap mVarDeclsToAdd;
};

#endif
