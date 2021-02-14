/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/

#include "codegen.hpp"

#include "utils.hpp"
#include <string>
using namespace std;

CodeGen::CodeGen(Analysis analysis) {
    mAnalysis = analysis;
    // TODO: Change to not use this call
    mpGrid = analysis.getEnv().getOnlyGrid(); 
}

void CodeGen::generate() {
    // Loop through gridLib calls
    for(CallsIterator i = mAnalysis.callsBegin();
        i != mAnalysis.callsEnd(); i++)
    {
        switch(i->mOperation) {
            case GRIDLIB_GRID_VISUALIZE:
                cout << "GRIDLIB_GRID_VISUALIZE" << endl;
            break;

            case GRIDLIB_DATA_NEW:
                outputNewDataObject(*i);
            break;

            case GRIDLIB_DATA_INPUT:
                cout << "GRIDLIB_DATA_INPUT" << endl;
            break;

            case GRIDLIB_DATA_OUTPUT:
                cout << "GRIDLIB_DATA_OUTPUT" << endl;
            break;

            case GRIDLIB_DATA_SET:
                cout << "GRIDLIB_DATA_SET" << endl;
            break;

            case GRIDLIB_DATA_APPLY_GRID:
                outputGridApplication(*i);
            break;

            case GRIDLIB_DATA_ADD:
                cout << "GRIDLIB_DATA_ADD" << endl;
            break;

            case GRIDLIB_DATA_SUB:
                cout << "GRIDLIB_DATA_SUB" << endl;
            break;

            case GRIDLIB_DATA_MULT:
                cout << "GRIDLIB_DATA_MULT" << endl;
            break;

            case GRIDLIB_DATA_POW:
                cout << "GRIDLIB_DATA_POW" << endl;
            break;

            case GRIDLIB_DATA_SUM:
                cout << "GRIDLIB_DATA_SUM" << endl;
            break;

            case GRIDLIB_DATA_PRINT:
                outputPrintCall(*i);
            break;
        }
    }

    // Output varaible declarations
    outputNewVariableDeclarations();
}

void CodeGen::outputPrintCall(GridLibCall call) {
    /* TODO: Make this output local blocks, not grids! */
    /* TODO: Generalize this to work with more than two dimensions! */
    
    ostringstream ss;
    SgFunctionCallExp *location = call.mLocation;
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());
    
    // Place new code in a string
    ss << pushIndt;
    for(int i = 0; i < mpGrid->getDims(); i++) {
        ss << indt << "do idx_" << i << " = ";
        if(i == 0) {
            ss << mpGrid->getBound(i) << ", 1, -1";
        } else {
            ss << "1, " << mpGrid->getBound(i);
        }
        ss << pushIndt << endl;
        addDeclaration(parent, "integer idx_" + str(i));
    }
    
    ss << indt << "write (*,\"(f12.3)\", advance=\"no\") &" << endl;
    ss << indt << "    " << call.getObjParam(0) << "%values((idx_1-1) * "
       << mpGrid->getBound(1) << " + idx_0" << ")" << endl;
    
    for(int i = 0; i < mpGrid->getDims(); i++) {
        if(i != 0) {
            ss << indt << "print *, \" \"" << endl;
        }
        ss << popIndt << indt << "end do" << endl;
    }
    
    ss << endl;
    
    // Insert new code
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    
    // Remove old code
    SageInterface::replaceStatement(parent, newStmt);
}

void CodeGen::outputNewDataObject(GridLibCall call) {
    string self = call.getObjParam(0);
    ostringstream ss;
    SgFunctionCallExp *location = call.mLocation;
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());
    addDeclaration(parent, "integer idx_0");
    addDeclaration(parent, "integer numElements");
    
    // Place new code in a string
    ss << pushIndt;
    
    #if 0
    self%grid => g
    numElements = 1
    do i=1,subgrid_numDims(self%grid)
        numElements = numElements * subgrid_bound(self%grid, i)
    end do
    
    allocate(self%values(numElements))
    #endif
    
    ss << indt << "numElements = 1" << endl;
    for(int i = 0; i < mpGrid->getDims(); i++) {
        ss << indt << "numElements = numElements * "
           << mpGrid->getBound(i) << endl;
    }
    ss << indt << self << "\%size = numElements" << endl;
    ss << indt << "allocate(" << self <<"%values(numElements))" << endl;
    ss << popIndt << endl;
    
    // Insert new code
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    
    // Remove old code
    SageInterface::replaceStatement(parent, newStmt);
}

void CodeGen::outputGridApplication(GridLibCall call) {
    ostringstream ss;
    string result = call.getObjLeftOfAssign();
    string data = call.getObjParam(0);
    SgFunctionCallExp *location = call.mLocation;
    SgAssignOp *assignment = isSgAssignOp(location->get_parent());
    assert(assignment != NULL);
    SgExprStatement *parent = isSgExprStatement(assignment->get_parent());
    assert(parent != NULL);
    
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());
    
    /* Place new code in a string */
    ss << pushIndt;
    addDeclaration(parent, "integer pt_center");
    for(int i = 0; i < mpGrid->getDims(); i++) {
        addDeclaration(parent, "integer idx_" + str(i));
    }
    
    // Output loops
    for(int i = 0; i < mpGrid->getDims(); i++) {
        ss << indt << "do idx_" << i << " = 1, "
             << mpGrid->getBound(i) << endl;
        ss << pushIndt;
    }

    ss << "pt_center = AT(";
    for(int i = 0; i < mpGrid->getDims(); i++) {
        if(i != 0) { ss << ", "; }
        ss << "idx_" << i;
    }
    ss << ", N)";

    cout << "Number of edges in grid: " << mpGrid->getNumEdges()
         << endl;
    cout << "Name of grid: " << mpGrid->getName() << endl;
    
    // Iterate through offets and generate code for them*/
    for(int i = 0; i < mpGrid->getNumEdges(); i++) {
        Neighbor off = mpGrid->getEdge(i).getNeighbor();
        outputNeighbor(&ss, off, call);
    }
    
    // Generate stencil
    ss << indt << result << "%values(pt_center) = "
       << mpGrid->getCoefficient() << " * (";
    for(int i = 0; i < mpGrid->getNumEdges(); i++) {
        Edge term = mpGrid->getEdge(i);
        Neighbor off = term.getNeighbor();
        if(i != 0) { ss << " + "; }
        ss << data << "%values(pt_" << off.getName() << ") * "
             << term.getCoefficient();
    }

   ss << ")" << endl;
    
    // Close loops
    for(int i = 0; i < mpGrid->getDims(); i++) {
        ss << popIndt;
        ss << indt << "enddo" << endl;
    }

    ss << popIndt;

    /* Insert new code */
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    
    /* Remove old code */
    SageInterface::replaceStatement(parent, newStmt);
}

void CodeGen::outputNeighbor(ostream *out, Neighbor off, GridLibCall call) {
    addDeclaration(call.mLocation, "integer pt_" + off.getName());

    if(off.numConditions() > 0) {
        for(int i = 0; i < off.numConditions(); i++) {
            *out << indt << "if(";
            unparse(*off.getCondition(i), *out);
            *out << ") then " << endl << pushIndt;
            *out << indt << "pt_" << off.getName() << " = AT("
                 << "(/ N,N /)" << ", (/ ";
            for(int j = 0; j < mpGrid->getDims(); j++) {
                if(j != 0) { *out << ", "; }
                unparse(*off.getResult(i)[j], *out);
            }
            *out << " /))" << endl << popIndt;
            *out << indt << "else \n" << pushIndt;
        }
    }
    *out << indt << "pt_" << off.getName() << " = AT(";
    for(int i = 0; i < mpGrid->getDims(); i++) {
        if(i != 0) { *out << ", "; }
        unparse(*off.getDefault()[i], *out);
    }
    *out << ", N)";    // TODO: Generalize to not use N
    if(off.numConditions() > 0) {
        *out << popIndt << indt << "endif\n \n";
    } else {
        *out << " \n";
    }
}

void CodeGen::unparse(Exp &exp, ostream &out) {
    cout << exp.toString() << endl;

    if(exp.getCommand() == CMD_IF) {
        unparse(*exp.getLeft(), out);
    }
    else if(exp.getCommand() == CMD_AND) {
        out << "(";
        unparse(*exp.getLeft(), out);
        out << ") .and. (";
        unparse(*exp.getRight(), out);
        out << ")";
    }
    else if(exp.getCommand() == CMD_EQ) {
        out << "(";
        unparse(*exp.getLeft(), out);
        out << ") .eq. (";
        unparse(*exp.getRight(), out);
        out << ")";
    }
    else if(exp.getCommand() == CMD_IDX) {
        out << "idx_" << exp.getLeft()->getCommand() - 1;
    }
    else if(exp.getCommand() == CMD_CONST) {
        out << exp.getLeft()->getCommand();
    }
    else if(exp.getCommand() == CMD_PARAM) {
        out << mpGrid->getBound(exp.getLeft()->getCommand() - 1);
    }
    else if(exp.getCommand() == CMD_MINUS) {
        out << "(";
        unparse(*exp.getLeft(), out);
        out << ") - (";
        unparse(*exp.getRight(), out);
        out << ")";
    }
    else if(exp.getCommand() == CMD_PLUS) {
        out << "(";
        unparse(*exp.getLeft(), out);
        out << ") + (";
        unparse(*exp.getRight(), out);
        out << ")";
    }
}

void CodeGen::outputNewVariableDeclarations() {
    for(VarDeclsMap::iterator i = mVarDeclsToAdd.begin();
        i != mVarDeclsToAdd.end(); i++)
    {
        SageInterface::addTextForUnparser(
                getTopOfFunction(i->first), "\n",
                    AstUnparseAttribute::e_after);

        for(set<string>::iterator j = i->second.begin();
            j != i->second.end(); j++)
        {
            SageInterface::addTextForUnparser(
                getTopOfFunction(i->first), *j + "\n",
                    AstUnparseAttribute::e_after);
        }
    }
}

SgNode *CodeGen::getTopOfFunction(SgFunctionDefinition *func) {
    SgBasicBlock *bb = func->get_body();
    
    // If there is an 'implicit' statement insert after that
    for(SgStatementPtrList::iterator i = bb->get_statements().begin();
        i != bb->get_statements().end(); i++)
    {
        if(isSgImplicitStatement(*i) != NULL) {
            return *i;
        }
    }

    return bb;
}

void CodeGen::addDeclaration(SgNode *at, string var) {
    while(isSgFunctionDefinition(at) == NULL) {
        at = at->get_parent();
    }
    mVarDeclsToAdd[isSgFunctionDefinition(at)].insert(var);
}

