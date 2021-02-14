/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "gridLibCall.hpp"

#include "utils.hpp"
#include <iostream>
#include <string>
#include "environment.hpp"
using namespace std;


const std::string CALL_IDENT[CALL__NUM_CALLS] = {
    "string_new",
    "string_init",

    "file_open",

    "subgrid_print",
    "subgrid_printSimp",
    "subgridptrvec_print",

    "grid_print",
    "grid_printSimp",

    "distribution_print",
    "distribution_printSimp",

    "environment_input",

    "schedule_newFromFile",
    "schedule_input",
    "schedule_print",
    "schedule_printSimp",
  
    "data_new",
    "data_newMedataCopied",
    "data_print",
    "data_printSimp",
    "data_printForProc",

    "data_input",
    "data_forceUpdate",
    "data_apply",
    "data_add",
    "data_sub",
    "data_mult",
    "data_pow",
    "data_sum"
};


GridLibCall* (*CALL_CONSTRUCTOR[CALL__NUM_CALLS])(SgFunctionCallExp *node) =
{
    constructCall__string_new,
    constructCall__string_init,

    constructCall__file_open,

    constructCall__subgrid_print,
    constructCall__subgrid_printSimp,
    constructCall__subgridptrvec_print,

    constructCall__grid_print,
    constructCall__grid_printSimp,

    constructCall__distribution_print,
    constructCall__distribution_printSimp,

    constructCall__environment_input,

    constructCall__schedule_newFromFile,
    constructCall__schedule_input,
    constructCall__schedule_print,
    constructCall__schedule_printSimp,
  
    constructCall__data_new,
    constructCall__data_newMetaDataCopied,
    constructCall__data_print,
    constructCall__data_printSimp,
    constructCall__data_printForProc,

    constructCall__data_input,
    constructCall__data_forceUpdate,
    constructCall__data_apply,
    constructCall__data_add,
    constructCall__data_sub,
    constructCall__data_mult,
    constructCall__data_pow,
    constructCall__data_sum
};



GridLibCall* constructCall__string_new(SgFunctionCallExp *node) {
    return new Call__string_new(node);
}


GridLibCall* constructCall__string_init(SgFunctionCallExp *node) {
    // Extract the call's arguments: (SgVarRefExp, SgStringVal)
    SgExprListExp *args = node->get_args();
    assert(args != NULL);

    SgVarRefExp *var = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));
    assert(var != 0);
    string node1 = var->get_symbol()->get_name().getString();

    SgStringVal *node2 = isSgStringVal(args->get_traversalSuccessorByIndex(1));
    assert(node2 != NULL);

    return new Call__string_init(node, 
        node1, node2->get_value());
}


GridLibCall* constructCall__file_open(SgFunctionCallExp *node) {
    // Extract the call's arguments: (SgVarRefExp, SgVarRefExp)
    SgExprListExp *args = node->get_args();
    assert(args != NULL);

    SgVarRefExp *var = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));
    assert(var != 0);
    string node1 = var->get_symbol()->get_name().getString();

    var = isSgVarRefExp(args->get_traversalSuccessorByIndex(1));
    assert(var != 0);
    string node2 = var->get_symbol()->get_name().getString();

    return new Call__file_open(node, node1, node2);
}


GridLibCall* constructCall__subgrid_print(SgFunctionCallExp *node) {
    return new Call__subgrid_print(node);
}

GridLibCall* constructCall__subgrid_printSimp(SgFunctionCallExp *node) {
    return new Call__subgrid_printSimp(node);
}

GridLibCall* constructCall__subgridptrvec_print(SgFunctionCallExp *node) {
    return new Call__subgridptrvec_print(node);
}


GridLibCall* constructCall__grid_print(SgFunctionCallExp *node) {
    return new Call__grid_print(node);
}

GridLibCall* constructCall__grid_printSimp(SgFunctionCallExp *node) {
    return new Call__grid_printSimp(node);
}


GridLibCall* constructCall__distribution_print(SgFunctionCallExp *node) {
    return new Call__distribution_print(node);
}

GridLibCall* constructCall__distribution_printSimp(SgFunctionCallExp *node) {
    return new Call__distribution_printSimp(node);
}

GridLibCall *constructCall__environment_input(SgFunctionCallExp *node) {
    // Extract the call's arguments: (SgVarRefExp, SgVarRefExp)
    SgExprListExp *args = node->get_args();
    assert(args != NULL);

    SgVarRefExp *var = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));
    assert(var != 0);
    string node1 = var->get_symbol()->get_name().getString();
    
    return new Call__environment_input(node, node1);
}

GridLibCall* constructCall__schedule_newFromFile(SgFunctionCallExp *node) {
    return new Call__schedule_newFromFile(node);
}

GridLibCall* constructCall__schedule_input(SgFunctionCallExp *node) {
    return new Call__schedule_input(node);
}

GridLibCall* constructCall__schedule_print(SgFunctionCallExp *node) {
    return new Call__schedule_print(node);
}

GridLibCall* constructCall__schedule_printSimp(SgFunctionCallExp *node) {
    return new Call__schedule_printSimp(node);
}


GridLibCall* constructCall__data_new(SgFunctionCallExp *node) {
    // Extract the call's arguments: (Data, Grid, Sched, Dist)
    SgExprListExp *args = node->get_args();
    assert(args != NULL);

    SgVarRefExp *var = isSgVarRefExp(args->get_traversalSuccessorByIndex(0));
    assert(var != 0);
    string node1 = var->get_symbol()->get_name().getString();

    EnvRef node2(args->get_traversalSuccessorByIndex(1));
    EnvRef node3(args->get_traversalSuccessorByIndex(2));
    EnvRef node4(args->get_traversalSuccessorByIndex(3));

    cout << endl << endl;
    printNodeAndChildren(cout, node);
    cout << endl << endl;

    return new Call__data_new(node, node1, node2, node3, node4);
}

GridLibCall* constructCall__data_newMetaDataCopied(SgFunctionCallExp *node) {
    return new Call__data_newMetaDataCopied(node);
}

GridLibCall* constructCall__data_print(SgFunctionCallExp *node) {
    return new Call__data_print(node);
}

GridLibCall* constructCall__data_printSimp(SgFunctionCallExp *node) {
    return new Call__data_printSimp(node);
}

GridLibCall* constructCall__data_printForProc(SgFunctionCallExp *node) {
    return new Call__data_printForProc(node);
}


GridLibCall* constructCall__data_input(SgFunctionCallExp *node) {
    return new Call__data_input(node);
}

GridLibCall* constructCall__data_forceUpdate(SgFunctionCallExp *node) {
    return new Call__data_forceUpdate(node);
}

GridLibCall* constructCall__data_apply(SgFunctionCallExp *node) {
    // Extract the call's arguments: (SgVarRefExp, SgFunctionRefExp)
    SgExprListExp *args = node->get_args();
    assert(args != NULL);
    EnvRef node1(args->get_traversalSuccessorByIndex(0));
    SgFunctionRefExp *node2 = isSgFunctionRefExp(
        args->get_traversalSuccessorByIndex(1));

    // Get the LHS identifier if the call is embedded in an assignment
    EnvRef leftVar;
    SgAssignOp *assignOp = isSgAssignOp(node->get_parent());
    if(assignOp != NULL) {
        leftVar.setNode(assignOp->get_lhs_operand_i());
    }

    return new Call__data_apply(node, leftVar, node1, node2);
}

GridLibCall* constructCall__data_add(SgFunctionCallExp *node) {
    return new Call__data_add(node);
}

GridLibCall* constructCall__data_sub(SgFunctionCallExp *node) {
    return new Call__data_sub(node);
}

GridLibCall* constructCall__data_mult(SgFunctionCallExp *node) {
    return new Call__data_mult(node);
}

GridLibCall* constructCall__data_pow(SgFunctionCallExp *node) {
    return new Call__data_pow(node);
}

GridLibCall* constructCall__data_sum(SgFunctionCallExp *node) {
    return new Call__data_sum(node);
}


void CallVisitor::accept(const CallsVectorT &v) {
    for(CallsConstIteratorT i = v.begin(); i != v.end(); i++) {
        cout << "accept: ";
        (*i)->printSimp(cout);
        cout << endl;
        (*i)->accept(*this);
    }
}


void CallVisitor::visit__string_new(Call__string_new *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__string_init(Call__string_init *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__file_open(Call__file_open *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__subgrid_print(Call__subgrid_print *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__subgrid_printSimp(Call__subgrid_printSimp *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__subgridptrvec_print(Call__subgridptrvec_print *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_print(Call__grid_print *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_printSimp(Call__grid_printSimp *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__distribution_print(Call__distribution_print *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__distribution_printSimp(Call__distribution_printSimp *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__environment_input(Call__environment_input *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_newFromFile(Call__schedule_newFromFile *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_input(Call__schedule_input *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_print(Call__schedule_print *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_printSimp(Call__schedule_printSimp *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_new(Call__data_new *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_newMetaDataCopied(Call__data_newMetaDataCopied *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_print(Call__data_print *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_printSimp(Call__data_printSimp *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_printForProc(Call__data_printForProc *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_input(Call__data_input *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_forceUpdate(Call__data_forceUpdate *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply(Call__data_apply *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_add(Call__data_add *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_sub(Call__data_sub *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_mult(Call__data_mult *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_pow(Call__data_pow *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_sum(Call__data_sum *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

SgNode *Call__data_apply::funcDef() {
    SgFunctionDefinition *def = isSgFunctionDefinition(
        func()->getAssociatedFunctionDeclaration()->
            get_traversalSuccessorByIndex(2));
    assert(def != NULL);
    
    SgBasicBlock *block =
        isSgBasicBlock(def->get_traversalSuccessorByIndex(0));
    assert(block != NULL);

    for(int i = 0; i < block->get_numberOfTraversalSuccessors(); i++) {
        if(isSgVariableDeclaration(block->get_traversalSuccessorByIndex(i)) ==
           NULL)
        {
            return block->get_traversalSuccessorByIndex(i);
        }
    }
    
    return NULL;
}


/*string getEnvironmentalIDFromSgNode(SgNode *node) {
    bool error = false;

    if(isSgVarRefExp(node)) {
        return isSgVarRefExp(node)->get_symbol()->get_name().getString();
    } else if(isSgFunctionCallExp(node)) {
        SgVarRefExp *var =
            isSgVarRefExp(node->get_traversalSuccessorByIndex(1)->
                get_traversalSuccessorByIndex(0));
        if(var == NULL) { error = true; }
        else {
            return var->get_symbol()->get_name().getString();
        }
    }
    
    if(error) {
        cerr << "Unknown AST to refer to an environmental object:";
        printNodeAndChildren(cerr, node);
        cerr << endl;
    }
}*/

EnvRef::EnvRef(SgNode *node) {
    setNode(node);
}

void EnvRef::setNode(SgNode *node) {
    bool error = false;

    mNode = node;

    // If this is a direct reference
    if(isSgVarRefExp(node)) {
        mRefType = REFERENCE_TYPE_DIRECT;
        mObjType = ENVIRONMENT_UNTYPED;
        mIdent = isSgVarRefExp(node)->get_symbol()->get_name().getString();
    }
    // If this is a reference via a call to environment_
    else if(isSgFunctionCallExp(node)) {
        SgVarRefExp *var =
            isSgVarRefExp(node->get_traversalSuccessorByIndex(1)->
                get_traversalSuccessorByIndex(0));
        if(var == NULL) { error = true; }
        else {
            mRefType = REFERENCE_TYPE_VIA_ENV;
            mIdent = var->get_symbol()->get_name().getString();

            // Set type based on what environment_ function was called
            string funcSym = 
                isSgFunctionCallExp(node)->
                    getAssociatedFunctionSymbol()->get_name().getString();
            if(funcSym == "environment_getNeighbor") {
                mObjType = ENVIRONMENT_NEIGHBOR;
            } else if(funcSym == "environment_getSubGrid") {
                mObjType = ENVIRONMENT_SUB_GRID;
            } else if(funcSym == "environment_getGrid") {
                mObjType = ENVIRONMENT_GRID;
            } else if(funcSym == "environment_getDistribution") {
                mObjType = ENVIRONMENT_DISTRIBUTION;
            } else if(funcSym == "environment_getSchedule") {
                mObjType = ENVIRONMENT_SCHEDULE;
            } else {
                mObjType = ENVIRONMENT_UNTYPED;
                error = true;
            }
        }
    } else {
        error = true;
    }
    
    if(error) {
        cerr << "Unknown AST to refer to an environmental object:";
        printNodeAndChildren(cerr, node);
        cerr << endl;
    }
}

string  *EnvRef::isString() {
    if(mObjType != ENVIRONMENT_STRING) { return NULL; }
    assert(false);
}

string  *EnvRef::isFile() {
    if(mObjType != ENVIRONMENT_FILE) { return NULL; }

    // Return object from the environment.  If the source code accessed
    // it through the environment we'll have to resolve the string it used.
    if(mRefType == REFERENCE_TYPE_DIRECT) {
        assert(false);
    } else if(mRefType == REFERENCE_TYPE_VIA_ENV) {
        assert(false);
    } else { assert(false); }
}

Neighbor  *EnvRef::isNeighbor() {
    if(mObjType != ENVIRONMENT_NEIGHBOR) { return NULL; }

    // Return object from the environment.  If the source code accessed
    // it through the environment we'll have to resolve the string it used.
    if(mRefType == REFERENCE_TYPE_DIRECT) {
        assert(false);
    } else if(mRefType == REFERENCE_TYPE_VIA_ENV) {
        assert(false);
    } else { assert(false); }

}

SubGrid  *EnvRef::isSubGrid() {
    if(mObjType != ENVIRONMENT_SUB_GRID) { return NULL; }
    
    // Return object from the environment.  If the source code accessed
    // it through the environment we'll have to resolve the string it used.
    if(mRefType == REFERENCE_TYPE_DIRECT) {
        assert(false);
    } else if(mRefType == REFERENCE_TYPE_VIA_ENV) {
        assert(false);
    } else { assert(false); }
}

Grid  *EnvRef::isGrid() {
    if(mObjType != ENVIRONMENT_GRID) { return NULL; }
    
    // Return object from the environment.  If the source code accessed
    // it through the environment we'll have to resolve the string it used.
    if(mRefType == REFERENCE_TYPE_DIRECT) {
        return gEnv.getGrid(mIdent);
    } else if(mRefType == REFERENCE_TYPE_VIA_ENV) {
        return gEnv.getGrid(gEnv.getStringValue(mIdent));
    } else { assert(false); }
}

Distribution  *EnvRef::isDistribution() {
    if(mObjType != ENVIRONMENT_DISTRIBUTION) { return NULL; }
    
    // Return object from the environment.  If the source code accessed
    // it through the environment we'll have to resolve the string it used.
    if(mRefType == REFERENCE_TYPE_DIRECT) {
        return gEnv.getDistribution(mIdent);
    } else if(mRefType == REFERENCE_TYPE_VIA_ENV) {
        return gEnv.getDistribution(gEnv.getStringValue(mIdent));
    } else { assert(false); }
}

DataObject  *EnvRef::isDataObject() {
    if(mObjType != ENVIRONMENT_DATA_OBJECT) { return NULL; }
    
    // Return object from the environment.  If the source code accessed
    // it through the environment we'll have to resolve the string it used.
    if(mRefType == REFERENCE_TYPE_DIRECT) {
        assert(false);
    } else if(mRefType == REFERENCE_TYPE_VIA_ENV) {
        assert(false);
    } else { assert(false); }
}

Schedule *EnvRef::isSchedule() {
    if(mObjType != ENVIRONMENT_SCHEDULE) { return NULL; }
    
    // Return object from the environment.  If the source code accessed
    // it through the environment we'll have to resolve the string it used.
    if(mRefType == REFERENCE_TYPE_DIRECT) {
        return gEnv.getSchedule(mIdent);       
    } else if(mRefType == REFERENCE_TYPE_VIA_ENV) {
        return gEnv.getSchedule(gEnv.getStringValue(mIdent));
    } else { assert(false); }
}

