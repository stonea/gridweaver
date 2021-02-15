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
    "subgrid_new",
    "subgrid_width",
    "subgrid_height",
    "grid_new",
    "grid_addSubgrid",
    "grid_addBorder",
    "grid_placeAdjacentLR",
    "grid_placeAdjacentRL",
    "grid_placeAdjacentTB",
    "grid_placeAdjacentBT",
    "grid_connectTtoB",
    "grid_connectRtoL",
    "grid_connectBtoT",
    "grid_connectLtoR",
    "grid_connectLtoT",
    "grid_connectLtoB",
    "grid_connectRtoT",
    "grid_connectRtoB",
    "grid_connectTtoL",
    "grid_connectTtoR",
    "grid_connectBtoL",
    "grid_connectBtoR",
    "grid_wrapLR",
    "grid_wrapTB",
    "grid_placeAdjacentWithOffsetLR",
    "grid_placeAdjacentWithOffsetRL",
    "grid_placeAdjacentWithOffsetTB",
    "grid_placeAdjacentWithOffsetBT",
    "grid_mirrorT",
    "grid_mirrorB",
    "grid_mirrorL",
    "grid_mirrorR",
    "grid_foldT",
    "grid_foldB",
    "grid_foldL",
    "grid_foldR",
    "grid_numSubgrids",
    "grid_getSubgrid",
    "grid_orient0",
    "grid_orient45",
    "grid_orient90",
    "grid_orient135",
    "grid_orient180",
    "grid_orient225",
    "grid_orient270",
    "grid_orient315",
    "grid_orient360",
    "distribution_new",
    "distribution_applyFillBlock",
    "distribution_applyBlockFill",
    "distribution_applyBlockCyclic",
    "distribution_visualize",
    "environment_print",
    "environment_output",
    "environment_input",
    "environment_clear",
    "schedule_new",
    "schedule_calculate",
    "schedule_transferToFortran",
    "schedule_printFortranVersion",
    "schedule_sendRegionSize",
    "schedule_recvRegionSize",
    "data_new",
    "data_print",
    "data_printForProcs",
    "data_printForProc",
    "data_apply1",
    "data_apply2",
    "data_apply3",
    "data_apply4",
    "data_apply5",
    "data_apply6",
    "data_apply7",
    "data_apply8",
    "data_apply9",
    "data_apply10",
    "data_forceUpdate"
};

GridLibCall* (*CALL_CONSTRUCTOR[CALL__NUM_CALLS])(SgFunctionCallExp *node) =
{
    constructCall__subgrid_new,
    constructCall__subgrid_width,
    constructCall__subgrid_height,
    constructCall__grid_new,
    constructCall__grid_addSubgrid,
    constructCall__grid_addBorder,
    constructCall__grid_placeAdjacentLR,
    constructCall__grid_placeAdjacentRL,
    constructCall__grid_placeAdjacentTB,
    constructCall__grid_placeAdjacentBT,
    constructCall__grid_connectTtoB,
    constructCall__grid_connectRtoL,
    constructCall__grid_connectBtoT,
    constructCall__grid_connectLtoR,
    constructCall__grid_connectLtoT,
    constructCall__grid_connectLtoB,
    constructCall__grid_connectRtoT,
    constructCall__grid_connectRtoB,
    constructCall__grid_connectTtoL,
    constructCall__grid_connectTtoR,
    constructCall__grid_connectBtoL,
    constructCall__grid_connectBtoR,
    constructCall__grid_wrapLR,
    constructCall__grid_wrapTB,
    constructCall__grid_placeAdjacentWithOffsetLR,
    constructCall__grid_placeAdjacentWithOffsetRL,
    constructCall__grid_placeAdjacentWithOffsetTB,
    constructCall__grid_placeAdjacentWithOffsetBT,
    constructCall__grid_mirrorT,
    constructCall__grid_mirrorB,
    constructCall__grid_mirrorL,
    constructCall__grid_mirrorR,
    constructCall__grid_foldT,
    constructCall__grid_foldB,
    constructCall__grid_foldL,
    constructCall__grid_foldR,
    constructCall__grid_numSubgrids,
    constructCall__grid_getSubgrid,
    constructCall__grid_orient0,
    constructCall__grid_orient45,
    constructCall__grid_orient90,
    constructCall__grid_orient135,
    constructCall__grid_orient180,
    constructCall__grid_orient225,
    constructCall__grid_orient270,
    constructCall__grid_orient315,
    constructCall__grid_orient360,
    constructCall__distribution_new,
    constructCall__distribution_applyFillBlock,
    constructCall__distribution_applyBlockFill,
    constructCall__distribution_applyBlockCyclic,
    constructCall__distribution_visualize,
    constructCall__environment_print,
    constructCall__environment_output,
    constructCall__environment_input,
    constructCall__environment_clear,
    constructCall__schedule_new,
    constructCall__schedule_calculate,
    constructCall__schedule_transferToFortran,
    constructCall__schedule_printFortranVersion,
    constructCall__schedule_sendRegionSize,
    constructCall__schedule_recvRegionSize,
    constructCall__data_new,
    constructCall__data_print,
    constructCall__data_printForProcs,
    constructCall__data_printForProc,
    constructCall__data_apply1,
    constructCall__data_apply2,
    constructCall__data_apply3,
    constructCall__data_apply4,
    constructCall__data_apply5,
    constructCall__data_apply6,
    constructCall__data_apply7,
    constructCall__data_apply8,
    constructCall__data_apply9,
    constructCall__data_apply10,
    constructCall__data_forceUpdate
};

/**
 * Print the types of children a given node has.  Useful for debugging.
 */
void printSgChildren(string name, SgFunctionCallExp *node) {
    cout << "In " << name << endl;

    SgExprListExp *args = node->get_args();
    
    for(int i = 0; i < args->get_numberOfTraversalSuccessors(); i++) {
        SgNode *n = args->get_traversalSuccessorByIndex(i);
        cout << "\t" << n->class_name() << endl;
    }

    cout << "\tPARENT: " << node->get_parent()->class_name() << endl;
}

/**
 * If the function call is on the right-hand side of an assignment operation then return the left-hand
 * side fo the assignemnt
 */
SgVarRefExp* getLHS(SgFunctionCallExp *node) {
    // Ensure that this function call is the RHS of an assignment 
    SgAssignOp *parent = isSgAssignOp(node->get_parent());
    if(parent != NULL) {
        cout << "\tCHILDRENS OF ASSIGN" << endl;

        SgVarRefExp *lhs = isSgVarRefExp(parent->get_traversalSuccessorByIndex(0));

        return lhs;
    }

    return NULL;
}


GridLibCall* constructCall__subgrid_new(SgFunctionCallExp *node) {
    printSgChildren("constructCall__subgrid_new", node);

    // Extract the call's arguments: SgVarRefExp = (SgStringVal, SgIntVal, SgIntVal)
    SgExprListExp *args = node->get_args();
    string lhs     = getLHS(node)->unparseToString();
    string envName = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string width   = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string height  = args->get_traversalSuccessorByIndex(2)->unparseToString();

    return new Call__subgrid_new(node, lhs, envName, width, height);
}

GridLibCall* constructCall__subgrid_width(SgFunctionCallExp *node) {
    printSgChildren("constructCall__subgrid_width", node);

    return new Call__subgrid_width(node);
}

GridLibCall* constructCall__subgrid_height(SgFunctionCallExp *node) {
    printSgChildren("constructCall__subgrid_height", node);

    return new Call__subgrid_height(node);
}

GridLibCall* constructCall__grid_new(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_new", node);

    // Extract the call's arguments: SgVarRefExp = (SgStringVal)
    SgExprListExp *args = node->get_args();
    string lhs     = getLHS(node)->unparseToString();
    string envName = args->get_traversalSuccessorByIndex(0)->unparseToString();

    return new Call__grid_new(node, lhs, envName);
}

GridLibCall* constructCall__grid_addSubgrid(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_addSubgrid", node);

    // Extract the call's arguments: (SgVarRefExp, SgVarRefExp)
    SgExprListExp *args = node->get_args();
    string grid    = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string subgrid = args->get_traversalSuccessorByIndex(1)->unparseToString();

    return new Call__grid_addSubgrid(node, grid, subgrid);
}

GridLibCall* constructCall__grid_addBorder(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_addBorder", node);

    // Extract the call's arguments:
    // (SgVarRefExp, SgIntVal, SgIntVal, SgIntVal, SgIntVal, SgIntVal,
    //  SgVarRefExp, SgIntVal, SgIntVal, SgIntVal, SgVarRefExp SgIntVal)
    SgExprListExp *args = node->get_args();
    string grid     = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string srcX1    = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string srcY1    = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string srcX2    = args->get_traversalSuccessorByIndex(3)->unparseToString();
    string srcY2    = args->get_traversalSuccessorByIndex(4)->unparseToString();
    string srcSG    = args->get_traversalSuccessorByIndex(5)->unparseToString();
    string tgtX1    = args->get_traversalSuccessorByIndex(6)->unparseToString();
    string tgtY1    = args->get_traversalSuccessorByIndex(7)->unparseToString();
    string tgtX2    = args->get_traversalSuccessorByIndex(8)->unparseToString();
    string tgtY2    = args->get_traversalSuccessorByIndex(9)->unparseToString();
    string tgtSG    = args->get_traversalSuccessorByIndex(10)->unparseToString();
    string rotation = args->get_traversalSuccessorByIndex(11)->unparseToString();
    
    return new Call__grid_addBorder(node, grid,
        srcX1, srcY1, srcX2, srcY2, srcSG,
        tgtX1, tgtY1, tgtX2, tgtY2, tgtSG, rotation);
}

GridLibCall* constructCall__grid_placeAdjacentLR(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_placeAdjacentLR", node);

    return new Call__grid_placeAdjacentLR(node);
}

GridLibCall* constructCall__grid_placeAdjacentRL(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_placeAdjacentRL", node);

    return new Call__grid_placeAdjacentRL(node);
}

GridLibCall* constructCall__grid_placeAdjacentTB(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_placeAdjacentTB", node);

    return new Call__grid_placeAdjacentTB(node);
}

GridLibCall* constructCall__grid_placeAdjacentBT(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_placeAdjacentBT", node);

    return new Call__grid_placeAdjacentBT(node);
}

GridLibCall* constructCall__grid_connectTtoB(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectTtoB", node);

    return new Call__grid_connectTtoB(node);
}

GridLibCall* constructCall__grid_connectRtoL(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectRtoL", node);

    return new Call__grid_connectRtoL(node);
}

GridLibCall* constructCall__grid_connectBtoT(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectBtoT", node);

    return new Call__grid_connectBtoT(node);
}

GridLibCall* constructCall__grid_connectLtoR(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectLtoR", node);

    return new Call__grid_connectLtoR(node);
}

GridLibCall* constructCall__grid_connectLtoT(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectLtoT", node);

    return new Call__grid_connectLtoT(node);
}

GridLibCall* constructCall__grid_connectLtoB(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectLtoB", node);

    return new Call__grid_connectLtoB(node);
}

GridLibCall* constructCall__grid_connectRtoT(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectRtoT", node);

    return new Call__grid_connectRtoT(node);
}

GridLibCall* constructCall__grid_connectRtoB(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectRtoB", node);

    return new Call__grid_connectRtoB(node);
}

GridLibCall* constructCall__grid_connectTtoL(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectTtoL", node);

    return new Call__grid_connectTtoL(node);
}

GridLibCall* constructCall__grid_connectTtoR(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectTtoR", node);

    return new Call__grid_connectTtoR(node);
}

GridLibCall* constructCall__grid_connectBtoL(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectBtoL", node);

    return new Call__grid_connectBtoL(node);
}

GridLibCall* constructCall__grid_connectBtoR(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_connectBtoR", node);

    return new Call__grid_connectBtoR(node);
}

GridLibCall* constructCall__grid_wrapLR(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_wrapLR", node);

    return new Call__grid_wrapLR(node);
}

GridLibCall* constructCall__grid_wrapTB(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_wrapTB", node);

    return new Call__grid_wrapTB(node);
}

GridLibCall* constructCall__grid_placeAdjacentWithOffsetLR(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_placeAdjacentWithOffsetLR", node);

    return new Call__grid_placeAdjacentWithOffsetLR(node);
}

GridLibCall* constructCall__grid_placeAdjacentWithOffsetRL(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_placeAdjacentWithOffsetRL", node);

    return new Call__grid_placeAdjacentWithOffsetRL(node);
}

GridLibCall* constructCall__grid_placeAdjacentWithOffsetTB(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_placeAdjacentWithOffsetTB", node);

    return new Call__grid_placeAdjacentWithOffsetTB(node);
}

GridLibCall* constructCall__grid_placeAdjacentWithOffsetBT(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_placeAdjacentWithOffsetBT", node);

    return new Call__grid_placeAdjacentWithOffsetBT(node);
}

GridLibCall* constructCall__grid_mirrorT(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_mirrorT", node);

    return new Call__grid_mirrorT(node);
}

GridLibCall* constructCall__grid_mirrorB(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_mirrorB", node);

    return new Call__grid_mirrorB(node);
}

GridLibCall* constructCall__grid_mirrorL(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_mirrorL", node);

    return new Call__grid_mirrorL(node);
}

GridLibCall* constructCall__grid_mirrorR(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_mirrorR", node);

    return new Call__grid_mirrorR(node);
}

GridLibCall* constructCall__grid_foldT(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_foldT", node);

    return new Call__grid_foldT(node);
}

GridLibCall* constructCall__grid_foldB(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_foldB", node);

    return new Call__grid_foldB(node);
}

GridLibCall* constructCall__grid_foldL(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_foldL", node);

    return new Call__grid_foldL(node);
}

GridLibCall* constructCall__grid_foldR(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_foldR", node);

    return new Call__grid_foldR(node);
}

GridLibCall* constructCall__grid_numSubgrids(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_numSubgrids", node);

    return new Call__grid_numSubgrids(node);
}

GridLibCall* constructCall__grid_getSubgrid(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_getSubgrid", node);

    return new Call__grid_getSubgrid(node);
}

GridLibCall* constructCall__grid_orient0(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_orient0", node);

    return new Call__grid_orient0(node);
}

GridLibCall* constructCall__grid_orient45(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_orient45", node);

    return new Call__grid_orient45(node);
}

GridLibCall* constructCall__grid_orient90(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_orient90", node);

    return new Call__grid_orient90(node);
}

GridLibCall* constructCall__grid_orient135(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_orient135", node);

    return new Call__grid_orient135(node);
}

GridLibCall* constructCall__grid_orient180(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_orient180", node);

    return new Call__grid_orient180(node);
}

GridLibCall* constructCall__grid_orient225(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_orient225", node);

    return new Call__grid_orient225(node);
}

GridLibCall* constructCall__grid_orient270(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_orient270", node);

    return new Call__grid_orient270(node);
}

GridLibCall* constructCall__grid_orient315(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_orient315", node);

    return new Call__grid_orient315(node);
}

GridLibCall* constructCall__grid_orient360(SgFunctionCallExp *node) {
    printSgChildren("constructCall__grid_orient360", node);

    return new Call__grid_orient360(node);
}

GridLibCall* constructCall__distribution_new(SgFunctionCallExp *node) {
    printSgChildren("constructCall__distribution_new", node);

    // Extract the call's arguments: SgVarRefExp == (SgStringVal)
    SgExprListExp *args = node->get_args();
    string lhs     = getLHS(node)->unparseToString();
    string envName = args->get_traversalSuccessorByIndex(0)->unparseToString();

    return new Call__distribution_new(node, lhs, envName);
}


GridLibCall* constructCall__distribution_applyFillBlock(SgFunctionCallExp *node) {
    printSgChildren("constructCall__distribution_applyFillBlock", node);
    
    // Extract the call's arguments:
    //   (SgVarRefExp, SgVarRefExp, SgIntVal, SgIntVal)
    SgExprListExp *args = node->get_args();
    string dist   = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string grid   = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string blockW = args->get_traversalSuccessorByIndex(2)->unparseToString();
    
    return new
        Call__distribution_applyFillBlock(node, dist, grid, blockW);
}

GridLibCall* constructCall__distribution_applyBlockFill(SgFunctionCallExp *node) {
    printSgChildren("constructCall__distribution_applyBlockFill", node);
    
    // Extract the call's arguments:
    //   (SgVarRefExp, SgVarRefExp, SgIntVal, SgIntVal)
    SgExprListExp *args = node->get_args();
    string dist   = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string grid   = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string blockH = args->get_traversalSuccessorByIndex(2)->unparseToString();

    return new
        Call__distribution_applyBlockFill(node, dist, grid, blockH);
}

GridLibCall* constructCall__distribution_applyBlockCyclic(SgFunctionCallExp *node) {
    printSgChildren("constructCall__distribution_applyBlockCyclic", node);

    // Extract the call's arguments:
    //   (SgVarRefExp, SgVarRefExp, SgIntVal, SgIntVal, SgIntVal)
    SgExprListExp *args = node->get_args();
    string dist   = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string grid   = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string blkW   = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string blkH   = args->get_traversalSuccessorByIndex(3)->unparseToString();

    return new
        Call__distribution_applyBlockCyclic(node, dist, grid, blkW, blkH);
}

GridLibCall* constructCall__distribution_visualize(SgFunctionCallExp *node) {
    printSgChildren("constructCall__distribution_visualize", node);

    return new Call__distribution_visualize(node);
}

GridLibCall* constructCall__environment_print(SgFunctionCallExp *node) {
    printSgChildren("constructCall__environment_print", node);

    return new Call__environment_print(node);
}

GridLibCall* constructCall__environment_output(SgFunctionCallExp *node) {
    printSgChildren("constructCall__environment_output", node);

    return new Call__environment_output(node);
}

GridLibCall* constructCall__environment_input(SgFunctionCallExp *node) {
    printSgChildren("constructCall__environment_input", node);

    return new Call__environment_input(node);
}

GridLibCall* constructCall__environment_clear(SgFunctionCallExp *node) {
    printSgChildren("constructCall__environment_clear", node);

    return new Call__environment_clear(node);
}

GridLibCall* constructCall__schedule_new(SgFunctionCallExp *node) {
    printSgChildren("constructCall__schedule_new", node);

    // Extract the call's arguments: SgVarRefExp = (SgVarRefExp)
    SgExprListExp *args = node->get_args();
    string lhs     = getLHS(node)->unparseToString();
    string envName = args->get_traversalSuccessorByIndex(0)->unparseToString();

    return new Call__schedule_new(node, lhs, envName);
}

GridLibCall* constructCall__schedule_calculate(SgFunctionCallExp *node) {
    printSgChildren("constructCall__schedule_calculate", node);

    // Extract the call's arguments: (SgVarRefExp, SgVarRefExp SgVarRefExp)
    SgExprListExp *args = node->get_args();
    string sched = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string grid  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dist  = args->get_traversalSuccessorByIndex(2)->unparseToString();

    return new Call__schedule_calculate(node, sched, grid, dist);
}

GridLibCall* constructCall__schedule_transferToFortran(SgFunctionCallExp *node) {
    printSgChildren("constructCall__schedule_transferToFortran", node);

    // Extract the call's arguments: (SgVarRefExp)
    SgExprListExp *args = node->get_args();
    string sched = args->get_traversalSuccessorByIndex(0)->unparseToString();

    return new Call__schedule_transferToFortran(node, sched);
}

GridLibCall* constructCall__schedule_printFortranVersion(SgFunctionCallExp *node) {
    printSgChildren("constructCall__schedule_printFortranVersion", node);

    return new Call__schedule_printFortranVersion(node);
}

GridLibCall* constructCall__schedule_sendRegionSize(SgFunctionCallExp *node) {
    printSgChildren("constructCall__schedule_sendRegionSize", node);

    return new Call__schedule_sendRegionSize(node);
}

GridLibCall* constructCall__schedule_recvRegionSize(SgFunctionCallExp *node) {
    printSgChildren("constructCall__schedule_recvRegionSize", node);

    return new Call__schedule_recvRegionSize(node);
}

GridLibCall* constructCall__data_new(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_new", node);

    // Extract the call's arguments: (SgVarRefExp) = (SgVarRefExp)
    SgExprListExp *args = node->get_args();
    string lhs  = getLHS(node)->unparseToString();
    string data = args->get_traversalSuccessorByIndex(0)->unparseToString();

    return new Call__data_new(node, lhs, data);
}

GridLibCall* constructCall__data_print(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_print", node);

    // Extract the call's arguments: (SgVarRefExp, SgIntVal)
    SgExprListExp *args = node->get_args();
    string dataObj = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string fileID  = args->get_traversalSuccessorByIndex(1)->unparseToString();

    return new Call__data_print(node, dataObj, fileID);
}

GridLibCall* constructCall__data_printForProcs(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_printForProcs", node);
    
    // Extract the call's arguments: (SgVarRefExp, SgVarRefExp, SgFunctionRefExp)
    SgExprListExp *args = node->get_args();
    string dataObj = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string fileID  = args->get_traversalSuccessorByIndex(1)->unparseToString();

    return new Call__data_printForProcs(node, dataObj, fileID);
}

GridLibCall* constructCall__data_printForProc(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_printForProc", node);

    return new Call__data_printForProc(node);
}

GridLibCall* constructCall__data_apply1(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply1", node);

    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1 = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string func    = args->get_traversalSuccessorByIndex(2)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(2));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);
   
    return new Call__data_apply1(node, dataOut, dataIn1, func, def);
}

GridLibCall* constructCall__data_apply2(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply2", node);

    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut  = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dataIn2  = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string func     = args->get_traversalSuccessorByIndex(3)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(3));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);
   
    return new Call__data_apply2(node, dataOut,
        dataIn1, dataIn2, func, def); 
}

GridLibCall* constructCall__data_apply3(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply3", node);

    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut  = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dataIn2  = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string dataIn3  = args->get_traversalSuccessorByIndex(3)->unparseToString();
    string func     = args->get_traversalSuccessorByIndex(4)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(4));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);

    return new Call__data_apply3(node, dataOut,
        dataIn1, dataIn2, dataIn3, func, def); 
}

GridLibCall* constructCall__data_apply4(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply4", node);

    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut  = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dataIn2  = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string dataIn3  = args->get_traversalSuccessorByIndex(3)->unparseToString();
    string dataIn4  = args->get_traversalSuccessorByIndex(4)->unparseToString();
    string func     = args->get_traversalSuccessorByIndex(5)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(5));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);

    return new Call__data_apply4(node, dataOut,
        dataIn1, dataIn2, dataIn3, dataIn4, func, def); 
}

GridLibCall* constructCall__data_apply5(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply5", node);

    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut  = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dataIn2  = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string dataIn3  = args->get_traversalSuccessorByIndex(3)->unparseToString();
    string dataIn4  = args->get_traversalSuccessorByIndex(4)->unparseToString();
    string dataIn5  = args->get_traversalSuccessorByIndex(5)->unparseToString();
    string func     = args->get_traversalSuccessorByIndex(6)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(6));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);

    return new Call__data_apply5(node, dataOut,
        dataIn1, dataIn2, dataIn3, dataIn4, dataIn5, func, def); 
}

GridLibCall* constructCall__data_apply6(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply6", node);

    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut  = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dataIn2  = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string dataIn3  = args->get_traversalSuccessorByIndex(3)->unparseToString();
    string dataIn4  = args->get_traversalSuccessorByIndex(4)->unparseToString();
    string dataIn5  = args->get_traversalSuccessorByIndex(5)->unparseToString();
    string dataIn6  = args->get_traversalSuccessorByIndex(6)->unparseToString();
    string func     = args->get_traversalSuccessorByIndex(7)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(7));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);

    return new Call__data_apply6(node, dataOut,
        dataIn1, dataIn2, dataIn3, dataIn4, dataIn5, dataIn6, func, def); 
}

GridLibCall* constructCall__data_apply7(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply7", node);

    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut  = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dataIn2  = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string dataIn3  = args->get_traversalSuccessorByIndex(3)->unparseToString();
    string dataIn4  = args->get_traversalSuccessorByIndex(4)->unparseToString();
    string dataIn5  = args->get_traversalSuccessorByIndex(5)->unparseToString();
    string dataIn6  = args->get_traversalSuccessorByIndex(6)->unparseToString();
    string dataIn7  = args->get_traversalSuccessorByIndex(7)->unparseToString();
    string func     = args->get_traversalSuccessorByIndex(8)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(8));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);

    return new Call__data_apply7(node, dataOut,
        dataIn1, dataIn2, dataIn3, dataIn4, dataIn5,
        dataIn6, dataIn7, func, def); 
}

GridLibCall* constructCall__data_apply8(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply8", node);
    
    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut  = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dataIn2  = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string dataIn3  = args->get_traversalSuccessorByIndex(3)->unparseToString();
    string dataIn4  = args->get_traversalSuccessorByIndex(4)->unparseToString();
    string dataIn5  = args->get_traversalSuccessorByIndex(5)->unparseToString();
    string dataIn6  = args->get_traversalSuccessorByIndex(6)->unparseToString();
    string dataIn7  = args->get_traversalSuccessorByIndex(7)->unparseToString();
    string dataIn8  = args->get_traversalSuccessorByIndex(8)->unparseToString();
    string func     = args->get_traversalSuccessorByIndex(9)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(9));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);

    return new Call__data_apply8(node, dataOut,
        dataIn1, dataIn2, dataIn3, dataIn4, dataIn5,
        dataIn6, dataIn7, dataIn8, func, def); 
}

GridLibCall* constructCall__data_apply9(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply9", node);
    
    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut  = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dataIn2  = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string dataIn3  = args->get_traversalSuccessorByIndex(3)->unparseToString();
    string dataIn4  = args->get_traversalSuccessorByIndex(4)->unparseToString();
    string dataIn5  = args->get_traversalSuccessorByIndex(5)->unparseToString();
    string dataIn6  = args->get_traversalSuccessorByIndex(6)->unparseToString();
    string dataIn7  = args->get_traversalSuccessorByIndex(7)->unparseToString();
    string dataIn8  = args->get_traversalSuccessorByIndex(8)->unparseToString();
    string dataIn9  = args->get_traversalSuccessorByIndex(9)->unparseToString();
    string func     = args->get_traversalSuccessorByIndex(10)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(10));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);

    return new Call__data_apply9(node, dataOut,
        dataIn1, dataIn2, dataIn3, dataIn4, dataIn5,
        dataIn6, dataIn7, dataIn8, dataIn9, func, def); 
}

GridLibCall* constructCall__data_apply10(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_apply10", node);

    // Extract the calls' arguments SgVarRefExp (SgVarRefExp)* SgFunctionRefExp
    SgExprListExp *args = node->get_args();
    string dataOut  = args->get_traversalSuccessorByIndex(0)->unparseToString();
    string dataIn1  = args->get_traversalSuccessorByIndex(1)->unparseToString();
    string dataIn2  = args->get_traversalSuccessorByIndex(2)->unparseToString();
    string dataIn3  = args->get_traversalSuccessorByIndex(3)->unparseToString();
    string dataIn4  = args->get_traversalSuccessorByIndex(4)->unparseToString();
    string dataIn5  = args->get_traversalSuccessorByIndex(5)->unparseToString();
    string dataIn6  = args->get_traversalSuccessorByIndex(6)->unparseToString();
    string dataIn7  = args->get_traversalSuccessorByIndex(7)->unparseToString();
    string dataIn8  = args->get_traversalSuccessorByIndex(8)->unparseToString();
    string dataIn9  = args->get_traversalSuccessorByIndex(9)->unparseToString();
    string dataIn10 = args->get_traversalSuccessorByIndex(10)->unparseToString();
    string func     = args->get_traversalSuccessorByIndex(11)->unparseToString();

    // Get the node that includes the body for the stencil function
    SgFunctionRefExp *f = isSgFunctionRefExp(args->get_traversalSuccessorByIndex(11));
    assert(f != NULL);
    SgFunctionDeclaration *decl = f->getAssociatedFunctionDeclaration();
    assert(decl != NULL);
    SgFunctionDefinition *def = decl->get_definition();
    assert(def != NULL);

    return new Call__data_apply10(node, dataOut,
        dataIn1, dataIn2, dataIn3, dataIn4, dataIn5,
        dataIn6, dataIn7, dataIn8, dataIn9, dataIn10, func, def); 
}

GridLibCall* constructCall__data_forceUpdate(SgFunctionCallExp *node) {
    printSgChildren("constructCall__data_forceUpdate", node);

    return new Call__data_forceUpdate(node);
}



void CallVisitor::accept(const CallsVectorT &v) {
    for(CallsConstIteratorT i = v.begin(); i != v.end(); i++) {
        (*i)->accept(*this);
    }
}

void CallVisitor::visit__subgrid_new(Call__subgrid_new *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__subgrid_width(Call__subgrid_width *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__subgrid_height(Call__subgrid_height *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_new(Call__grid_new *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_addSubgrid(Call__grid_addSubgrid *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_addBorder(Call__grid_addBorder *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_placeAdjacentLR(Call__grid_placeAdjacentLR *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_placeAdjacentRL(Call__grid_placeAdjacentRL *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_placeAdjacentTB(Call__grid_placeAdjacentTB *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_placeAdjacentBT(Call__grid_placeAdjacentBT *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectTtoB(Call__grid_connectTtoB *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectRtoL(Call__grid_connectRtoL *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectBtoT(Call__grid_connectBtoT *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectLtoR(Call__grid_connectLtoR *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectLtoT(Call__grid_connectLtoT *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectLtoB(Call__grid_connectLtoB *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectRtoT(Call__grid_connectRtoT *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectRtoB(Call__grid_connectRtoB *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectTtoL(Call__grid_connectTtoL *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectTtoR(Call__grid_connectTtoR *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectBtoL(Call__grid_connectBtoL *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_connectBtoR(Call__grid_connectBtoR *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_wrapLR(Call__grid_wrapLR *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_wrapTB(Call__grid_wrapTB *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_placeAdjacentWithOffsetLR(Call__grid_placeAdjacentWithOffsetLR *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_placeAdjacentWithOffsetRL(Call__grid_placeAdjacentWithOffsetRL *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_placeAdjacentWithOffsetTB(Call__grid_placeAdjacentWithOffsetTB *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_placeAdjacentWithOffsetBT(Call__grid_placeAdjacentWithOffsetBT *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_mirrorT(Call__grid_mirrorT *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_mirrorB(Call__grid_mirrorB *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_mirrorL(Call__grid_mirrorL *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_mirrorR(Call__grid_mirrorR *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_foldT(Call__grid_foldT *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_foldB(Call__grid_foldB *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_foldL(Call__grid_foldL *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_foldR(Call__grid_foldR *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_numSubgrids(Call__grid_numSubgrids *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_getSubgrid(Call__grid_getSubgrid *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_orient0(Call__grid_orient0 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_orient45(Call__grid_orient45 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_orient90(Call__grid_orient90 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_orient135(Call__grid_orient135 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_orient180(Call__grid_orient180 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_orient225(Call__grid_orient225 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_orient270(Call__grid_orient270 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_orient315(Call__grid_orient315 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__grid_orient360(Call__grid_orient360 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__distribution_new(Call__distribution_new *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__distribution_applyFillBlock(Call__distribution_applyFillBlock *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__distribution_applyBlockFill(Call__distribution_applyBlockFill *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__distribution_applyBlockCyclic(Call__distribution_applyBlockCyclic *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__distribution_visualize(Call__distribution_visualize *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__environment_print(Call__environment_print *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__environment_output(Call__environment_output *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__environment_input(Call__environment_input *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__environment_clear(Call__environment_clear *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_new(Call__schedule_new *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_calculate(Call__schedule_calculate *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_transferToFortran(Call__schedule_transferToFortran *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_printFortranVersion(Call__schedule_printFortranVersion *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_sendRegionSize(Call__schedule_sendRegionSize *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__schedule_recvRegionSize(Call__schedule_recvRegionSize *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_new(Call__data_new *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_print(Call__data_print *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_printForProcs(Call__data_printForProcs *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_printForProc(Call__data_printForProc *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply1(Call__data_apply1 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply2(Call__data_apply2 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply3(Call__data_apply3 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply4(Call__data_apply4 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply5(Call__data_apply5 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply6(Call__data_apply6 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply7(Call__data_apply7 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply8(Call__data_apply8 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply9(Call__data_apply9 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_apply10(Call__data_apply10 *call) {
    visit__default(static_cast<GridLibCall*>(call));
}

void CallVisitor::visit__data_forceUpdate(Call__data_forceUpdate *call) {
    visit__default(static_cast<GridLibCall*>(call));
}


void Call__subgrid_new::print(std::ostream &out) const {
    printObj_start(out, "Call__subgrid_new", "");
    printObj_property(out, "lhs", mLHS);
    printObj_property(out, "width", mWidth);
    printObj_property(out, "height", mHeight);
    printObj_end(out);
}

void Call__subgrid_width::print(std::ostream &out) const {
    printObj_start(out, "Call__subgrid_width", "");
    printObj_end(out);
}

void Call__subgrid_height::print(std::ostream &out) const {
    printObj_start(out, "Call__subgrid_height", "");
    printObj_end(out);
}

void Call__grid_new::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_new", "");
    printObj_property(out, "lhs", mLHS);
    printObj_property(out, "envName", mEnvName);
    printObj_end(out);
}

void Call__grid_addSubgrid::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_addSubgrid", "");
    printObj_property(out, "grid", mGrid);
    printObj_property(out, "subgrid", mSubgrid);
    printObj_end(out);
}

void Call__grid_addBorder::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_addBorder", "");
    printObj_property(out, "grid", mGrid);
    printObj_property(out, "srcX1", mSrcX1);
    printObj_property(out, "srcY1", mSrcY1);
    printObj_property(out, "srcX2", mSrcX2);
    printObj_property(out, "srcY2", mSrcY2);
    printObj_property(out, "srcSG", mSrcSG);
    printObj_property(out, "tgtX1", mTgtX1);
    printObj_property(out, "tgtY1", mTgtY1);
    printObj_property(out, "tgtX2", mTgtX2);
    printObj_property(out, "tgtY2", mTgtY2);
    printObj_property(out, "tgtSG", mTgtSG);
    printObj_property(out, "rotation", mRotation);
    printObj_end(out);
}

void Call__grid_placeAdjacentLR::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_placeAdjacentLR", "");
    printObj_end(out);
}

void Call__grid_placeAdjacentRL::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_placeAdjacentRL", "");
    printObj_end(out);
}

void Call__grid_placeAdjacentTB::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_placeAdjacentTB", "");
    printObj_end(out);
}

void Call__grid_placeAdjacentBT::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_placeAdjacentBT", "");
    printObj_end(out);
}

void Call__grid_connectTtoB::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectTtoB", "");
    printObj_end(out);
}

void Call__grid_connectRtoL::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectRtoL", "");
    printObj_end(out);
}

void Call__grid_connectBtoT::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectBtoT", "");
    printObj_end(out);
}

void Call__grid_connectLtoR::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectLtoR", "");
    printObj_end(out);
}

void Call__grid_connectLtoT::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectLtoT", "");
    printObj_end(out);
}

void Call__grid_connectLtoB::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectLtoB", "");
    printObj_end(out);
}

void Call__grid_connectRtoT::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectRtoT", "");
    printObj_end(out);
}

void Call__grid_connectRtoB::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectRtoB", "");
    printObj_end(out);
}

void Call__grid_connectTtoL::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectTtoL", "");
    printObj_end(out);
}

void Call__grid_connectTtoR::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectTtoR", "");
    printObj_end(out);
}

void Call__grid_connectBtoL::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectBtoL", "");
    printObj_end(out);
}

void Call__grid_connectBtoR::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_connectBtoR", "");
    printObj_end(out);
}

void Call__grid_wrapLR::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_wrapLR", "");
    printObj_end(out);
}

void Call__grid_wrapTB::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_wrapTB", "");
    printObj_end(out);
}

void Call__grid_placeAdjacentWithOffsetLR::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_placeAdjacentWithOffsetLR", "");
    printObj_end(out);
}

void Call__grid_placeAdjacentWithOffsetRL::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_placeAdjacentWithOffsetRL", "");
    printObj_end(out);
}

void Call__grid_placeAdjacentWithOffsetTB::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_placeAdjacentWithOffsetTB", "");
    printObj_end(out);
}

void Call__grid_placeAdjacentWithOffsetBT::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_placeAdjacentWithOffsetBT", "");
    printObj_end(out);
}

void Call__grid_mirrorT::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_mirrorT", "");
    printObj_end(out);
}

void Call__grid_mirrorB::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_mirrorB", "");
    printObj_end(out);
}

void Call__grid_mirrorL::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_mirrorL", "");
    printObj_end(out);
}

void Call__grid_mirrorR::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_mirrorR", "");
    printObj_end(out);
}

void Call__grid_foldT::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_foldT", "");
    printObj_end(out);
}

void Call__grid_foldB::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_foldB", "");
    printObj_end(out);
}

void Call__grid_foldL::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_foldL", "");
    printObj_end(out);
}

void Call__grid_foldR::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_foldR", "");
    printObj_end(out);
}

void Call__grid_numSubgrids::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_numSubgrids", "");
    printObj_end(out);
}

void Call__grid_getSubgrid::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_getSubgrid", "");
    printObj_end(out);
}

void Call__grid_orient0::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_orient0", "");
    printObj_end(out);
}

void Call__grid_orient45::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_orient45", "");
    printObj_end(out);
}

void Call__grid_orient90::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_orient90", "");
    printObj_end(out);
}

void Call__grid_orient135::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_orient135", "");
    printObj_end(out);
}

void Call__grid_orient180::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_orient180", "");
    printObj_end(out);
}

void Call__grid_orient225::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_orient225", "");
    printObj_end(out);
}

void Call__grid_orient270::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_orient270", "");
    printObj_end(out);
}

void Call__grid_orient315::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_orient315", "");
    printObj_end(out);
}

void Call__grid_orient360::print(std::ostream &out) const {
    printObj_start(out, "Call__grid_orient360", "");
    printObj_end(out);
}

void Call__distribution_new::print(std::ostream &out) const {
    std::string mLHS, mEnvName;

    printObj_start(out, "Call__distribution_new", "");
    printObj_end(out);
}

void Call__distribution_applyFillBlock::print(std::ostream &out) const {
    printObj_start(out, "Call__distribution_applyFillBlock", "");
    printObj_property(out, "dist", mDist);
    printObj_property(out, "grid", mGrid);
    printObj_property(out, "blockW", mBlockW);
    printObj_end(out);
}

void Call__distribution_applyBlockFill::print(std::ostream &out) const {
    printObj_start(out, "Call__distribution_applyBlockFill", "");
    printObj_property(out, "dist", mDist);
    printObj_property(out, "grid", mGrid);
    printObj_property(out, "blockH", mBlockH);
    printObj_end(out);
}

void Call__distribution_applyBlockCyclic::print(std::ostream &out) const {
    printObj_start(out, "Call__distribution_applyBlockCyclic", "");
    printObj_property(out, "dist", mDist);
    printObj_property(out, "grid", mGrid);
    printObj_property(out, "blkW", mBlkW);
    printObj_property(out, "blkH", mBlkH);
    printObj_end(out);
}

void Call__distribution_visualize::print(std::ostream &out) const {
    printObj_start(out, "Call__distribution_visualize", "");
    printObj_end(out);
}

void Call__environment_print::print(std::ostream &out) const {
    printObj_start(out, "Call__environment_print", "");
    printObj_end(out);
}

void Call__environment_output::print(std::ostream &out) const {
    printObj_start(out, "Call__environment_output", "");
    printObj_end(out);
}

void Call__environment_input::print(std::ostream &out) const {
    printObj_start(out, "Call__environment_input", "");
    printObj_end(out);
}

void Call__environment_clear::print(std::ostream &out) const {
    printObj_start(out, "Call__environment_clear", "");
    printObj_end(out);
}

void Call__schedule_new::print(std::ostream &out) const {
    printObj_start(out, "Call__schedule_new", "");
    printObj_property(out, "lhs", mLHS);
    printObj_property(out, "envName", mEnvName);
    printObj_end(out);
}

void Call__schedule_calculate::print(std::ostream &out) const {
    printObj_start(out, "Call__schedule_calculate", "");
    printObj_property(out, "sched", mSched);
    printObj_property(out, "grid", mGrid);
    printObj_property(out, "dist", mDist);
    printObj_end(out);
}

void Call__schedule_transferToFortran::print(std::ostream &out) const {
    printObj_start(out, "Call__schedule_transferToFortran", "");
    printObj_property(out, "sched", mSched);
    printObj_end(out);
}

void Call__schedule_printFortranVersion::print(std::ostream &out) const {
    printObj_start(out, "Call__schedule_printFortranVersion", "");
    printObj_end(out);
}

void Call__schedule_sendRegionSize::print(std::ostream &out) const {
    printObj_start(out, "Call__schedule_sendRegionSize", "");
    printObj_end(out);
}

void Call__schedule_recvRegionSize::print(std::ostream &out) const {
    printObj_start(out, "Call__schedule_recvRegionSize", "");
    printObj_end(out);
}

void Call__data_new::print(std::ostream &out) const {
    printObj_start(out, "Call__data_new", "");
    printObj_property(out, "lhs", mLHS);
    printObj_property(out, "envName", mEnvName);
    printObj_end(out);
}

void Call__data_print::print(std::ostream &out) const {
    printObj_start(out, "Call__data_print", "");
    printObj_property(out, "data", mData);
    printObj_property(out, "fileID", mFileID);
    printObj_end(out);
}

void Call__data_printForProcs::print(std::ostream &out) const {
    printObj_start(out, "Call__data_printForProcs", "");
    printObj_property(out, "dataObj", mDataObj);
    printObj_property(out, "fileID", mFileID);
    printObj_end(out);
}

void Call__data_printForProc::print(std::ostream &out) const {
    printObj_start(out, "Call__data_printForProc", "");
    printObj_end(out);
}

void Call__data_apply1::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply1", "");
    printObj_end(out);
}

void Call__data_apply2::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply2", "");
    printObj_end(out);
}

void Call__data_apply3::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply3", "");
    printObj_end(out);
}

void Call__data_apply4::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply4", "");
    printObj_end(out);
}

void Call__data_apply5::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply5", "");
    printObj_end(out);
}

void Call__data_apply6::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply6", "");
    printObj_end(out);
}

void Call__data_apply7::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply7", "");
    printObj_end(out);
}

void Call__data_apply8::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply8", "");
    printObj_end(out);
}

void Call__data_apply9::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply9", "");
    printObj_end(out);
}

void Call__data_apply10::print(std::ostream &out) const {
    printObj_start(out, "Call__data_apply10", "");
    printObj_end(out);
}

void Call__data_forceUpdate::print(std::ostream &out) const {
    printObj_start(out, "Call__data_forceUpdate", "");
    printObj_end(out);
}

