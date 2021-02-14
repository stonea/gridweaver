/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#ifndef RUNTIME_WRAPPER_HPP_
#define RUNTIME_WRAPPER_HPP_

#include "stdio.h"
#include "stdlib.h"

/**
 * @file
 * This file includes functions that can be called from the GridWeaver Fortran
 * runtime library.
 */


/**
 * Initialization function for the runtimewrapper module.
 */
void initializeModule_runtimewrapper();


extern "C" {
    // Functions the user might use:
    
    void __gridweaver_initialize();

    void __turnOffSyntaxHighlighting();

    void __neighbor_new(char *neigh, int &x, int &y);
    
    void __subgrid_new(char *sg, int &width, int &height);
    void __subgrid_width(char *sg, int &ret);
    void __subgrid_height(char *sg, int &ret);
    
    void __grid_new(char *g);
    void __grid_addSubgrid(char *g, char *sg);
    void __grid_addBorder(
        char *g,
        int &srcX1, int &srcY1, int &srcX2, int &srcY2, char* srcSG,
        int &tgtX1, int &tgtY1, int &tgtX2, int &tgtY2, char* tgtSG,
        int &rotation);
    void __grid_placeAdjacentLR(char *g, char *sgL, char *sgR);
    void __grid_placeAdjacentRL(char *g, char *sgR, char *sgL);
    void __grid_placeAdjacentTB(char *g, char *sgT, char *sgB);
    void __grid_placeAdjacentBT(char *g, char *sgB, char *sgT);
    void __grid_connectTtoB(char *g, char *sg1, char  *sg2);
    void __grid_connectRtoL(char *g, char *sg1, char  *sg2);
    void __grid_connectBtoT(char *g, char *sg1, char  *sg2);
    void __grid_connectLtoR(char *g, char *sg1, char  *sg2);
    void __grid_connectLtoT(char *g, char *sg,  char *sgBL);
    void __grid_connectLtoB(char *g, char *sg,  char *sgTL);
    void __grid_connectRtoT(char *g, char *sg,  char *sgBR);
    void __grid_connectRtoB(char *g, char *sg,  char *sgTR);
    void __grid_connectTtoL(char *g, char *sg,  char *sgTR);
    void __grid_connectTtoR(char *g, char *sg,  char *sgTL);
    void __grid_connectBtoL(char *g, char *sg,  char *sgBR);
    void __grid_connectBtoR(char *g, char *sg,  char *sgBL);
    void __grid_wrapLR(char *g, char *sg);
    void __grid_wrapTB(char *g, char *sg);
    void __grid_mirrorT(char *g, char *sg);
    void __grid_mirrorB(char *g, char *sg);
    void __grid_mirrorL(char *g, char *sg);
    void __grid_mirrorR(char *g, char *sg);
    void __grid_foldT(char *g, char *sg);
    void __grid_foldB(char *g, char *sg);
    void __grid_foldL(char *g, char *sg);
    void __grid_foldR(char *g, char *sg);
    void __grid_numSubgrids(char *g, int &ret);
    void __grid_getSubgrid(char *g, int &idx, char *ret);

    void __distribution_new(char *dist);
    void __distribution_applyFillBlock(char *dist, char *grid,
                                       int &nProcs, int &blockH);
    void __distribution_applyBlockFill(char *dist, char *grid,
                                       int &nProcs, int &blockW);
    void __distribution_applyBlockCyclic(
        char *dist, char *grid, int &nProcs, int &blkW, int &blkH);
    void __distribution_visualize(char *dist, char *dirName);

    void __schedule_new(char *sched);
    void __schedule_calculate(char *sched, char *grid, char *dist);

    void __environment_print();
    void __environment_output(char *filename);
    void __environment_input(char *filename);
    void __environment_clear();

    // Functions used by GridWeaver:
    void __schedule_transferSizesToFortran(
        char *sched,
        int &size_msgRecvFrom,
        int &size_recvMsgStart,
        int &size_numTransfersInRecvMsg,
        int &size_transferRecvAtLBID,
        int &size_transferRegionRecvLowX,
        int &size_transferRegionRecvLowY,
        int &size_transferRegionRecvHighX,
        int &size_transferRegionRecvHighY,
        int &size_transferRecvOrientation,
        int &size_msgSendTo, 
        int &size_sendMsgStart,
        int &size_numTransfersInSendMsg,
        int &size_transferSendFromLBID,
        int &size_transferRegionSendLowX,
        int &size_transferRegionSendLowY,
        int &size_transferRegionSendHighX,
        int &size_transferRegionSendHighY,
        int &size_transferSendOrientation);

    
    void __schedule_transferToFortran(
        char *sched,
        int &nMsgsRecv,
        int *msgRecvFrom,
        int *recvMsgStart,
        int *numTransfersInRecvMsg,
        int *transferRecvAtLBID,
        int *transferRegionRecvLowX,
        int *transferRegionRecvLowY,
        int *transferRegionRecvHighX,
        int *transferRegionRecvHighY,
        int *transferRecvOrientation,
        int &nMsgsSend,
        int *msgSendTo, 
        int *sendMsgStart,
        int *numTransfersInSendMsg,
        int *transferSendFromLBID,
        int *transferRegionSendLowX,
        int *transferRegionSendLowY,
        int *transferRegionSendHighX,
        int *transferRegionSendHighY,
        int *transferSendOrientation);
    
    void __distribution_width(char *dist, int &ret);
    void __distribution_height(char *dist, int &ret);
    void __distribution_numLocalBlocks(char *dist, int &ret);
    void __distribution_lbid2gbid(char *dist, int &lbid, int &ret);
    void __distribution_gbid2lbid(char *dist, int &gbid, int &ret);
    void __distribution_gbid2proc(char *dist, int &gbid, int &ret);
    void __distribution_blockLowX(char *dist, int &gbid, int &ret);
    void __distribution_blockLowY(char *dist, int &gbid, int &ret);
    void __distribution_blockHighX(char *dist, int &gbid, int &ret);
    void __distribution_blockHighY(char *dist, int &gbid, int &ret);
    void __distribution_pos2BlockPos(
        char *dist, int &x, int &y, char *sg, int &blkX, int &blkY, int &gbid);

    void __setColor(int &color);
}

#endif
