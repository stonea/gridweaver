#include "runtimewrapper.hpp"
#include "stdio.h"
#include "stdlib.h"
#include "environment.hpp"
#include "utils.hpp"
#include "svgprinter.hpp"
#include "cellfieldpicture.hpp"
#include "vispage.hpp"
#include "binIO.hpp"

#include <string.h>
#include <iostream>
#include <fstream>
using namespace std;

static bool debug;
void initializeModule_runtimewrapper() {
    GW_DEBUG_CTRL("wrapper:all", debug);
}

void __gridweaver_initialize() {
    initializeModule_error();
    initializeModule_iprintable();
    initializeModule_svgprinter();
    initializeModule_cellfieldpicture();
    initializeModule_vispage();
    initializeModule_binio();
    initializeModule_utils();
    initializeModule_grid();
    initializeModule_environment();
    initializeModule_distribution();
    initializeModule_schedule();
    initializeModule_runtimewrapper();
}

void __turnOffSyntaxHighlighting() {
    DBG_MSG("In function __turnOffSyntaxHighlighting");

    turnOffSyntaxHighlighting();
}

void __neighbor_new(char *name, int &x, int &y) {
    DBG_MSG_V2("In function __neighbor_new.", x, y);

    Neighbor *n = Environment::newNeighbor(name);
    n->set(x, y);
}


void __subgrid_new(char *sg, int &width, int &height) {
    DBG_MSG_V2("In function __subgrid_new.", width, height);

    Subgrid *obj_sg = Environment::newSubgrid(sg);
    obj_sg->setExtents(width, height);
}

void __subgrid_width(char *sg, int &ret) {
    DBG_MSG_V("In function __subgrid_width", sg);

    Subgrid *obj_sg = Environment::getSubgrid(sg);
    ret = obj_sg->w();
}

void __subgrid_height(char *sg, int &ret) {
    DBG_MSG_V("In function __subgrid_height", sg);

    Subgrid *obj_sg = Environment::getSubgrid(sg);
    ret = obj_sg->h();
}

    
void __grid_new(char *g) {
    DBG_MSG_V("In function __grid_new.", g);

    Grid *obj_g = Environment::newGrid(g);
}

void __grid_addSubgrid(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_addSubgrid.", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->addSubgrid(obj_sg);
}

void __grid_addBorder(
    char *g,
    int &srcX1, int &srcY1, int &srcX2, int &srcY2, char* srcSG,
    int &tgtX1, int &tgtY1, int &tgtX2, int &tgtY2, char* tgtSG,
    int &rotation)
{
    DBG_MSG_V("In function __grid_addBorder.", g);

    Grid    *obj_g     = Environment::getGrid(g);
    Subgrid *obj_srcSG = Environment::getSubgrid(srcSG);
    Subgrid *obj_tgtSG = Environment::getSubgrid(tgtSG);
    obj_g->addBorder(Region(srcX1, srcY1, srcX2, srcY2), obj_srcSG,
                     Region(tgtX1, tgtY1, tgtX2, tgtY2), obj_tgtSG,
                     rotation);
}

void __grid_placeAdjacentLR(char *g, char *sgL, char *sgR) {
    DBG_MSG_V3("In function __grid_placeAdjacentLR.", g, sgL, sgR);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sgL = Environment::getSubgrid(sgL);
    Subgrid *obj_sgR = Environment::getSubgrid(sgR);
    
    obj_g->placeAdjacentLR(obj_sgL, obj_sgR);
}

void __grid_placeAdjacentRL(char *g, char *sgR, char *sgL) {
    DBG_MSG_V3("In function __grid_placeAdjacentRL.", g, sgR, sgL);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sgR = Environment::getSubgrid(sgR);
    Subgrid *obj_sgL = Environment::getSubgrid(sgL);

    obj_g->placeAdjacentRL(obj_sgR, obj_sgL);
}

void __grid_placeAdjacentTB(char *g, char *sgT, char *sgB) {
    DBG_MSG_V3("In function __grid_placeAdjacentTB", g, sgT, sgB);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sgT = Environment::getSubgrid(sgT);
    Subgrid *obj_sgB = Environment::getSubgrid(sgB);

    obj_g->placeAdjacentTB(obj_sgT, obj_sgB);
}

void __grid_placeAdjacentBT(char *g, char *sgB, char *sgT) {
    DBG_MSG_V3("In function __grid_placeAdjacentBT", g, sgB, sgT);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sgB = Environment::getSubgrid(sgB);
    Subgrid *obj_sgT = Environment::getSubgrid(sgT);

    obj_g->placeAdjacentBT(obj_sgB, obj_sgT);
}

void __grid_connectTtoB(char *g, char *sg1, char  *sg2) {
    DBG_MSG_V3("In function __grid_connectTtoB", g, sg1, sg2);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg1);
    Subgrid *obj_sg2 = Environment::getSubgrid(sg2);

    obj_g->connectTtoB(obj_sg1, obj_sg2);
}

void __grid_connectRtoL(char *g, char *sg1, char  *sg2) {
    DBG_MSG_V3("In function __grid_connectRtoL", g, sg1, sg2);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg1);
    Subgrid *obj_sg2 = Environment::getSubgrid(sg2);

    obj_g->connectRtoL(obj_sg1, obj_sg2);
}

void __grid_connectBtoT(char *g, char *sg1, char  *sg2) {
    DBG_MSG_V3("In function __grid_connectBtoT", g, sg1, sg2);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg1);
    Subgrid *obj_sg2 = Environment::getSubgrid(sg2);

    obj_g->connectBtoT(obj_sg1, obj_sg2);
}

void __grid_connectLtoR(char *g, char *sg1, char  *sg2) {
    DBG_MSG_V3("In function __grid_connectLtoR", g, sg1, sg2);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg1);
    Subgrid *obj_sg2 = Environment::getSubgrid(sg2);

    obj_g->connectLtoR(obj_sg1, obj_sg2);
}

void __grid_connectLtoT(char *g, char *sg,  char *sgBL) {
    DBG_MSG_V3("In function __grid_connectLtoT", g, sg, sgBL);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg);
    Subgrid *obj_sg2 = Environment::getSubgrid(sgBL);

    obj_g->connectLtoT(obj_sg1, obj_sg2);
}

void __grid_connectLtoB(char *g, char *sg,  char *sgTL) {
    DBG_MSG_V3("In function __grid_connectLtoB", g, sg, sgTL);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg);
    Subgrid *obj_sg2 = Environment::getSubgrid(sgTL);

    obj_g->connectLtoB(obj_sg1, obj_sg2);
}

void __grid_connectRtoT(char *g, char *sg,  char *sgBR) {
    DBG_MSG_V3("In function __grid_connectRtoT", g, sg, sgBR);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg);
    Subgrid *obj_sg2 = Environment::getSubgrid(sgBR);

    obj_g->connectRtoT(obj_sg1, obj_sg2);
}

void __grid_connectRtoB(char *g, char *sg,  char *sgTR) {
    DBG_MSG_V3("In function __grid_connectRtoB", g, sg, sgTR);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg);
    Subgrid *obj_sg2 = Environment::getSubgrid(sgTR);

    obj_g->connectRtoB(obj_sg1, obj_sg2);
}

void __grid_connectTtoL(char *g, char *sg,  char *sgTR) {
    DBG_MSG_V3("In function __grid_connectTtoL", g, sg, sgTR);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg);
    Subgrid *obj_sg2 = Environment::getSubgrid(sgTR);

    obj_g->connectTtoL(obj_sg1, obj_sg2);
}

void __grid_connectTtoR(char *g, char *sg,  char *sgTL) {
    DBG_MSG_V3("In function __grid_connectTtoR", g, sg, sgTL);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg);
    Subgrid *obj_sg2 = Environment::getSubgrid(sgTL);

    obj_g->connectTtoR(obj_sg1, obj_sg2);
}

void __grid_connectBtoL(char *g, char *sg,  char *sgBR) {
    DBG_MSG_V3("In function __grid_connectBtoL", g, sg, sgBR);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg);
    Subgrid *obj_sg2 = Environment::getSubgrid(sgBR);

    obj_g->connectBtoL(obj_sg1, obj_sg2);
}

void __grid_connectBtoR(char *g, char *sg,  char *sgBL) {
    DBG_MSG_V3("In function __grid_connectBtoR", g, sg, sgBL);
    
    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sg1 = Environment::getSubgrid(sg);
    Subgrid *obj_sg2 = Environment::getSubgrid(sgBL);

    obj_g->connectBtoR(obj_sg1, obj_sg2);
}

void __grid_wrapLR(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_wrapLR", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->wrapLR(obj_sg);
}

void __grid_wrapTB(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_wrapTB", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->wrapTB(obj_sg);
}

/*
void __grid_placeAdjacentWithOffsetLR(char *g, char *sgL, char *sgR,
                                      int shiftUp)
{
    DBG_MSG_V4("In function __grid_placeAdjacentWithOffsetLR",
              g, sgL, sgR, shiftUp);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sgL = Environment::getSubgrid(sgL);
    Subgrid *obj_sgR = Environment::getSubgrid(sgR);

    obj_g->placeAdjacentWithOffsetLR(obj_sgL, obj_sgR, shiftUp);
}

void __grid_placeAdjacentWithOffsetRL(char *g, char *sgR, char *sgL,
                                      int &shiftUp)
{
    DBG_MSG_V3("In function __grid_placeAdjacentWithOffsetRL",
               sgR, sgL, shiftUp);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sgR = Environment::getSubgrid(sgR);
    Subgrid *obj_sgL = Environment::getSubgrid(sgL);

    obj_g->placeAdjacentWithOffsetRL(obj_sgR, obj_sgL, shiftUp);
}

void __grid_placeAdjacentWithOffsetTB(char *g, char *sgT, char *sgB,
                                      int &shiftRight)
{
    DBG_MSG_V3("In function __grid_placeAdjacentWithOffsetTB", sgT, sgB,
               shiftRight);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sgT = Environment::getSubgrid(sgT);
    Subgrid *obj_sgB = Environment::getSubgrid(sgB);

    obj_g->placeAdjacentWithOffsetTB(obj_sgT, obj_sgB, shiftRight);
}

void __grid_placeAdjacentWithOffsetBT(char *g, char *sgB, char *sgT,
                                      int &shiftRight)
{
    DBG_MSG_V4("In function __grid_placeAdjacentWithOffsetBT", g, sgB, sgT,
               shiftRight);

    Grid    *obj_g   = Environment::getGrid(g);
    Subgrid *obj_sgB = Environment::getSubgrid(sgB);
    Subgrid *obj_sgT = Environment::getSubgrid(sgT);

    obj_g->placeAdjacentWithOffsetBT(obj_sgB, obj_sgT, shiftRight);
}
*/
void __grid_mirrorT(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_mirrorT", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->mirrorT(obj_sg);
}

void __grid_mirrorB(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_mirrorB", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->mirrorB(obj_sg);
}

void __grid_mirrorL(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_mirrorL", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->mirrorL(obj_sg);
}

void __grid_mirrorR(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_mirrorR", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->mirrorR(obj_sg);
}

void __grid_foldT(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_foldT", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->foldT(obj_sg);
}

void __grid_foldB(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_foldB", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->foldB(obj_sg);
}

void __grid_foldL(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_foldL", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->foldL(obj_sg);
}

void __grid_foldR(char *g, char *sg) {
    DBG_MSG_V2("In function __grid_foldR", g, sg);

    Grid    *obj_g  = Environment::getGrid(g);
    Subgrid *obj_sg = Environment::getSubgrid(sg);

    obj_g->foldR(obj_sg);
}

void __grid_numSubgrids(char *g, int &ret) {
    DBG_MSG_V("In function __grid_foldR", g);

    Grid    *obj_g  = Environment::getGrid(g);
    ret = obj_g->numSubgrids();
}

void __grid_getSubgrid(char *g, int &idx, char *ret) {
    DBG_MSG_V("In function __grid_getSubgrid", g);

    Grid    *obj_g  = Environment::getGrid(g);
    strcpy(ret, obj_g->subgrid(idx)->getID().c_str());
}

void __distribution_new(char *name) {
    DBG_MSG_V("In function __distribution_new.", name);

    Distribution *obj_dist = Environment::newDistribution(name);
}

void __distribution_applyFillBlock(char *dist, char *grid,
                                   int &nProcs, int &blockH)
{
    DBG_MSG_V3("In function __distribution_applyFillBlock",
               dist, nProcs, blockH);

    Distribution *obj_dist = Environment::getDistribution(dist);
    Grid         *obj_g    = Environment::getGrid(grid);
    obj_dist->applyFillBlock(obj_g, nProcs, blockH);
}

void __distribution_applyBlockFill(char *dist, char *grid,
                                   int &nProcs, int &blockW)
{
    DBG_MSG_V3("In function __distribution_applyFillBlock",
               dist, nProcs, blockW);

    Distribution *obj_dist = Environment::getDistribution(dist);
    Grid         *obj_g    = Environment::getGrid(grid);
    obj_dist->applyBlockFill(obj_g, nProcs, blockW);
}

void __distribution_applyBlockCyclic(
    char *dist, char *grid, int &nProcs, int &blkW, int &blkH)
{
    DBG_MSG_V5("In function __distribution_applyBlockCyclic.",
               dist, grid, nProcs, blkW, blkH);

    Distribution *obj_dist = Environment::getDistribution(dist);
    Grid         *obj_g    = Environment::getGrid(grid);
    obj_dist->applyBlockCyclic(obj_g, nProcs, blkW, blkH);
}

void __distribution_visualize(char *dist, char *dirName) {
    DBG_MSG_V2("In function __distribution_visualize.", dist, dirName);

    Distribution *obj_dist = Environment::getDistribution(dist);
    obj_dist->visualize(dirName);
}

void __schedule_new(char *sched) {
    DBG_MSG_V("In function __schedule_new.", sched);

    Schedule *obj_sched = Environment::newSchedule(sched);
}

void __schedule_calculate(char *sched, char *grid, char *dist) {
    DBG_MSG_V3("In function __schedule_calculate.", sched, grid, dist);

    Schedule     *obj_sched = Environment::getSchedule(sched);
    Grid         *obj_g     = Environment::getGrid(grid);
    Distribution *obj_dist  = Environment::getDistribution(dist);
    obj_sched->calculate(obj_g, obj_dist);
}

void __environment_print() {
    DBG_MSG("In function __environment_print.");

    Environment::print(cout);
}

void __environment_output(char *filename) {
    DBG_MSG_V("In function __environment_output.", filename);

    ofstream file(filename);
    Environment::output(file);
    file.close();
}

void __environment_input(char *filename) {
    DBG_MSG_V("In function __environment_input.", filename);

    ifstream file(filename);
    Environment::input(file);
    file.close();
}

void __environment_clear() {
    DBG_MSG("In function __environment_clear.");

    Environment::clear();
}

//---------------------------------------------------------

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
    int &size_transferSendOrientation)
{
    DBG_MSG("In function __schedule_transferSizesToFortran");
    
    Schedule *obj_sched = Environment::getSchedule(sched);
    
    obj_sched->transferSizesToFortran(
        size_msgRecvFrom,
        size_recvMsgStart,
        size_numTransfersInRecvMsg,
        size_transferRecvAtLBID,
        size_transferRegionRecvLowX,
        size_transferRegionRecvLowY,
        size_transferRegionRecvHighX,
        size_transferRegionRecvHighY,
        size_transferRecvOrientation, 
        size_msgSendTo, 
        size_sendMsgStart,
        size_numTransfersInSendMsg, 
        size_transferSendFromLBID, 
        size_transferRegionSendLowX,
        size_transferRegionSendLowY, 
        size_transferRegionSendHighX,
        size_transferRegionSendHighY, 
        size_transferSendOrientation);
}

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
    int *transferSendOrientation)
{
    DBG_MSG("In function __schedule_transferToFortran");
    
    Schedule *obj_sched = Environment::getSchedule(sched);
    
    obj_sched->transferToFortran(
        nMsgsRecv,
        msgRecvFrom,
        recvMsgStart,
        numTransfersInRecvMsg,
        transferRecvAtLBID,
        transferRegionRecvLowX,
        transferRegionRecvLowY,
        transferRegionRecvHighX,
        transferRegionRecvHighY,
        transferRecvOrientation,
        nMsgsSend,
        msgSendTo,
        sendMsgStart,
        numTransfersInSendMsg,
        transferSendFromLBID,
        transferRegionSendLowX,
        transferRegionSendLowY,
        transferRegionSendHighX,
        transferRegionSendHighY,
        transferSendOrientation);
}

void __distribution_width(char *dist, int &ret) {
    DBG_MSG("In function __distribution_width");
    
    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->blockWidth();
}

void __distribution_height(char *dist, int &ret) {
    DBG_MSG("In function __distribution_height");

    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->blockHeight();
}

void __distribution_numLocalBlocks(char *dist, int &ret) {
    DBG_MSG("In function __distribution_numLocalBlocks");
    
    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->numLclBlocksForProc(myRank());
}

void __distribution_lbid2gbid(char *dist, int &lbid, int &ret) {
    DBG_MSG("In function __distribution_lbid2gbid");
    
    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->lbid2gbid(myRank(), lbid);
}

void __distribution_gbid2lbid(char *dist, int &gbid, int &ret) {
    DBG_MSG("In function __distribution_gbid2lbid");
    
    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->gbid2lbid(gbid);
}

void __distribution_gbid2proc(char *dist, int &gbid, int &ret) {
    DBG_MSG("In function __distribution_gbid2proc");
    
    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->gbidProc(gbid);
}

void __distribution_blockLowX(char *dist, int &gbid, int &ret) {
    DBG_MSG("In function __distribution_blockLowX");
    
    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->gbidRegion(gbid).lowX();
}

void __distribution_blockLowY(char *dist, int &gbid, int &ret) {
    DBG_MSG("In function __distribution_blockLowY");
    
    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->gbidRegion(gbid).lowY();
}

void __distribution_blockHighX(char *dist, int &gbid, int &ret) {
    DBG_MSG("In function __distribution_blockHighX");
    
    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->gbidRegion(gbid).highX();
}

void __distribution_blockHighY(char *dist, int &gbid, int &ret) {
    DBG_MSG("In function __distribution_blockHighY");
    
    Distribution *obj_dist = Environment::getDistribution(dist);
    ret = obj_dist->gbidRegion(gbid).highY();
}

void __distribution_pos2BlockPos(
    char *dist, int &x, int &y, char *sg, int &blkX, int &blkY, int &gbid)
{
    DBG_MSG_V3("In function __distribution_pos2BlockPos", x, y, sg);

    Distribution *obj_dist = Environment::getDistribution(dist);
    Subgrid *obj_sg = Environment::getSubgrid(sg);
    
    obj_dist->pos2BlockPos(x, y, obj_sg, blkX, blkY, gbid);
}


void __setColor(int &color) {
    DBG_MSG("In function __setColor");

    if(color == -1) { cout << "\e[0m"; }
    if(color == -2) { cout << "\e[4;30m"; }
    if(color == -3) { cout << "\e[1;30m"; }
    if(color == -4) { cout << "\e[40;97m"; }
    if(color == -5) { cout << COLOR_WHITE; }
    
    switch(color % 10) {
        case 0: cout << COLOR_RED;          break;
        case 1: cout << COLOR_GREEN;        break;
        case 2: cout << COLOR_BLUE;         break;
        case 3: cout << COLOR_MAGENTA;      break;
        case 4: cout << COLOR_CYAN;         break;
        case 5: cout << COLOR_ERED;         break;
        case 6: cout << COLOR_EGREEN;       break;
        case 7: cout << COLOR_EYELLOW;      break;
        case 8: cout << COLOR_EBLUE;        break;
        case 9: cout << COLOR_EMAGENTA;     break;
    };
}

