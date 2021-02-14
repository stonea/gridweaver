/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "schedule.hpp"
#include "utils.hpp"
#include "grid.hpp"
#include "binIO.hpp"
#include <mpi.h>
#include <iostream>
using namespace std;

void initializeModule_schedule() {
}

Schedule::Schedule(const string &name) :
    mName(name),
    mGrid(NULL),
    mDist(NULL)
{
}

void Schedule::calculate(Grid *g, Distribution *dist) {
    // Copy parameters into member variables
    mGrid = g;
    mDist = dist;
    int nProcs = dist->numProcs();

    // Iterate through each process
    for(int pid = 0; pid < nProcs; pid++) {
        // Iterate through every local block
        for(int lbid = 1; lbid <= dist->numLclBlocksForProc(pid); lbid++) {
            // Get information about the block
            int gbid    = mDist->lbid2gbid(pid, lbid);
            Subgrid *sg = mDist->gbidSubgrid(gbid);

            // Construct a region representing the data that is accessed by
            // this block (that is get the region for the block than expand it
            // to include the points in its halo).
            Region rgnDataAccessed = mDist->gbidRegion(gbid);
            rgnDataAccessed.expand(-1, -1, 1, 1);
            
            // Find all blocks that intersect with the region of data accessed
            // by the given block
            set<int> surroundingBlocks =
                dist->gbidsIntersectingRegion(sg, rgnDataAccessed);
            surroundingBlocks.erase(gbid);
            
            // Iterate through each of the surrounding blocks and register a
            // transfer for the region that overlaps the block and the region
            // of accessed points of block gbid
            for(set<int>::iterator i  = surroundingBlocks.begin();
                                   i != surroundingBlocks.end(); i++)
            {
                Region r = mDist->gbidRegion(*i);
                r.cut(rgnDataAccessed);
                registerTransfer(*i, r, gbid, r);
            }
                
        }
    }

    // Set number of messages to send/receive
    mnMsgsRecv = mMsgRecvFrom.size();
    mnMsgsSend = mMsgSendTo.size();
}


void Schedule::print(ostream &out) const {
    // Dummy iterators
    vector<int>::iterator intVectorDummy;
    vector<Region>::iterator regionVectorDummy;

    if(isMasterRank()) {
        printObj_start(out, "Schedule", mName);
        printObj_property(out, "grid", mGrid);
        printObj_property(out, "dist", mDist);
        
        if(mGrid == NULL || mDist == NULL) {
            printObj_end(out);
            return;
        } else {
            out << endl;
        }
    }

    // --- Print out receiving information for each proccess: ---
        for(int pid = 0; pid < mDist->numProcs(); pid++) {
            // Determine how many messages are received on the specified pid
            int nMsgsRecv = mTransferRecvAtLBID.size();
            MPI_Bcast(&nMsgsRecv, 1, MPI_INT, pid, MPI_COMM_WORLD);

            if(isMasterRank()) {
                printObj_startSection(out,
                    "Receiving info for proccess " + str(pid) + ":");
            }
            printObj_propertyFromRank(out, pid, "nMsgRecv",
                str(mMsgRecvFrom.size()));
            printObj_propertyFromRank(out, pid, "msgRecvFrom");
            printValsFromRank(out, pid, mMsgRecvFrom.begin(),
                                        mMsgRecvFrom.end());
            
            // print transferRecvAtLBID and mTransferRegionRecv for each
            // message
            for(int i = 0; i < nMsgsRecv; i++) {
                printObj_propertyFromRank(out, pid, "mTransferRecvAtLBID[" +
                                          str(i) + "]");
                printValsFromRank(out, pid,
                    (myRank() == pid) ? mTransferRecvAtLBID[i].begin()
                                      : intVectorDummy,
                    (myRank() == pid) ? mTransferRecvAtLBID[i].end()
                                      : intVectorDummy);
            }
            for(int i = 0; i < nMsgsRecv; i++) {
                printObj_propertyFromRank(out, pid, "mTransferRegionRecv[" +
                                  str(i) + "]");
                printValsFromRank(out, pid,
                    (myRank() == pid) ? mTransferRegionRecv[i].begin()
                                      : regionVectorDummy,
                    (myRank() == pid) ? mTransferRegionRecv[i].end()
                                      : regionVectorDummy);
            }

            if(isMasterRank()) {
                printObj_endSection(out);
            }
        }
    
    // create a separator between the receiver side and sender side info
    if(isMasterRank()) {
        cout << endl;
        cout << indt << hiFmt("               * * * * * * * * *");
        cout << endl;
    }

    // --- Print out sending information for each proccess: ---
        for(int pid = 0; pid < mDist->numProcs(); pid++) {
            // Determine how many messages are sent on the specified pid
            int nMsgsSend = mTransferSendFromLBID.size();
            MPI_Bcast(&nMsgsSend, 1, MPI_INT, pid, MPI_COMM_WORLD);

            if(isMasterRank()) {
                printObj_startSection(out,
                    "Sending info for proccess " + str(pid) + ":");
            }
            printObj_propertyFromRank(out, pid, "nMsgSend",
                str(mMsgSendTo.size()));
            printObj_propertyFromRank(out, pid, "msgSendTo");
            printValsFromRank(out, pid, mMsgSendTo.begin(), mMsgSendTo.end());
            
            // print transferSendFromLBID and mTransferRegionSend for each proccess
            for(int i = 0; i < nMsgsSend; i++) {
                printObj_propertyFromRank(out, pid, "mTransferSendFromLBID[" +
                                          str(i) + "]");
                printValsFromRank(out, pid,
                    (myRank() == pid) ? mTransferSendFromLBID[i].begin()
                                      : intVectorDummy,
                    (myRank() == pid) ? mTransferSendFromLBID[i].end()
                                      : intVectorDummy);
            }
            for(int i = 0; i < nMsgsSend; i++) {
                printObj_propertyFromRank(out, pid, "mTransferRegionSend[" +
                                          str(i) + "]");
                printValsFromRank(out, pid,
                    (myRank() == pid) ? mTransferRegionSend[i].begin()
                                      : regionVectorDummy,
                    (myRank() == pid) ? mTransferRegionSend[i].end()
                                      : regionVectorDummy);
            }

            if(isMasterRank()) {
                printObj_endSection(out);
            }
        }

    if(isMasterRank()) {
        printObj_end(out);
    }
}

void Schedule::printSimp(ostream &out) const {
    out << getID();
}

void Schedule::output(ostream &out) const {
    #if 0
    BinIO::out(out, mName);
    BinIO::outIdent(out, *mGrid);
    BinIO::outIdent(out, *mDist);
    BinIO::out(out, mnMsgsRecv, mDist->numProcs());
    BinIO::out(out, mMsgRecvFrom, mDist->numProcs());
    BinIO::out(out, mTransferRecvAtLBID, mDist->numProcs());
    BinIO::out(out, mTransferRegionRecv, mDist->numProcs());
    BinIO::out(out, mMsgSendTo, mDist->numProcs());
    BinIO::out(out, mTransferSendFromLBID, mDist->numProcs());
    BinIO::out(out, mTransferRegionSend, mDist->numProcs());
    #endif
}

void Schedule::input(istream &in) {
    #if 0
    BinIO::in(in, mName);
    BinIO::inIdent(in, &mGrid, &Environment::getGrid);
    BinIO::inIdent(in, &mDist, &Environment::getDistribution);
    BinIO::in(in, &mnMsgsRecv);
    BinIO::in(in, &mMsgRecvFrom);
    BinIO::in(in, &mTransferRecvAtLBID);
    BinIO::in(in, &mTransferRegionRecv);
    BinIO::in(in, &mMsgSendTo);
    BinIO::in(in, &mTransferSendFromLBID);
    BinIO::in(in, &mTransferRegionSend);
    #endif
}

void Schedule::transferSizesToFortran(
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
    size_msgRecvFrom  = mMsgRecvFrom.size();
    size_recvMsgStart = size_msgRecvFrom;

    size_numTransfersInRecvMsg = size_msgRecvFrom;
    
    int nTransfers = 0;
    for(int i = 0; i < mTransferRecvAtLBID.size(); i++) {
        nTransfers += mTransferRecvAtLBID[i].size();
    }
    size_transferRecvAtLBID = nTransfers;
    size_transferRegionRecvLowX = nTransfers;
    size_transferRegionRecvLowY = nTransfers;
    size_transferRegionRecvHighX = nTransfers;
    size_transferRegionRecvHighY = nTransfers;
    size_transferRecvOrientation = nTransfers;
   

    size_msgSendTo = mMsgSendTo.size();
    size_sendMsgStart = size_msgSendTo;
    size_numTransfersInSendMsg = size_msgSendTo;
    
    nTransfers = 0;
    for(int i = 0; i < mTransferSendFromLBID.size(); i++) {
        nTransfers += mTransferSendFromLBID[i].size();
    }
    size_transferSendFromLBID = nTransfers;
    size_transferRegionSendLowX = nTransfers;
    size_transferRegionSendLowY = nTransfers;
    size_transferRegionSendHighX = nTransfers;
    size_transferRegionSendHighY = nTransfers;
    size_transferSendOrientation = nTransfers;
}

void Schedule::transferToFortran(
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
    // Transfer receiving messages
    nMsgsRecv = mnMsgsRecv;
    int offset = 0;
    for(int i = 0; i < mnMsgsRecv; i++) {
        msgRecvFrom[i] = mMsgRecvFrom[i];
        recvMsgStart[i] = offset + 1;
        numTransfersInRecvMsg[i] = mTransferRecvAtLBID[i].size();
        
        // Store transfers for the message
        for(int j = 0; j < mTransferRecvAtLBID[i].size(); j++) {
            transferRecvAtLBID[offset] = mTransferRecvAtLBID[i][j];
            transferRegionRecvLowX[offset] = mTransferRegionRecv[i][j].lowX();
            transferRegionRecvLowY[offset] = mTransferRegionRecv[i][j].lowY();
            transferRegionRecvHighX[offset] = mTransferRegionRecv[i][j].highX();
            transferRegionRecvHighY[offset] = mTransferRegionRecv[i][j].highY();
            transferRecvOrientation[offset] =
                mTransferRegionRecv[i][j].orientation();
            offset++;
        }
    }

    // Transfer sending messages
    nMsgsSend = mnMsgsSend;
    offset = 0;
    for(int i = 0; i < mnMsgsSend; i++) {
        msgSendTo[i] = mMsgSendTo[i];
        sendMsgStart[i] = offset + 1;
        numTransfersInSendMsg[i] = mTransferSendFromLBID[i].size();
        
        // Store transfers for the message
        for(int j = 0; j < mTransferSendFromLBID[i].size(); j++) {
            transferSendFromLBID[offset] = mTransferSendFromLBID[i][j];
            transferRegionSendLowX[offset] = mTransferRegionSend[i][j].lowX();
            transferRegionSendLowY[offset] = mTransferRegionSend[i][j].lowY();
            transferRegionSendHighX[offset] = mTransferRegionSend[i][j].highX();
            transferRegionSendHighY[offset] = mTransferRegionSend[i][j].highY();
            transferSendOrientation[offset] =
                mTransferRegionSend[i][j].orientation();
            offset++;
        }
    }
}

void Schedule::registerTransfer(int srcGbid, const Region &srcReg,
                                int tgtGbid, const Region &tgtReg)
{
    // Convert srcReg and tgtReg so that they are in the index space of the
    // block instead of the subgrid.  That is position (1,1) should be the
    // lower-left corner of srcGbid for srcReg and of tgtGbid for tgtReg.
    Region srcRegTranslated = srcReg;
    srcRegTranslated.translate(-mDist->gbidRegion(srcGbid).lowX()+1,
                               -mDist->gbidRegion(srcGbid).lowY()+1);
    Region tgtRegTranslated = tgtReg;
    tgtRegTranslated.translate(-mDist->gbidRegion(tgtGbid).lowX()+1,
                               -mDist->gbidRegion(tgtGbid).lowY()+1);

    // Determine the PID of the receiving and sending processes
    int pidRecv = mDist->gbidProc(tgtGbid);
    int pidSend = mDist->gbidProc(srcGbid);

    //-- Create the receiving side message --//
    if(myRank() == pidRecv) {
        // Find the message to place the receiving side of the transfer in.  If
        // it doesn't already exists then create it.
        int msgID = -1;
        for(int i = 0; i < mMsgRecvFrom.size(); i++) {
            if(pidSend == mMsgRecvFrom[i]) {
                msgID = i; break;
            }
        }
        if(msgID == -1) {
            mMsgRecvFrom.push_back(pidSend);
            mTransferRecvAtLBID.push_back(vector<int>());
            mTransferRegionRecv.push_back(vector<Region>());
            msgID = mTransferRecvAtLBID.size() - 1;
        }

        // Add the transfer to the receiving side message
        mTransferRecvAtLBID[msgID].push_back(
            mDist->gbid2lbid(tgtGbid));
        mTransferRegionRecv[msgID].push_back(tgtRegTranslated);
    }
    
    //-- Create the sending side message --//
    if(myRank() == pidSend) {
        // Find the message to place the receiving side of the transfer in.  If
        // it doesn't already exists then create it.
        int msgID = -1;
        for(int i = 0; i < mMsgSendTo.size(); i++) {
            if(pidRecv == mMsgSendTo[i]) {
                msgID = i; break;
            }
        }
        if(msgID == -1) {
            mMsgSendTo.push_back(pidRecv);
            mTransferSendFromLBID.push_back(vector<int>());
            mTransferRegionSend.push_back(vector<Region>());
            msgID = mTransferSendFromLBID.size() - 1;
        }

        // Add the transfer to the receiving side message
        mTransferSendFromLBID[msgID].push_back(
            mDist->gbid2lbid(srcGbid));
        mTransferRegionSend[msgID].push_back(srcRegTranslated);
    }
}


void Schedule::saveIntVector(std::ostream &out,
                             const std::vector<int> &obj)
{
    BinIO::out(out, obj.begin(), obj.end());
}

void Schedule::loadIntVector(std::istream &in, std::vector<int> &obj)
{
    int blankInt;
    BinIO::in(in, back_inserter(obj), blankInt);
}

void Schedule::saveIntVectorVector(ostream &out,
                                   const vector<vector<int> > &obj)
{
    BinIO::out(out, obj.begin(), obj.end(), &saveIntVector);
}

void Schedule::loadIntVectorVector(std::istream &in,
                                   std::vector<std::vector<int> > &obj)
{
    vector<int> blankIntVector;
    BinIO::in(in, back_inserter(obj), &loadIntVector, blankIntVector);
}

void Schedule::saveRegionVector(std::ostream &out,
                                const std::vector<Region> &obj)
{
    BinIO::out(out, obj.begin(), obj.end());
}

void Schedule::saveRegionVectorVector(
    std::ostream &out, const std::vector<std::vector<Region> > &obj)
{
    BinIO::out(out, obj.begin(), obj.end(), &saveRegionVector);
}

