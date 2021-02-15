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

void Schedule::calculate(Grid *g, Distribution *dist, int depth) {
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
            //rgnDataAccessed.expand(-1, -1, 1, 1);
            rgnDataAccessed.expand(-depth, -depth, depth, depth);
            
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
                
            // Iterate through border mappings and register a transfer if
            // there's any overlap with the region of accessed points of block
            // gbid
            for(int i = 0; i < mGrid->mBorderSrcRegions.size(); i++) {
                // Check that the border mapping is for the appropriate subgrid
                if(mGrid->mBorderSrcSubgrids[i] != sg) continue;

                // Check for overlap; stop with this border map if there is
                // no overlap
                Region srcIntersection =
                    rgnDataAccessed.intersect(mGrid->mBorderSrcRegions[i]);
                if(srcIntersection.isEmpty()) continue;

                // Determine the analogous region on the target side
                Region analogousRegion = 
                    mGrid->mBorderSrcRegions[i].analogousRegion(
                        srcIntersection, mGrid->mBorderTgtRegions[i]);

                // Find all blocks that intersect with the analogous region
                set<int> overlappingBlocks =
                    dist->gbidsIntersectingRegion(
                        mGrid->mBorderTgtSubgrids[i], analogousRegion);

                //cout << "Check region: " << analogousRegion << endl;
                //cout << "Number of overlapping blocks: " << overlappingBlocks.size() << endl;

                // Iterate through each of the overlapping blocks and register a
                // transfer for the region that overlaps the block and the
                // region of accessed points of block gbid
                for(set<int>::iterator j  = overlappingBlocks.begin();
                                       j != overlappingBlocks.end(); j++)
                {
                    Region regOnHalo, regOnBorder;

                    Region r = mDist->gbidRegion(*j);

                    analogousRegion.cutAnalogously(
                        r, 
                        srcIntersection, 
                        regOnBorder,
                        regOnHalo);
                    
                    registerTransfer(*j, regOnBorder, gbid, regOnHalo);
                }
            }
        }
    }

    // Set number of messages to send/receive
    mnMsgsRecv = mMsgRecvFrom.size();
    mnMsgsSend = mMsgSendTo.size();
}


void Schedule::calculateGhostNodePlan(Grid *g, Distribution *dist, int depth) {
    // Copy parameters into member variables
    mGrid = g;
    mDist = dist;
    int nProcs = dist->numProcs();

    // Create a maps: proc X (gbid received from) -> (set of coordinates)
    map<int, set<GlobalCoordinate> > coordinatesProcRecvs[nProcs];

    // Iterate through each process gathering the coordinates needed for it
    for(int pid = 0; pid < nProcs; pid++) {
        // Iterate through every local block
        for(int lbid = 1; lbid <= dist->numLclBlocksForProc(pid); lbid++) {
            // Get information about the block
            int gbid    = mDist->lbid2gbid(pid, lbid);
            Subgrid *sg = mDist->gbidSubgrid(gbid);

            // If this is an interior block we don't need to calculate ghost
            // nodes for it
            if(mDist->isInteriorBlock(gbid)) { continue; }

            // Get a list of nodes reachable from the block, have processor
            // PID recall that it needs to store these
            set<GlobalCoordinate> coordinates = 
                breadthFirstExpansionFromBlock(gbid, depth);

            // Insert coordinates in maps
            for(set<GlobalCoordinate>::iterator coord = coordinates.begin();
                coord != coordinates.end(); coord++)
            {
                int recvFromGbid = mDist->gbidAtPos(coord->sg, coord->x, coord->y);
                coordinatesProcRecvs[pid][recvFromGbid].insert(*coord);
            }
        }
    }

    // Iterate through each process checking to see if it receives a message
    // from this proc.  If so create a sending message to it.
    mnGhostMsgsRecv = 0;
    mnGhostMsgsSend = 0;
    for(int pidRecv = 0; pidRecv < nProcs; pidRecv++) {
        // Iterate through every block local block to see if its sends to
        // pidRecv
        for(int lbidSend  = 1;
                lbidSend <= dist->numLclBlocksForProc(myRank()); lbidSend++)
        {
            // Get information about the block
            int gbidSend = mDist->lbid2gbid(myRank(), lbidSend);

            // If pidRecv should receive nodes from gbidSend construct a sending
            // transfer from gbidSend
            if(!coordinatesProcRecvs[pidRecv][gbidSend].empty()) {
                // Find the message to place the sending side of the
                // transfer in.  If it doesn't already exists then create
                // it.
                int msgID = -1;
                for(int j = 0; j < mGhostMsgSendTo.size(); j++) {
                    if(pidRecv == mGhostMsgSendTo[j]) {
                        msgID = j; break;
                    }
                }
                if(msgID == -1) {
                    mnGhostMsgsSend++;
                    mGhostMsgSendTo.push_back(pidRecv);
                    mTransferGhostCoordsSend.push_back(vector<GlobalCoordinate>());
                    msgID = mGhostMsgSendTo.size() - 1;
                }

                // Add coordinates to the message
                for(set<GlobalCoordinate>::iterator
                        j  = coordinatesProcRecvs[pidRecv][gbidSend].begin();
                        j != coordinatesProcRecvs[pidRecv][gbidSend].end(); j++)
                {
                    mTransferGhostCoordsSend[msgID].push_back(*j);
                }
            }
        }
    }

    // Iterate through each block this processor receives from
    for(map<int, set<GlobalCoordinate> >::const_iterator 
        gbidIter  = coordinatesProcRecvs[myRank()].begin();
        gbidIter != coordinatesProcRecvs[myRank()].end(); gbidIter++)
    {
        int gbidSend = gbidIter->first;
        int lbidSend = mDist->gbid2lbid(gbidSend);
        int pidSend  = mDist->gbidProc(gbidSend);

        // No need to create a message if we don't receive from gbidSend
        if(coordinatesProcRecvs[myRank()][gbidSend].size() == 0) { continue; }

        // Find the message to place the receiving side of the
        // transfer in.  If it doesn't already exists then create
        // it.
        int msgID = -1;
        for(int j = 0; j < mGhostMsgRecvFrom.size(); j++) {
            if(pidSend == mGhostMsgRecvFrom[j]) {
                msgID = j; break;
            }
        }
        if(msgID == -1) {
            mnGhostMsgsRecv++;
            mGhostMsgRecvFrom.push_back(pidSend);
            mTransferGhostCoordsRecv.push_back(vector<GlobalCoordinate>());
            msgID = mGhostMsgRecvFrom.size() - 1;
        }

        // Add coordinates to the message
        for(set<GlobalCoordinate>::iterator
            j  = coordinatesProcRecvs[myRank()][gbidSend].begin();
            j != coordinatesProcRecvs[myRank()][gbidSend].end(); j++)
        {
            mTransferGhostCoordsRecv[msgID].push_back(*j);
        }
    }
}

void Schedule::print(ostream &out) const {
    // Dummy iterators
    vector<int>::iterator intVectorDummy;
    vector<Region>::iterator regionVectorDummy;
    vector<GlobalCoordinate>::iterator globalCoordVectorDummy;

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
                    "Halo receiving info for proccess " + str(pid) + ":");
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
        cout << indt << hiFmt("    * * * * * * * * * * * * * * * * * * * *");
        cout << endl;
    }

    // --- Print out sending information for each proccess: ---
        for(int pid = 0; pid < mDist->numProcs(); pid++) {
            // Determine how many messages are sent on the specified pid
            int nMsgsSend = mTransferSendFromLBID.size();
            MPI_Bcast(&nMsgsSend, 1, MPI_INT, pid, MPI_COMM_WORLD);

            if(isMasterRank()) {
                printObj_startSection(out,
                    "Halo sending info for proccess " + str(pid) + ":");
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

    // create a separator between halo and ghost schedules
    if(isMasterRank()) {
        cout << endl;
        cout << indt << hiFmt("    =======================================");
        cout << endl;
    }

    // --- Print out receiving information for ghosts on each proccess: ---
        for(int pid = 0; pid < mDist->numProcs(); pid++) {
            // Determine how many messages are received on the specified pid
            int nGhostMsgsRecv = mnGhostMsgsRecv;
            MPI_Bcast(&nGhostMsgsRecv, 1, MPI_INT, pid, MPI_COMM_WORLD);

            if(isMasterRank()) {
                printObj_startSection(out,
                    "Ghost receiving info for proccess " + str(pid) + ":");
            }
            printObj_propertyFromRank(out, pid, "nGhostMsgRecv",
                str(mGhostMsgRecvFrom.size()));
            printObj_propertyFromRank(out, pid, "ghostMsgRecvFrom");
            printValsFromRank(out, pid, mGhostMsgRecvFrom.begin(),
                                        mGhostMsgRecvFrom.end());
            
            // print coordinates for each message
            for(int i = 0; i < nGhostMsgsRecv; i++) {
                printObj_propertyFromRank(out, pid, "mTransferGhostCoordsRecv[" +
                                  str(i) + "]");

                printValsFromRank(out, pid,
                    (myRank() == pid) ? mTransferGhostCoordsRecv[i].begin()
                                      : globalCoordVectorDummy,
                    (myRank() == pid) ? mTransferGhostCoordsRecv[i].end()
                                      : globalCoordVectorDummy);
            }

            if(isMasterRank()) {
                printObj_endSection(out);
            }
        }

    // create a separator between the receiver side and sender side info
    if(isMasterRank()) {
        cout << endl;
        cout << indt << hiFmt("    * * * * * * * * * * * * * * * * * * * *");
        cout << endl;
    }

    // --- Print out sending information for ghosts on each proccess: ---
        for(int pid = 0; pid < mDist->numProcs(); pid++) {
            // Determine how many messages are sent on the specified pid
            int nGhostMsgsSend = mnGhostMsgsSend;
            MPI_Bcast(&nGhostMsgsSend, 1, MPI_INT, pid, MPI_COMM_WORLD);

            if(isMasterRank()) {
                printObj_startSection(out,
                    "Ghost sending info for proccess " + str(pid) + ":");
            }
            printObj_propertyFromRank(out, pid, "nGhostMsgSend",
                str(mGhostMsgSendTo.size()));
            printObj_propertyFromRank(out, pid, "ghostMsgSendTo");
            printValsFromRank(out, pid, mGhostMsgSendTo.begin(), mGhostMsgSendTo.end());
            
            // print transferSendFromLBID and mTransferRegionSend for each proccess
            for(int i = 0; i < nGhostMsgsSend; i++) {
                printObj_propertyFromRank(out, pid, "mTransferGhostCoordsSend[" +
                                          str(i) + "]");
                printValsFromRank(out, pid,
                    (myRank() == pid) ? mTransferGhostCoordsSend[i].begin()
                                      : globalCoordVectorDummy,
                    (myRank() == pid) ? mTransferGhostCoordsSend[i].end()
                                      : globalCoordVectorDummy);
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

void Schedule::transferGhostSizesToFortran(
    int &size_ghostMsgRecvFrom,
    int &size_recvGhostMsgStart,
    int &size_recvGhostMsgSG,
    int &size_recvGhostMsgX,
    int &size_recvGhostMsgY,
    int &size_ghostMsgSendTo,
    int &size_sendGhostMsgStart,
    int &size_sendGhostMsgSG,
    int &size_sendGhostMsgX,
    int &size_sendGhostMsgY)
{
    size_ghostMsgRecvFrom  = mnGhostMsgsRecv;
    size_recvGhostMsgStart = mnGhostMsgsRecv+1;
    size_ghostMsgSendTo    = mnGhostMsgsSend;
    size_sendGhostMsgStart = mnGhostMsgsSend+1;

    int nCoordsRecv = 0;
    for(int i = 0; i < mnGhostMsgsRecv; i++) {
        nCoordsRecv += mTransferGhostCoordsRecv[i].size();
    }
    size_recvGhostMsgSG = nCoordsRecv;
    size_recvGhostMsgX  = nCoordsRecv;
    size_recvGhostMsgY  = nCoordsRecv;

    int nCoordsSend = 0;
    for(int i = 0; i < mnGhostMsgsSend; i++) {
        nCoordsSend += mTransferGhostCoordsSend[i].size();
    }
    size_sendGhostMsgSG = nCoordsSend;
    size_sendGhostMsgX  = nCoordsSend;
    size_sendGhostMsgY  = nCoordsSend;
}

void Schedule::transferGhostsToFortran(
    int &nGhostMsgsRecv,
    int *ghostMsgRecvFrom,
    int *recvGhostMsgStart,
    int *recvGhostMsgSG,
    int *recvGhostMsgX,
    int *recvGhostMsgY,
    int &nGhostMsgsSend,
    int *ghostMsgSendTo,
    int *sendGhostMsgStart,
    int *sendGhostMsgSG,
    int *sendGhostMsgX,
    int *sendGhostMsgY)
{
    // Transfer receiving messages
    nGhostMsgsRecv = mnGhostMsgsRecv;
    int offset = 0;
    for(int i = 0; i < mnGhostMsgsRecv; i++) {
        ghostMsgRecvFrom[i] = mGhostMsgRecvFrom[i];
        recvGhostMsgStart[i] = offset + 1;

        // Store coordinates
        for(int j = 0; j < mTransferGhostCoordsRecv[i].size(); j++) {
            recvGhostMsgSG[offset] = mTransferGhostCoordsRecv[i][j].sg->sgid();
            recvGhostMsgX[offset] = mTransferGhostCoordsRecv[i][j].x;
            recvGhostMsgY[offset] = mTransferGhostCoordsRecv[i][j].y;

            offset++;
        }
    }
    recvGhostMsgStart[mnGhostMsgsRecv] = offset + 1;
    
    // Transfer sending messages
    nGhostMsgsSend = mnGhostMsgsSend;
    offset = 0;
    for(int i = 0; i < mnGhostMsgsSend; i++) {
        ghostMsgSendTo[i] = mGhostMsgSendTo[i];
        sendGhostMsgStart[i] = offset + 1;

        // Store coordinates
        for(int j = 0; j < mTransferGhostCoordsSend[i].size(); j++) {
            sendGhostMsgSG[offset] = mTransferGhostCoordsSend[i][j].sg->sgid();
            sendGhostMsgX[offset] = mTransferGhostCoordsSend[i][j].x;
            sendGhostMsgY[offset] = mTransferGhostCoordsSend[i][j].y;

            offset++;
        }
    }
    sendGhostMsgStart[mnGhostMsgsSend] = offset + 1;
}

void Schedule::registerTransfer(int srcGbid, const Region &srcReg,
                                int tgtGbid, const Region &tgtReg)
{
    //cout << "REGISTER TRANSFER " << srcReg << " in " << srcGbid << " -> "
    //     << tgtReg << " in " << tgtGbid << endl;

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

    // If either pid is -1 don't bother with the transfer
    if(pidRecv == -1 || pidSend == -1) { return; }

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


set<GlobalCoordinate> Schedule::breadthFirstExpansionFromBlock(
    int gbid, int d) const
{
    Subgrid *sg = mDist->gbidSubgrid(gbid);
    int blkXOffset = mDist->gbidRegion(gbid).lowX()-1;
    int blkYOffset = mDist->gbidRegion(gbid).lowY()-1;

    set<GlobalCoordinate> visited, result;
    set<GlobalCoordinate> *prevFrontier, *nextFrontier, *tmp;
    prevFrontier = new set<GlobalCoordinate>();
    nextFrontier = new set<GlobalCoordinate>();

    //cout << "------------[ GBID: " << gbid << " (";
    //cout << sg->getID() << ") ]----------------" << endl;

    int blkW = MIN(mDist->blockWidth(), sg->w());
    int blkH = MIN(mDist->blockHeight(), sg->h());
    // Set a ring of interior block as visited
    if(sg->w() > 1 && sg->h() > 1) {
        for(int i = 2; i <= mDist->blockWidth() - 1; i++) {
            visited.insert(GlobalCoordinate(
                sg, i+blkXOffset, 2+blkYOffset));
            visited.insert(GlobalCoordinate(
                sg, i+blkXOffset, mDist->blockHeight()-1+blkYOffset));
        }
        for(int j = 2; j <= mDist->blockHeight() - 1; j++) {
            visited.insert(GlobalCoordinate(
                sg, 2+blkXOffset, j+blkYOffset));
            visited.insert(GlobalCoordinate(
                sg, mDist->blockWidth()-1+blkXOffset, j+blkYOffset));
        }
    }

    // Set frontier to the ring of nodes bordering the block
    //cout << "offsets: " << blkW << " " << blkH << " "
    //     << blkXOffset << " " << blkYOffset << endl;
    for(int i = 1; i <= blkW; i++) {
        prevFrontier->insert(GlobalCoordinate(sg, i+blkXOffset,    1+blkYOffset));
        prevFrontier->insert(GlobalCoordinate(sg, i+blkXOffset, blkH+blkYOffset));
        visited.insert(GlobalCoordinate(sg, i+blkXOffset,    1+blkYOffset));
        visited.insert(GlobalCoordinate(sg, i+blkXOffset, blkH+blkYOffset));
    }
    for(int j = 1; j <= blkH; j++) {
        prevFrontier->insert(GlobalCoordinate(sg,    1+blkXOffset, j+blkYOffset));
        prevFrontier->insert(GlobalCoordinate(sg, blkW+blkXOffset, j+blkYOffset));
        visited.insert(GlobalCoordinate(sg,    1+blkXOffset, j+blkYOffset));
        visited.insert(GlobalCoordinate(sg, blkW+blkXOffset, j+blkYOffset));
    }

    //cout << "Visited  = "; printSet(cout, visited); cout << endl;
    //cout << "Frontier = "; printSet(cout, *prevFrontier); cout << endl;
    //cout << endl << endl;

    // Iterate for each layer of depth
    for(int idxLayer = 1; idxLayer <= d; idxLayer++) {
        // Visit each node on the current frontier
        for(set<GlobalCoordinate>::iterator n = prevFrontier->begin();
            n != prevFrontier->end(); n++)
        {
            // See if we can expand to each neighbor
            for(std::map<std::string, Neighbor*>::const_iterator neighIter =
                Environment::neighborsBegin();
                neighIter != Environment::neighborsEnd();
                neighIter++)
            {
                Neighbor *neigh = neighIter->second;

                // Grab the global coordinate visited from the neighbor
                GlobalCoordinate neighCoord(
                    n->sg, n->x + neigh->x(), n->y + neigh->y());

                // Use border map to translate if needed
                neighCoord = mGrid->resolveBMap(neighCoord);
                
                // Ignore this neighbor if we've already visited it or if
                // it falls into an unfilled halo location
                if(visited.find(neighCoord) != visited.end()) { continue; }
                if(nextFrontier->find(neighCoord) != nextFrontier->end()) { continue; }
                if(prevFrontier->find(neighCoord) != prevFrontier->end()) { continue; }
                if(neighCoord.x <= 0 || neighCoord.y <= 0 ||
                   neighCoord.x > neighCoord.sg->w() ||
                   neighCoord.y > neighCoord.sg->h()) { continue; }

                visited.insert(neighCoord);
                result.insert(neighCoord);
                nextFrontier->insert(neighCoord);
            }
        }

        // Swap frontiers for next iteration
        tmp = prevFrontier;
        prevFrontier = nextFrontier;
        nextFrontier = tmp;
        nextFrontier->clear();
    }

    //printSet(cout, result);
    //cout << endl;

    // Clean up allocate data
    delete prevFrontier;
    delete nextFrontier;

    return result;
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
