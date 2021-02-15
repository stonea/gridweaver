/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#ifndef SCHEDULE_HPP_
#define SCHEDULE_HPP_

#include <string>
#include <vector>
#include "grid.hpp"
#include "distribution.hpp"
#include "iprintable.hpp"
#include "iserializable.hpp"
#include "ienvironmental.hpp"

/**
 * Initialization function for the schedule module.
 */
void initializeModule_schedule();


/**
 * A schedule is a collection of metadata used to drive communication for halo
 * populating the halos of a data object's blocks.
 **/
class Schedule : public IPrintable, public ISerializable, public IEnvironmental
{
  public:
    friend class Environment;   // Must be a friend so that environment may
                                // construct objects of this type. 
    
    // =======================
    // - [Construction] -
    // =======================
    /** @name Construction */
    ///@{
        /**
         * Calculate a valid schedule for populating a halo of depth 1 for
         * grid g using distribution dist.  Store the resulting schedule in
         * this object.
         */
        void calculate(Grid *g, Distribution *dist, int depth);

        void calculateGhostNodePlan(Grid *g, Distribution *dist, int depth);
    ///@}

    // =======================
    // - [Input and Output] -
    // =======================
    /** @name Input and Output */
    ///@{
        void print(std::ostream &out) const;
        void printSimp(std::ostream &out) const;
        virtual void output(std::ostream &out) const;
        virtual void input(std::istream &in);
    ///@}
    
    // =======================
    // - [Accessors] -
    // =======================
    /** @name Accessors */
    ///@{
        std::string getID() const { return mName; }
        
        /**
         * Return the grid this schedule is applied to. 
         */
        Grid* grid() const { return mGrid; }

        /**
         * Return the distribution this schedule is applied to. 
         */
        Distribution* distribution() const { return mDist; }
   //@}

    // ==========================
    // - [Transfer to Fortran] -
    // ==========================
    /**
     * Return the sizes of data-structures used to store the schedule in
     * Fortran.
     */
    void transferSizesToFortran(
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
 
    /**
     * the following function transforms this schedule object into
     * Fortran data structures.  This is used to pass data between the C++
     * and fortran libraries.
     */
    void transferToFortran(
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

    void transferGhostSizesToFortran(
        int &size_ghostMsgRecvFrom,
        int &size_recvGhostMsgStart,
        int &size_recvGhostMsgSG,
        int &size_recvGhostMsgX,
        int &size_recvGhostMsgY,
        int &size_ghostMsgSendTo,
        int &size_sendGhostMsgStart,
        int &size_sendGhostMsgSG,
        int &size_sendGhostMsgX,
        int &size_sendGhostMsgY);

    void transferGhostsToFortran(
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
        int *sendGhostMsgY);

  private:
    // =======================
    // - [Construction] -
    // =======================
        Schedule(const std::string &name);

        /**
         * Register a data-transfer in the schedule that srcRef in srcBlk should
         * be copied into tgtReg of the halo of tgtBlk.
         */
        void registerTransfer(int srcGbid, const Region &srcReg,
                              int tgtGbid, const Region &tgtReg);

        /**
         * Return set of global coordinates reachable from gbid within d steps
         * of a breadth-first search.  Do not expand into the block.
         */
        std::set<GlobalCoordinate> breadthFirstExpansionFromBlock(
            int gbid, int d) const;

    // =======================
    // - [Input and Output] -
    // =======================
        /**
         * Given an entry in mMsgRecvFrom or mMsgSendTo serialize it to the
         * stream.
         **/
        static void saveIntVector(std::ostream &out,
                                  const std::vector<int> &obj);

        /**
         * Given an entry in mMsgRecvFrom or mMsgSendTo load it from a 
         * serialized stream.
         **/
        static void loadIntVector(std::istream &in,
                                  std::vector<int> &obj);

        /**
         * Given an entry in mTransferRecvAtLBID or mTransferSendFromLBID
         * serialize it to the stream.
         **/
        static void saveIntVectorVector(std::ostream &out,
                                        const std::vector<std::vector<int> >
                                            &obj);

        /**
         * Given an entry in mTransferRecvAtLBID or mTransferSendFromLBID
         * load it from a serialized stream.
         **/
        static void loadIntVectorVector(std::istream &in,
                                        std::vector<std::vector<int> > &obj);
 
        /**
         * Save a vector of regions.
         **/
        static void saveRegionVector(std::ostream &out,
                                     const std::vector<Region> &obj);

        /**
         * Given an entry in mTransferRegionRecv or mTransferRegionSend
         * serialize it to the stream.
         **/
        static void saveRegionVectorVector(
            std::ostream &out, const std::vector<std::vector<Region> > &obj);

    std::string mName;
    Grid  *mGrid;
    Distribution  *mDist;

    int  mnMsgsRecv;
    std::vector<int>  mMsgRecvFrom;
    std::vector<std::vector<int> >  mTransferRecvAtLBID;    // msgID (x) tid -> lbid
    std::vector<std::vector<Region> >  mTransferRegionRecv; // msgID (x) tid -> rgn
    
    int  mnMsgsSend;
    std::vector<int>  mMsgSendTo;
    std::vector<std::vector<int> >  mTransferSendFromLBID;
    std::vector<std::vector<Region> >  mTransferRegionSend;


    int  mnGhostMsgsRecv;
    std::vector<int>  mGhostMsgRecvFrom;
    std::vector<std::vector<GlobalCoordinate> >  // msgID -> coordinates
        mTransferGhostCoordsRecv; 

    int  mnGhostMsgsSend;
    std::vector<int>  mGhostMsgSendTo;
    std::vector<std::vector<GlobalCoordinate> >  // msgID -> coordinates
        mTransferGhostCoordsSend;
};

#endif
